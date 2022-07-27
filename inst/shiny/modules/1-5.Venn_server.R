#1-5、韦恩图（为做圈图取重复样本的交叉数据）---------------------------------------------------------------------------------------------------------------

#1-5.1、添加行名
venn_ALL_data <- reactive({
  ALL_variants_vcf <- ALL_variants_vcf()
  venn_ALL_data <- lapply(names(ALL_variants_vcf),function(x){
    if("POS" %in% (names(ALL_variants_vcf[[x]]))){        #判断是哪种vcf文件
      rownames(ALL_variants_vcf[[x]]) <- paste(ALL_variants_vcf[[x]]$CHROM,ALL_variants_vcf[[x]]$POS,ALL_variants_vcf[[x]]$REF,ALL_variants_vcf[[x]]$ALT,sep = "_")
      ALL_variants_vcf[[x]]
    }else if("Start" %in% (names(ALL_variants_vcf[[x]])) & "End" %in% (names(ALL_variants_vcf[[x]]))){
      rownames(ALL_variants_vcf[[x]]) <-
        paste(ALL_variants_vcf[[x]]$Chr, ALL_variants_vcf[[x]]$Start, ALL_variants_vcf[[x]]$End, ALL_variants_vcf[[x]]$REF, ALL_variants_vcf[[x]]$ALT,sep = "_")
      ALL_variants_vcf[[x]]
    }
  })
  names(venn_ALL_data) <- names(ALL_variants_vcf)
  #print("添加行名OK")
  return(venn_ALL_data)
})


#1-5.2 、根据输入筛选数据并作Venn图

output$venn_group_id <- renderUI({
  if(!is.null(ALL_variants_vcf())){
    group <- stringr::str_remove(names(ALL_variants_vcf()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
    selectInput("venn_group_ID","Select your group to analyse:", choices = group)
  }
})

vennPlot_data <- eventReactive(input$venn_star,{
  req(input$venn_group_ID, input$venn_type_id, input$venn_box, input$venn_ellipse, input$venn_ilcs, input$venn_sncs)

  venn_ALL_data <- venn_ALL_data()
  samples <- stringr::str_subset(names(venn_ALL_data),pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
  venn_list <- lapply(samples, function(x){
    venn_ALL_data[[x]] %>% row.names()
  })
  names(venn_list) <- samples
  return(venn_list)
})

vennPlot <- eventReactive(input$venn_star,{
  req(input$venn_box, input$venn_ellipse, input$venn_ilcs, input$venn_sncs)
  venn::venn(vennPlot_data(), zcolor = "style", box = as.logical(input$venn_box),
             ellipse = as.logical(input$venn_ellipse),ilcs = (input$venn_ilcs), sncs = (input$venn_sncs)
  )
})


output$Display_venn_plot <- renderPlot({
  vennPlot()
})

output$Venn_download_plot <- downloadHandler(
  req(input$venn_type_id, input$venn_group_ID, input$VennPlot_download_width, input$VennPlot_download_height),
  filename = function(){
    paste("Venn_plot", input$venn_type_id, input$venn_group_ID, "pdf", sep = ".")
  },
  content = function(file){
    pdf(file, width = input$VennPlot_download_width, height = input$VennPlot_download_height)
    req(input$venn_box, input$venn_ellipse, input$venn_ilcs, input$venn_sncs)
    venn::venn(vennPlot_data(), zcolor = "style", box = as.logical(input$venn_box),
               ellipse = as.logical(input$venn_ellipse),ilcs = (input$venn_ilcs), sncs = (input$venn_sncs))
    dev.off()
  }
)

#1-5.3 、提取对应的交叉数据并展示
venn_table <- reactive({
  req(input$venn_group_ID, input$venn_type_id)
  samples <- stringr::str_subset(names(venn_ALL_data()),pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)

  venn_binded <- lapply(samples, function(x){
    df <- ALL_variants_vcf()[[x]]
    return(df)
  }) %>% bind_rows()


  if("POS" %in% (names(venn_binded))){
    venn_binded$Merge_Name <- paste(venn_binded$CHROM, venn_binded$POS, venn_binded$REF, venn_binded$ALT,sep = "_")
  }else if("Start" %in% (names(venn_binded)) & "End" %in% (names(venn_binded))){
    venn_binded$Merge_Name <- paste(venn_binded$Chr, venn_binded$Start, venn_binded$End, venn_binded$REF, venn_binded$ALT,sep = "_")
  }
  duplicated_row <- (venn_binded$Merge_Name %>% table() %>% as.data.frame() %>% dplyr::filter(Freq == max(Freq)))[,1]
  duplicated_row <- as.character(duplicated_row)
  venn_binded <- venn_binded[(venn_binded$Merge_Name %in% duplicated_row) , ] %>% unique()
  venn_binded <- subset(venn_binded,select = -c(Merge_Name))
  return(venn_binded)
})

observeEvent(input$venn_star, {
  output$venn_Table <- DT::renderDataTable(
    return(venn_table()) , extensions ="Buttons",
    server =  as.logical(input$Venn_data_Set),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download'))
    )
  )
})


#1-5.4 、 提取所有交叉数据（圈图准备数据）

circle_table <- reactive({
  req(input$circle_type_ID)
  group_table <- lapply((stringr::str_remove(names(venn_ALL_data()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()),function(x){
    sample <- stringr::str_subset(names(venn_ALL_data()),pattern = x, negate = F) %>% stringr::str_subset(pattern = input$circle_type_ID, negate = F)
    circle_data <- lapply(sample,function(y){
      df <- ALL_variants_vcf()[[y]]
    }) %>% bind_rows()

    if("POS" %in% (names(circle_data))){
      circle_data$Merge_Name <- paste(circle_data$CHROM, circle_data$POS, circle_data$REF, circle_data$ALT,sep = "_")
    }else if("Start" %in% (names(circle_data)) & "End" %in% (names(circle_data))){
      circle_data$Merge_Name <- paste(circle_data$Chr, circle_data$Start, circle_data$End, circle_data$REF, circle_data$ALT,sep = "_")
    }
    duplicated_row <- (circle_data$Merge_Name %>% table() %>% as.data.frame() %>% dplyr::filter(Freq == max(Freq)))[,1]
    duplicated_row <- as.character(duplicated_row)
    circle_data <- circle_data[(circle_data$Merge_Name %in% duplicated_row) , ] %>% unique()
    circle_data <- subset(circle_data,select = -c(Merge_Name))
    return(circle_data)
  })
  names(group_table) <- stringr::str_remove(names(venn_ALL_data()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
  #print("提取所有交叉数据OK")
  return(group_table)
})

# 输出所有交叉数据
# output$circle_Table <- DT::renderDataTable({
#   circle_table()[[1]]
# }, options = list(scrollX = TRUE))
