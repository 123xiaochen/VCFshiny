#1-5、韦恩图（为做圈图取重复样本的交叉数据）---------------------------------------------------------------------------------------------------------------

#1-5.1、添加行名

venn_data_list <- reactive({
  raw_variants_list <- raw_variants_list()
  venn_data_list <- lapply(raw_variants_list,function(x){
    raw_variants <- x
    raw_variants$name <- paste(raw_variants[, 1], raw_variants[, 2], raw_variants[, 3], raw_variants[, 4],
                               raw_variants[, 5],  sep = "_")
    raw_variants
  })
  return(venn_data_list)
})


#1-5.2 、根据输入筛选数据并作Venn图
output$venn_group_id <- renderUI({
  if(!is.null(raw_variants_list())){
    group <- names(venn_data_list()) %>% stringr::str_replace_all(pattern = "-[0-9].snp|_[0-9].snp|.snp|-[0-9].indel|_[0-9].indel|.indel", replacement = "") %>% unique()
    virtualSelectInput("venn_group_ID","Sample groups:", choices = group, selected = group[1], multiple = F,
                       zIndex = 4,search = F, width = "100%")
  }
})

vennPlot_data <- eventReactive(input$venn_star, {
  withProgress(message = "Analyse", min = 0, max = 1, {
  venn_data_list <- venn_data_list()
  incProgress(0.4, detail = "Select Data ...")
  samples <- stringr::str_subset(names(venn_data_list), pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
  venn_list <- lapply(samples, function(x){
    venn_data_list[[x]][,"name"] %>% unique()
  })
  names(venn_list) <- samples
  return(venn_list)
  })
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
    paste(paste("1",input$venn_type_id, input$venn_group_ID, "Venn_plot", sep = "_"), "pdf", sep = ".")
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
venn_table <- eventReactive(input$venn_star, {
  samples <- stringr::str_subset(names(venn_data_list()),pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
  venn_binded <- lapply(samples, function(x){
      df <- venn_data_list()[[x]]
  }) %>% bind_rows()

  duplicated_row <- (venn_binded$name %>% table() %>% as.data.frame() %>% dplyr::filter(Freq == max(Freq)))[,1]
  duplicated_row <- as.character(duplicated_row)
  venn_binded <- venn_binded[(venn_binded$name %in% duplicated_row) , ] %>% unique()
  venn_binded <- subset(venn_binded, select = -c(name))
  return(venn_binded)
})

output$venn_Table <- DT::renderDataTable(
  return(venn_table()),
  options = list(
    scrollX = TRUE,
    pageLength = 5
  )
)


output$venn_tab_download <- downloadHandler(
  filename = function()  {paste0("1_venn_tab",".csv")},
  content = function(file) {
    write.csv(venn_table(), file, row.names = F)
  }
)
