#2-3、构建各组变异基因热图数据-----------------------------------------------------------------------------------------------------------------------

#2-3.1、 构建选择位置输入框
output$Variants_position_id <- renderUI({
  ALL_variants_vcf <- ALL_variants_vcf()
  position <- unique(ALL_variants_vcf[[input$Variants_sampleID]]$Func.refGene)
  selectInput("Variants_position_ID", "Select Position To Analyse:", choices = position, multiple = T ,width = "100%")
})


Variants_heatmap_data <- reactive({
  req(input$Variants_heatmap_types, input$Variants_position_ID, input$Variants_heatmap_numbers)

  Variants_ALL_genes <- Variants_ALL_genes()
  sample_ID <- stringr::str_subset(names(Variants_ALL_genes), pattern = input$Variants_heatmap_types, negate = F)


  Variants_heatmap_df <- lapply(sample_ID, function(x){

    df <- Variants_ALL_genes[[x]]
    df <- df[df$Position == input$Variants_position_ID, ]
    df <- df[order(df$Numbers, decreasing = T), ][1:input$Variants_heatmap_numbers, ]
    name <- gsub("-", "_", x)
    colnames(df) <- c("Position", "Genes", name)
    df <- as.data.frame(df)
  }) %>% plyr::join_all(by = "Genes", type = "full")

  Variants_heatmap_df[is.na(Variants_heatmap_df)] <- 0
  return(Variants_heatmap_df)
})


observeEvent(input$Variants_heatmap, {
  output$Variants_Heatmap_data <- DT::renderDataTable(
    return(Variants_heatmap_data()), extensions ="Buttons",
    server =  as.logical(input$Variants_Genes_Heatmap_data),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download')
      )
    )
  )
})

#2-3.2 、 绘制热图
Variants_Genes_heatmap_plot <- eventReactive(input$Variants_heatmap, {
  req(input$Variants_heatmap_show_rownames, input$Variants_heatmap_show_colnames, input$Variants_heatmap_treeheight_row, input$Variants_heatmap_treeheight_col)
  Variants_heatmap_data <- as.data.frame(Variants_heatmap_data())

  Variants_heatmap_data <- Variants_heatmap_data[ , -(colnames(Variants_heatmap_data) == "Position")]
  Variants_heatmap_data <- Variants_heatmap_data %>% dplyr::group_by(Genes) %>% dplyr::summarise_each(funs(sum))

  Variants_heatmap_data <- as.data.frame(Variants_heatmap_data)

  rownames(Variants_heatmap_data) <- Variants_heatmap_data$Genes
  Variants_heatmap_data <- Variants_heatmap_data[ , -(colnames(Variants_heatmap_data) == "Genes")]
  Variants_heatmap_data <- Variants_heatmap_data[apply(Variants_heatmap_data, 1, function(x) sd(x) != 0), ]
  Variants_heatmap_data <- as.data.frame(Variants_heatmap_data)
  p <- pheatmap::pheatmap(Variants_heatmap_data, scale = "row",
                     show_rownames = as.logical(input$Variants_heatmap_show_rownames),
                     show_colnames = as.logical(input$Variants_heatmap_show_colnames),
                     treeheight_row = input$Variants_heatmap_treeheight_row,
                     treeheight_col = input$Variants_heatmap_treeheight_col)

  return(p)
  })



output$VariantsGenes_Heatmap_Plot <- renderPlot({
  Variants_Genes_heatmap_plot()
})

#2-3.3、 下载热图
output$Variants_heatmap_Download <- downloadHandler(
  req(input$Variants_sampleID, input$Variants_heatmap_types, input$Variants_heatmap_download_width, input$Variants_heatmap_download_height),
  filename = function(){
    paste("Variants_Heatmap_Summerise_plot", input$Variants_sampleID, input$Variants_heatmap_types, "pdf", sep = ".")
  },
  content = function(file){
    pdf(file, width = input$Variants_heatmap_download_width, height = input$Variants_heatmap_download_height)
    print(Variants_Genes_heatmap_plot())
    dev.off()
  }
)
