#2-1、构建位置分布图--------------------------------------------------------------------------------------------------------------------------------------
#######---------------------------------------------------------无法选组
output$distribution_type_group <- renderUI({
  virtualSelectInput(
    inputId = "distribution_type_group",  label = "Sample groups:",
    choices = unique(gsub("-[0-9].*$|_[0-9].*","",names(raw_variants_list()))),
    selected = unique(gsub("-[0-9].*$|_[0-9].*","",names(raw_variants_list()))),
    multiple = T, search = F, width = "100%"
  )
})

output$distribution_feature_column <- renderUI({
  virtualSelectInput(
    inputId = "distribution_feature_column",  label = "Select Genomic Feature Column:",
    choices = colnames(raw_variants_list()[[1]]),
    selected = colnames(raw_variants_list()[[1]])[6], multiple = F, search = F, width = "100%"
  )
})


#2-1.1 构建位置分布数据
distribution_binded <- eventReactive(input$plot_distribution, {
  distribution_bind <- data.frame()
  sample_ID <- stringr::str_subset(names(raw_variants_list()), pattern = input$distribution_type_id, negate = F)
  sample_ID <- lapply(input$distribution_type_group, function(x){
    stringr::str_subset(sample_ID, pattern = x, negate = F)
  }) %>% unlist

  distribution_bind <- lapply(sample_ID, function(x){
    data <- raw_variants_list()[[x]]
    colnames(data)[colnames(data) == input$distribution_feature_column] <- "feature_column"
    number_df <- data %>% dplyr::group_by(feature_column) %>% count %>% as.data.frame()
    number_df$sample_name <- x
    number_df$group <- gsub("-[0-9].*|_[0-9].*", "", x)
    number_df$percentage <- number_df$n / sum(number_df$n) * 100
    return(number_df)
  }) %>% bind_rows()
  return(distribution_bind)
})


output$distribution_data <- DT::renderDataTable(
  return(distribution_binded()),
  options = list(
    scrollX = TRUE,
    pageLength = 5
  )
)

output$distribution_tab_download <- downloadHandler(
  filename = function()  {paste0("distribution_tab",".csv")},
  content = function(file) {
    write.csv(distribution_binded(), file, row.names = F)
  }
)

#2-1.2 做位置分布柱状图
distribution_plot <- eventReactive(input$plot_distribution, {
  distribution_binded <- distribution_binded()
 if(input$distribution_type_position == "dodge"){
   p <- ggpubr::ggbarplot(distribution_binded, x = "feature_column", y = "percentage", palette = "Paired",
                          add = c("mean_sd"), fill = "group", width = input$distribution_bar_width, position = position_dodge(),
                          add.params = list(width = input$distribution_error_bar_width, shape = "group", size = input$distribution_error_bar_size), ggtheme = theme_classic())+
     xlab(NULL) + ylab("Proportion of variants distribution \n in the genomic features (%)")+
     theme(legend.position = "right",
           legend.title = element_text(color = "black", size = input$distribution_label_text_size),
           legend.text = element_text(color = "black", size = input$distribution_label_text_size),
           axis.text.x = element_text(angle = 45, hjust = 1),
           axis.text = element_text(color = "black", size = input$distribution_label_text_size),
           axis.title = element_text(color = "black", size = input$distribution_title_text_size),
           text = element_text(face = "bold", family = "Times", color = "black"))

 }else{
   p <- ggplot2::ggplot(distribution_binded, aes(x = group, y = percentage, fill = feature_column))+
     geom_bar(stat = "summary", color = NA, position = "stack")+
     scale_fill_brewer(palette = "Paired")+
     xlab(NULL) + ylab("Proportion of variants distribution \n in the genomic features (%)")+
     theme_classic()+
     theme(legend.position = "right",
           legend.title = element_text(color = "black", size = input$distribution_label_text_size),
           legend.text = element_text(color = "black", size = input$distribution_label_text_size),
           axis.text.x = element_text(angle = 45, hjust = 1),
           axis.text = element_text(color = "black", size = input$distribution_label_text_size),
           axis.title = element_text(color = "black", size = input$distribution_title_text_size),
           text = element_text(face = "bold", family = "Times", color = "black"))
 }

  if (!is.null(input$distribution_ggText)) {
    add_funcs <- strsplit(input$distribution_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }

  return(p)
})

output$Distribution_Plot <- renderPlot({
  distribution_plot()
})

#2-1.3 下载位置分布柱状图
output$Distribution_Download <- downloadHandler(
  filename = function(){
    paste(paste("2.1", input$distribution_type_id,"ALL_Variants_Distribution_plot", sep = "_"), "pdf",sep = ".")
  },
  content = function(file){
    pdf(file,width = input$Distribution_download_width,height = input$Distribution_download_height)
    print(distribution_plot())
    dev.off()
  }
)

