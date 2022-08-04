#2-1、构建位置分布图--------------------------------------------------------------------------------------------------------------------------------------
#2-1.1 构建位置分布数据
distribution_binded <- reactive({
  req(input$distribution_type_id)
  distribution_bind <- data.frame()
  sample_ID <-stringr::str_subset(names(ALL_variants_vcf()),pattern = input$distribution_type_id, negate = F)
  groups_ID <- stringr::str_remove(names(ALL_variants_vcf()),"-[0-9].snp$|-[0-9].indel|_[0-9].snp$|_[0-9].indel$") %>% unique()

  for(x in sample_ID){
    data <- ALL_variants_vcf()[[x]]
    sample_name <- strsplit(x, ".",fixed = T)[[1]][1]
    group <- gsub("-[0-9]$|_[0-9]$","",sample_name)
    data <- data %>% dplyr::group_by(Func.refGene) %>% summarise(percentage = (n()/nrow(data))*100)
    distribution_bind <- rbind(distribution_bind, data.frame(group, sample_name, data))
  }
  return(distribution_bind)
})

observeEvent(input$plot_distribution, {
  output$distribution_data <- DT::renderDataTable(
    return(distribution_binded()), extensions ="Buttons",
    server =  as.logical(input$Distribution_data_Set),
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
#2-1.2 做位置分布柱状图
distribution_plot <- eventReactive(input$plot_distribution, {
  req(input$distribution_text_size, input$distribution_bar_width,input$distribution_error_bar_size, input$distribution_error_bar_width)

  distribution_binded <- distribution_binded()

  p <- ggpubr::ggbarplot(distribution_binded,x = "Func.refGene", y = "percentage", palette = "Set1",
                    add = c("mean_sd"), fill = "group", width = input$distribution_bar_width, position = position_dodge(),
                    add.params = list(width = input$distribution_error_bar_width, shape = "group", size = input$distribution_error_bar_size), ggtheme = theme_classic())+
    xlab(NULL) + ylab("Distribution Proportion of biotypes (%)")+
    theme(text = element_text(face = "bold",family = "Times",size = input$distribution_text_size,color = "black"),legend.position = "right",
          strip.background = element_blank(),axis.text.x = element_text(angle = 45,hjust = 1))

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
    paste("ALL_Variants_Distribution_plot",input$distribution_type_id,sep = ".")
  },
  content = function(file){
    pdf(file,width = input$Distribution_download_width,height = input$Distribution_download_height)
    print(distribution_plot())
    dev.off()
  }
)

