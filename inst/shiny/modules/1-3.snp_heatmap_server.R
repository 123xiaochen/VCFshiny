#1-3、SNP 全突变热图 -------------------------------------------------------------------------------------------------------------------------
#1-3.1、构建突变热图作图表格并构建下载控件
SNP_heatmap_Data <-reactive({
  SNP_heatmap_data <- data.frame()
  for(x in 1:length(SNP_data())){
    seq = c("A","T","C","G")
    for(y in seq){
      for(z in seq){
        SNP_numbers <- sum(SNP_data()[[x]]$REF == y & SNP_data()[[x]]$ALT == z)
        REF <- y
        ALT <- z
        samples <- strsplit(names(SNP_data())[x], ".",fixed = T)[[1]][1]
        group <- gsub("-[0-9]$|_[0-9]$","",samples)
        freq <- SNP_numbers / dim(SNP_data()[[x]])[1]
        SNP_heatmap_data <- rbind(SNP_heatmap_data,data.frame(samples,group,REF,ALT,SNP_numbers,freq))
      }
    }
  }
  return(SNP_heatmap_data)
})


observeEvent(input$Heatplot,{
  output$snp_heatmap_data <- DT::renderDataTable(
    return(SNP_heatmap_Data()),extensions ="Buttons",
    server =  as.logical(input$Heatmap_data_Set),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download'
      )
      )
    ))
})

#1-3.2 、绘制热图并构建下载图片控制下载图片大小控件

Heatmapplot <- eventReactive(input$Heatplot, {
  req(input$heatmap_digits,input$heatmap_ncol,input$heatmap_text.size, input$heatmap_color)
  SNP_data <- SNP_heatmap_Data() %>% group_by(group,REF,ALT) %>% summarise_if(is.numeric,mean)
  SNP_data$freq <- SNP_data$freq*100

  ggplot2::ggplot(SNP_data,aes(ALT,REF,fill = freq))+
    geom_tile()+
    geom_text(aes(label = round(freq, digits = input$heatmap_digits)))+
    facet_wrap(~group,ncol = input$heatmap_ncol)+
    scale_fill_gradient(low = "white" , high = input$heatmap_color)+
    theme_test()+
    theme(text = element_text(face = "bold", family = "Times", size = input$heatmap_text.size, color = "black"))
})

output$Heat_Plot <- renderPlot({
  Heatmapplot()
})

output$heatmap_plot <- downloadHandler(
  req(input$heatmap_download_width,input$heatmap_download_height),
  filename = "Heatmap_plot.pdf",
  content = function(file){
    pdf(file, width = input$heatmap_download_width, height = input$heatmap_download_height)
    print(Heatmapplot())
    dev.off()
  }
)
