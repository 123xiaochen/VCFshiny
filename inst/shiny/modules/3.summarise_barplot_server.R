#1-1、SNP+Indel突变柱状图 --------------------------------------------------------------------------------------------------------------------------

#1-1.1 、 对输入数据进行数据总结并显示

sumData <- eventReactive(input$summarise_plot,{
  req(raw_variants_list())
  summary_df <- data.frame()
  for(i in 1:length(raw_variants_list())){
    variant_numbers = dim(raw_variants_list()[[i]])[1]
    samples = strsplit(names(raw_variants_list())[i], ".", fixed = T)[[1]][1]
    group =  gsub("-[0-9]$|_[0-9]$","",samples)
    type = strsplit(names(raw_variants_list())[i], ".", fixed = T)[[1]][length(strsplit(names(raw_variants_list())[i], ".",fixed = T)[[1]])]
    summary_df <- rbind(summary_df, data.frame(group, samples, type, variant_numbers))
  }
  return(summary_df)
})

output$information <- DT::renderDataTable(
  return(sumData()),
  options = list(
    scrollX = TRUE,
    pageLength = 5
  )
)

output$summary_tab <- downloadHandler(
  filename = function()  {paste0("2_summary_tab",".csv")},
  content = function(file) {
    write.csv(sumData(), file, row.names = F)
  }
)

#1-1.2 、 绘制SNP+Indel突变柱状图
SummarisePlot <- eventReactive(input$summarise_plot,{
  p <- ggpubr::ggbarplot(sumData(), x = "group",y =  "variant_numbers" , facet.by = "type" ,
                         palette = "Set1", label = as.logical(input$Summariseplot_label), lab.nb.digits = 2,
                         fill = "group" , add = "mean_sd" ,  width = input$bar_width , position = position_dodge(),
                         add.params = list(width = input$error_bar_width ,size = input$error_bar_size), ggtheme = theme_classic())+
    geom_jitter(size = input$jitter_size,width = input$jitter_width)+
    scale_alpha_continuous(expand = expansion(mult = c(0.1, 0.1)))+
    facet_wrap( ~ type, ncol = 4, scales = "free")+
    xlab(NULL)+ylab("The total number of variants")+
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = 21, color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(color = "black", size = input$label_text_size),
          axis.title = element_text(color = "black", size = input$title_text_size),
          text = element_text(face = "bold", family = "Times", color = "black"))

  if (!is.null(input$summ_ggText)) {
    add_funcs <- strsplit(input$summ_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})

output$SummarisePlot <- renderPlot({
  SummarisePlot()
})

output$summarise_plot <- downloadHandler(
  filename = "2_Summarise_plot.pdf",
  content = function(file){
    pdf(file,width = input$summarise_download_width,height = input$summarise_download_height)
    print(SummarisePlot())
    dev.off()
  }
)
