#1、SNP+Indel突变柱状图 --------------------------------------------------------------------------------------------------------------------------

#1.1 、 对输入数据进行数据总结并显示
ALL_data <- reactive({
  ALL_Variants_summary <- data.frame()
  for(i in 1:length(ALL_variants_vcf())){
    SNV_numbers = dim(ALL_variants_vcf()[[i]])[1]
    samples = strsplit(names(ALL_variants_vcf())[i], ".",fixed = T)[[1]][1]
    group =  gsub("-[0-9]$|[0-9]$","",samples)
    type = strsplit(names(ALL_variants_vcf())[i], ".",fixed = T)[[1]][2]
    ALL_Variants_summary <- rbind(ALL_Variants_summary,data.frame(group,samples,type,SNV_numbers))
  }
  return(ALL_Variants_summary)
})

observeEvent(input$Data_summarise_plot,{
  output$information <- DT::renderDataTable(
    return(ALL_data()),extensions ="Buttons",
    server =  as.logical(input$ALL_SNP_INDEL_data_Set),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download'
      ))
    )
  )
})

#1.2 、 构建选择参考组控件
output$Summariseplot_stat.test.group <- renderUI({
  if(!is.null(ALL_variants_vcf())){
    stat.test.group <- unique(ALL_data()$group)
    selectInput("Summariseplot_stat.test.GROUP","Select your reference group:" , choices = stat.test.group, width = "100%")
  }
})


#1.3 、 绘制SNP+Indel突变柱状图
Data_SummarisePlot <- eventReactive(input$Data_summarise_plot,{
  req(input$Summariseplot_stat.test.GROUP,input$text_size,input$bar_width,input$error_bar_width,input$error_bar_size,
      input$brack.size,input$tip.length,input$jitter_size,input$jitter_width, input$Summariseplot_label)

  stat.test <- ALL_data() %>% group_by(type) %>% rstatix::pairwise_t_test(SNV_numbers ~ group) %>% rstatix::add_significance()
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "group")
  stat.test <- stat.test[stat.test$group1 == input$Summariseplot_stat.test.GROUP, ]
  #选择WT为对照

  p <- ggpubr::ggbarplot(ALL_data(),x = "group",y =  "SNV_numbers" , facet.by = "type" , palette = "Set1",label = as.logical(input$Summariseplot_label),
                    fill = "group" , add = "mean_sd" ,  width = input$bar_width , position = position_dodge(),
                    add.params = list(width = input$error_bar_width ,size = input$error_bar_size), ggtheme = theme_classic())+
    ggpubr::stat_pvalue_manual(stat.test, bracket.size = input$brack.size, tip.length = input$tip.length, bracket.nudge.y = 0)+
    scale_alpha_continuous(expand = expansion(mult = c(0.1, 0.1)))+
    facet_wrap( ~ type, ncol = 4, scales = "free")+
    xlab(NULL)+ylab("WGS Total SNV numbers")+
    theme(text = element_text(face = "bold", family = "Times", size = input$text_size, color = "black"), legend.position = "none",
          strip.background = element_blank())+
    geom_jitter(size = input$jitter_size,width = input$jitter_width)

  if (!is.null(input$summ_ggText)) {
    add_funcs <- strsplit(input$summ_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }

  return(p)
})

output$SummarisePlot <- renderPlot({
  Data_SummarisePlot()
})

output$summarise_plot <- downloadHandler(
  filename = function(){
    paste("Summarise_plot",input$summarise_down_filetype,sep = ".")
  },
  content = function(file){
    if(input$summarise_down_filetype == "pdf"){
      pdf(file,width = input$summarise_download_width,height = input$summarise_download_height)
    }else if(input$summarise_down_filetype == "png"){
      png(file,width = input$summarise_download_width,height = input$summarise_download_height)
    }else{
      jpeg(file,width = input$summarise_download_width,height = input$summarise_download_height)
    }
    print(Data_SummarisePlot())
    dev.off()
  }
)
