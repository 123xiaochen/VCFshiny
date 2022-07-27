# 1-4、SNP各个变异类型统计柱状图 -------------------------------------------------------------------------------------------------------------------------
# 1-4.1 、给数据加一列type ，构建SNP各个变异类型统计数据
SNP_type_data <- reactive({
  SNP_ACTG_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "A" & SNP_heatmap_Data()$ALT == "C")| (SNP_heatmap_Data()$REF == "T" & SNP_heatmap_Data()$ALT == "G"),]
  SNP_ACTG_data$type <- "A>C/T>G"
  SNP_AGTC_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "A" & SNP_heatmap_Data()$ALT == "G")| (SNP_heatmap_Data()$REF == "T" & SNP_heatmap_Data()$ALT == "C"),]
  SNP_AGTC_data$type <- "A>G/T>C"
  SNP_ATTA_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "A" & SNP_heatmap_Data()$ALT == "T")| (SNP_heatmap_Data()$REF == "T" & SNP_heatmap_Data()$ALT == "A"),]
  SNP_ATTA_data$type <- "A>T/T>A"
  SNP_CAGT_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "C" & SNP_heatmap_Data()$ALT == "A")| (SNP_heatmap_Data()$REF == "G" & SNP_heatmap_Data()$ALT == "T"),]
  SNP_CAGT_data$type <- "C>A/G>T"
  SNP_CGGC_Data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "C" & SNP_heatmap_Data()$ALT == "G")| (SNP_heatmap_Data()$REF == "G" & SNP_heatmap_Data()$ALT == "C"),]
  SNP_CGGC_Data$type <- "C>G/G>C"
  SNP_CTGA_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "C" & SNP_heatmap_Data()$ALT == "T")| (SNP_heatmap_Data()$REF == "G" & SNP_heatmap_Data()$ALT == "A"),]
  SNP_CTGA_data$type <- "C>T/G>A"
  SNP_type_data<- rbind(SNP_ACTG_data,SNP_AGTC_data,SNP_ATTA_data,SNP_CAGT_data,SNP_CGGC_Data,SNP_CTGA_data)

  SNP_type_data <- SNP_type_data %>% group_by(type,group,samples) %>% summarise_if(is.numeric,sum)

  return(SNP_type_data)
})


observeEvent(input$ALL_Variantsplot,{
  output$SNP_ALL_variants_data <- DT::renderDataTable(
    return(SNP_type_data()),extensions ="Buttons",
    server =  as.logical(input$ALL_Variants_data_Set),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download'
      )
      ))
  )
})
# 1-4.2、 绘制SNP各个变异类型统计柱状图
All_Variants_type_plot <- eventReactive(input$ALL_Variantsplot, {
  req(input$ALL_Variants_plot_Text_size, input$ALL_Variants_plot_bar.width, input$ALL_Variants_plot_Error_bar.width, input$ALL_Variants_plot_Error_bar.size, input$ALL_Variants_plot_label)
  SNP_type_data <- SNP_type_data()
  stat.test1 <- SNP_type_data %>% group_by(type) %>% rstatix::pairwise_t_test(freq~ group) %>% rstatix::add_significance()
  stat.test1 <- stat.test1 %>% rstatix::add_xy_position(x = "type")
  stat.test1 <- stat.test1[stat.test1$group1 == "WT",]

  p <- ggpubr::ggbarplot(SNP_type_data,x = "type",y = "freq", palette = "Set1", label = as.logical(input$ALL_Variants_plot_label), lab.nb.digits = input$ALL_Variants_plot_label_digits,
                    add = "mean_sd",fill = "group",width = input$ALL_Variants_plot_bar.width, position = position_dodge(),
                    add.params = list(width = input$ALL_Variants_plot_Error_bar.width, size = input$ALL_Variants_plot_Error_bar.size), ggtheme = theme_classic())+
    stat_pvalue_manual(stat.test1, bracket.size = 0.5,tip.length = 0.01,bracket.nudge.y = 0)+
    xlab(NULL)+ylab("Freq of different types of SNVs versus total SNVs")+
    theme(text = element_text(face = "bold",family = "Times",size = input$ALL_Variants_plot_Text_size,color = "black"),
          strip.background = element_blank())

  if (!is.null(input$ALL_Variants_plot_ggText)) {
    add_funcs <- strsplit(input$ALL_Variants_plot_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})

output$SNP_type_plot <- renderPlot({
  All_Variants_type_plot()
})

output$ALL_Variants_plot <- downloadHandler(
  filename = "Heatmap_plot.pdf",
  content = function(file){
    pdf(file, width = input$ALL_Variants_download_width, height = input$ALL_Variants_download_height)
    print(All_Variants_type_plot())
    dev.off()
  }
)
