#1-2、SNP 突变分析 -------------------------------------------------------------------------------------------------------------------------
#1-2-1 SNP 热图分析 ------------------------------
#1-2-11.1、构建突变热图作图表格并构建下载控件

#SNP数据筛选
SNP_data <- reactive({
  snp_list <- lapply(stringr::str_subset(names(raw_variants_list()), pattern = ".snp"),function(x){
    SNP_ALL <- raw_variants_list()[[x]]
  })
  names(snp_list) <- stringr::str_subset(names(raw_variants_list()),pattern = ".snp")
  return(snp_list)
})

#构建选择分组控件
output$snp_analyse_group <- renderUI({
  observe(SNP_data())
  virtualSelectInput(
    inputId = "SNP_Analysis_Group",  label = "Sample groups:",
    choices = unique(gsub("-[0-9].snp$|_[0-9].snp$","",names(SNP_data()))),
    selected = unique(gsub("-[0-9].snp$|_[0-9].snp$","",names(SNP_data()))),
    multiple = T, search = F, width = "100%"
  )
})

# 构建 SNP 柱状图数据
SNP_Analysis_data <- eventReactive(input$SNP_Analysis_action, {
  req(input$SNP_Analyse_mode, input$SNP_Analysis_Group)

  #构建 SNP 热图数据
  SNP_data <- SNP_data()
  SNP_heatmap_data <- data.frame()
  for(x in 1:length(SNP_data)){
    seq = c("A","T","C","G")
    for(y in seq){
      for(z in seq){
        SNP_numbers <- sum(SNP_data[[x]][, 4] == y & SNP_data[[x]][, 5] == z)
        REF <- y
        ALT <- z
        samples <- strsplit(names(SNP_data)[x], ".",fixed = T)[[1]][1]
        group <- gsub("-[0-9]$|_[0-9]$","", samples)
        freq <- SNP_numbers / dim(SNP_data[[x]])[1]
        SNP_heatmap_data <- rbind(SNP_heatmap_data, data.frame(group, samples, REF, ALT, SNP_numbers, freq))
      }
    }
  }

  if(input$SNP_Analyse_mode == "heatmap"){
    SNP_Data <- SNP_heatmap_data[SNP_heatmap_data$group %in% input$SNP_Analysis_Group, ]
  }else{
    SNP_type_data <- SNP_heatmap_data[SNP_heatmap_data$group %in% input$SNP_Analysis_Group, ]
    SNP_type_data <- SNP_type_data[SNP_type_data$REF != SNP_type_data$ALT, ]
    SNP_type_data[(SNP_type_data$REF == "A" & SNP_type_data$ALT == "C") | (SNP_type_data$REF == "T" & SNP_type_data$ALT == "G"), "type"] <- "A>C/T>G"
    SNP_type_data[(SNP_type_data$REF == "A" & SNP_type_data$ALT == "G") | (SNP_type_data$REF == "T" & SNP_type_data$ALT == "C"), "type"] <- "A>G/T>C"
    SNP_type_data[(SNP_type_data$REF == "A" & SNP_type_data$ALT == "T") | (SNP_type_data$REF == "T" & SNP_type_data$ALT == "A"), "type"] <- "A>T/T>A"
    SNP_type_data[(SNP_type_data$REF == "C" & SNP_type_data$ALT == "A") | (SNP_type_data$REF == "G" & SNP_type_data$ALT == "T"), "type"] <- "C>A/G>T"
    SNP_type_data[(SNP_type_data$REF == "C" & SNP_type_data$ALT == "G") | (SNP_type_data$REF == "G" & SNP_type_data$ALT == "C"), "type"] <- "C>G/G>C"
    SNP_type_data[(SNP_type_data$REF == "C" & SNP_type_data$ALT == "T") | (SNP_type_data$REF == "G" & SNP_type_data$ALT == "A"), "type"] <- "C>T/G>A"
    SNP_Data <- SNP_type_data %>% group_by(type, group, samples) %>% summarise_if(is.numeric, sum) %>% as.data.frame()
  }
  return(SNP_Data)
})

##1-2-2 、绘制热图以及柱状图
SNP_Analysis_plot <- eventReactive(input$SNP_Analysis_action, {
  if(input$SNP_Analyse_mode == "heatmap"){
    data <- SNP_Analysis_data()
    SNP_data <- data %>% group_by(group, REF, ALT) %>% summarise_if(is.numeric, mean) %>% as.data.frame()
    SNP_data$freq <- SNP_data$freq * 100

    p <- ggplot2::ggplot(SNP_data, aes(ALT,REF,fill = freq))+
          geom_tile()+
          geom_text(aes(label = round(freq, digits = 2)), size = (input$heatmap_text.size)/4)+
          facet_wrap(~group,ncol = input$heatmap_ncol)+
          scale_fill_gradient(low = stringr::str_split(input$heatmap_color, ",")[[1]][1],
                              high = stringr::str_split(input$heatmap_color, ",")[[1]][2])+
          theme_test()+
          theme(text = element_text(face = "bold", family = "Times", size = input$heatmap_text.size, color = "black"))
  }else{
    SNP_type_data <- SNP_Analysis_data()
    if (input$SNP_barplot_mode == "dodge"){
      p <- ggpubr::ggbarplot(SNP_type_data, x = "type", y = "freq", fill = "group", palette = "Set1", label = F,
                             add = c("mean_sd"), width = input$SNV_barplot_bar_width, position = position_dodge(),
                             add.params = list(width = input$SNV_barplot_err_bar_width, size = input$SNV_barplot_err_bar_size),
                             ggtheme = theme_classic())+
        xlab(NULL) + ylab("Frequency of indicated SNVs")+
        theme(legend.position = "right",
              legend.title = element_text(color = "black", size = input$SNV_label_Text_size),
              legend.text = element_text(color = "black", size = input$SNV_label_Text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text = element_text(color = "black", size = input$SNV_label_Text_size),
              axis.title = element_text(color = "black", size = input$SNV_title_Text_size),
              text = element_text(face = "bold", family = "Times", color = "black"))
    }else{
      p <- ggplot2::ggplot(SNP_type_data, aes(x = group, y = freq, fill = type))+
        geom_bar(stat = "summary", color = NA, position = "stack")+
        scale_fill_brewer(palette = "Paired")+
        xlab(NULL) + ylab("Frequency of indicated SNVs")+
        theme_classic()+
        theme(legend.position = "right",
              legend.title = element_text(color = "black", size = input$SNV_label_Text_size),
              legend.text = element_text(color = "black", size = input$SNV_label_Text_size),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text = element_text(color = "black", size = input$SNV_label_Text_size),
              axis.title = element_text(color = "black", size = input$SNV_title_Text_size),
              text = element_text(face = "bold", family = "Times", color = "black"))
    }

    if(!is.null(input$barplot_ggText)){
      add_funcs <- strsplit(input$barplot_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
  }
  return(p)
})


## 构建图片显示以及下载输出控件
output$Display_SNP_Plot <- renderPlot({
  return(SNP_Analysis_plot())
})



output$SNP_plot <- downloadHandler(
    req(input$SNP_download_width,input$SNP_download_height),
    filename = "1.2_SNP_Analyse_plot.pdf",
    content = function(file){
      pdf(file, width = input$SNP_download_width, height = input$SNP_download_height)
      print(SNP_Analysis_plot())
      dev.off()
    }
)


#
# # 构建热图，柱状图输出表格
output$snp_analysis_data <- DT::renderDataTable(
  return(SNP_Analysis_data()),
  options = list(
    scrollX = TRUE,
    pageLength = 5
))

output$snp_tab <- downloadHandler(
  filename = function()  {paste0("snp_tab",".csv")},
  content = function(file) {
    write.csv(SNP_Analysis_data(), file, row.names = F)
  }
)






