# 1-4、Indel 各个变异类型统计图 -------------------------------------------------------------------------------------------------------------------------

#Indel数据筛选
Indel_data <- reactive({
  Indel_ALL_list <- lapply(stringr::str_subset(names(raw_variants_list()),pattern = ".indel"),function(x){
    Indel_ALL <- raw_variants_list()[[x]]
  })
  names(Indel_ALL_list) <- stringr::str_subset(names(raw_variants_list()),pattern = ".indel")
  return(Indel_ALL_list)
})

### Indel 突变长度密度分布图
output$Indel_analysis_group <- renderUI({
  observe(Indel_data())
  virtualSelectInput(
    inputId = "Indel_analysis_Group",  label = "Sample groups:",
    choices = unique(gsub("-[0-9].indel$|_[0-9].indel$|.indel","",names(Indel_data()))),
    selected = unique(gsub("-[0-9].indel$|_[0-9].indel$|.indel","",names(Indel_data()))),
    multiple = T, search = F, width = "100%"
  )
})

Indel_Length_Data <- eventReactive(input$Indel_Analysis_action, {
  withProgress(message = "Loading Indel data...", min = 0, max = 1, {
    indel_list <- Indel_data()
    df <- lapply(names(indel_list), function(x){
      df <- indel_list[[x]]
      df$Indel_length <- nchar(df[, 5]) - nchar(df[, 4])
      df$Indel_type <- ifelse(df$Indel_length > 0, "Insertion", "Delection")

      df$group <- gsub("-[0-9].indel$|_[0-9].indel$|.indel","", x)
      df$samples <- x
      df
    }) %>% dplyr::bind_rows()
    incProgress(0.4, message = "Analyse Indel data ...")

    if(input$Indel_Analyse_mode == "density"){
      df$Indel_length <- abs(df$Indel_length)
    }else {
      df$Indel_size = ifelse(df$Indel_length>50, "I >50bp",
                              ifelse(df$Indel_length>25, "I 26-50bp",
                                     ifelse(df$Indel_length>10, "I 11-25bp",
                                            ifelse(df$Indel_length>7, "I 8-10bp",
                                                   ifelse(df$Indel_length>4, "I 5-7bp",
                                                          ifelse(df$Indel_length>1, "I 2-4bp",
                                                                 ifelse(df$Indel_length == 1, "I 1bp",
                                                                        ifelse(df$Indel_length == -1, "D 1bp",
                                                                               ifelse(df$Indel_length>-5, "D 2-4bp",
                                                                                      ifelse(df$Indel_length>-8,"D 5-7bp",
                                                                                             ifelse(df$Indel_length>-11,"D 8-10bp",
                                                                                                    ifelse(df$Indel_length>-26,"D 11-25bp",
                                                                                                           ifelse(df$Indel_length>-50,"D 26-50bp","D >50bp")))))))))))))
      df$Indel_size <- factor(df$Indel_size, levels = c("D >50bp","D 26-50bp","D 11-25bp","D 8-10bp","D 5-7bp","D 2-4bp","D 1bp",
                                                          "I >50bp","I 26-50bp","I 11-25bp","I 8-10bp","I 5-7bp","I 2-4bp","I 1bp"))
      df <- df %>% dplyr::group_by(samples, group, Indel_size) %>% dplyr::count() %>% as.data.frame()
      df <- lapply(df$samples, function(x){
        df <- df[df$samples == x, ]
        df$Proportion <- ((df$n) / sum(df$n)) * 100
        df
      }) %>% bind_rows()
    }
    df <- df[df$group %in% input$Indel_analysis_Group, ]
    incProgress(0.8, message = "Drawwing Indel Plot ...")
  })
  return(df)
})

Indel_Length_plot <- eventReactive(input$Indel_Analysis_action, {
  req(Indel_Length_Data(), input$Indel_Analyse_mode)
    if(input$Indel_Analyse_mode == "density"){
      ggplot(Indel_Length_Data(), aes(Indel_length, fill = samples, color = samples))+
        xlab("Indel Length (bp)")+
        geom_density(alpha = input$Indel_density_alpha, position = input$Indel_density_position)+
        facet_wrap(.~Indel_type)+
        geom_rug()+
        theme_bw()+
        theme(text = element_text(face = "bold", family = "Times", size = input$Indel_density_text_size, color = "black"))
    }else{
      ggplot2::ggplot(Indel_Length_Data(), aes(samples, Indel_size,fill = Proportion))+
        geom_tile()+
        labs(x = NULL, y = NULL)+
        geom_text(aes(label = round(Proportion, digits = input$Indel_heatmap_digits)), size = (input$Indel_heatmap_Text_size)/4)+
        scale_fill_gradient(low = stringr::str_split(input$Indel_heatmap_color, ",")[[1]][1],
                            high = stringr::str_split(input$Indel_heatmap_color, ",")[[1]][2])+
        theme_test()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(face = "bold", family = "Times", size = input$Indel_heatmap_Text_size, color = "black"))
    }
})

output$Indel_Density_data <- DT::renderDataTable(
  return(Indel_Length_Data()),
  options = list(
    dom = 'Bfrtip',
    scrollX = TRUE,
    pageLength = 5
))

output$indel_tab <- downloadHandler(
  filename = function()  {paste0("4_indel_tab",".csv")},
  content = function(file) {
    write.csv(Indel_Length_Data(), file, row.names = F)
  }
)

output$Display_Indel_plot <- renderPlot({
  return(Indel_Length_plot())
})

#下载图片控件
output$Indel_plot <- downloadHandler(
  filename = function() {paste(paste("4_Indel_Analyse", input$Indel_Analyse_mode, sep = "_"),".pdf")},
  content = function(file){
    pdf(file, width = input$Indel_download_width, height = input$Indel_download_height)
    print(Indel_Length_plot())
    dev.off()
})
