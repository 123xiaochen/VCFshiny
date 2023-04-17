#2-2、找出突变基因并绘制柱状图--------------------------------------------------------------------------------------------------------------------------

# 2-2.1、筛选突变基因
output$Variants_sample_id <- renderUI({
  virtualSelectInput(
    inputId = "Variants_sampleID",  label = "Select samples:",
    choices = unique(gsub("\\..*","",names(raw_variants_list()))),
    selected = unique(gsub("\\..*","",names(raw_variants_list()))),
    multiple = F, search = F, width = "100%"
  )
})

output$Variants_feature_column <- renderUI({
  virtualSelectInput(
    inputId = "variants_feature_column",  label = "Select Genomic Feature Column:",
    choices = colnames(raw_variants_list()[[1]]),
    selected = colnames(raw_variants_list()[[1]])[6], multiple = F, search = F, width = "100%"
  )
})

output$Variants_gene_column <- renderUI({
  virtualSelectInput(
    inputId = "variants_gene_column",  label = "Select Related Gene Column:",
    choices = colnames(raw_variants_list()[[1]]),
    selected = colnames(raw_variants_list()[[1]])[7], multiple = F, search = F, width = "100%"
  )
})

output$Variants_bar_position_id <- renderUI({
  if (input$Variants_type_id == "all") {
    simplifed_df1 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".indel")]] %>% dplyr::bind_rows()
    simplifed_df2 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".snp")]] %>% dplyr::bind_rows()
    simplifed_df <- rbind(simplifed_df1, simplifed_df2)
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
  }else {
    simplifed_df <- raw_variants_list()[[paste0(input$Variants_sampleID, ".", input$Variants_type_id)]]
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
  }
  no_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column, invert = T), ])
  need_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column), ])
  if(nrow(need_splite_df) > 0){
    splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% unique()
    combined_df <- rbind(splited_df, no_splite_df)
  }else{
    combined_df <- no_splite_df
  }

  virtualSelectInput(
    inputId = "Variants_bar_position_ID",  label = "Select Position To Analyse:",
    choices = unique(combined_df$feature_column),  selected = unique(combined_df$feature_column), multiple = T, zIndex = 4, search = F, width = "100%"
  )
})

Variants_genes <- eventReactive(input$plot_Variants_genes, {
  raw_variants_list <- raw_variants_list()

  if (input$Variants_type_id == "all") {
    simplifed_df1 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".indel")]] %>% dplyr::bind_rows()
    simplifed_df2 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".snp")]] %>% dplyr::bind_rows()
    simplifed_df <- rbind(simplifed_df1, simplifed_df2)
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_gene_column] <- "gene_column"
  }else {
    simplifed_df <- raw_variants_list()[[paste0(input$Variants_sampleID, ".", input$Variants_type_id)]] %>% dplyr::bind_rows()
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_gene_column] <- "gene_column"
  }

  need_splite_df <- as.data.frame(simplifed_df[(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)),])
  no_splite_df <- as.data.frame(simplifed_df[!(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)) ,])

  if(nrow(need_splite_df) > 0){
    splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>%
      tidyr::separate_rows(gene_column, sep = ";") %>% unique()
    combined_df <- rbind(splited_df, no_splite_df)
  }else{
      combined_df <- no_splite_df
  }
  combined_df <- combined_df %>% dplyr::group_by(feature_column, gene_column) %>% dplyr::count()
  combined_df <- combined_df[combined_df$feature_column %in% input$Variants_bar_position_ID, ]
  colnames(combined_df) <- c("Position", "Genes", "Numbers")
  combined_df <- combined_df[combined_df$Genes !=  "NONE", ]
  combined_df <- combined_df[combined_df$Genes !=  "", ]
  df <- combined_df %>% dplyr::group_by(Position, Genes) %>% summarise_at(.vars = "Numbers", sum)
  df <- df[order(df$Numbers, decreasing = T), ]
  return(df)
})

# 2-2.2 对筛选突变的同一基因进行合并
output$Variants_df <- DT::renderDataTable(
  Variants_genes(),
  options = list(
    scrollX = TRUE,
    pageLength = 5
  )
)

output$summary_gene_tab <- downloadHandler(
  filename = function()  {paste0("7_Relevant Genes",".csv")},
  content = function(file) {
    write.csv(Variants_genes(), file, row.names = F)
  }
)


#2-2.3、 绘制突变基因柱状图
Variants_plot <- eventReactive(input$plot_Variants_genes, {
  withProgress(message = "processing", min = 0, max = 1, {
  incProgress(0.4, detail = "Analyse Data ...")
  Variants_df <- Variants_genes() %>% group_by(Genes) %>% summarise_at(.vars = "Numbers", sum)
  Variants_df <- Variants_df[Variants_df$Genes != "",]
  Variants_genes <- Variants_df[order(Variants_df$Numbers, decreasing = T), ][1:input$Variants_genes_numbers, ]
  incProgress(0.6, detail = "Analyse Data...")
  p <- ggpubr::ggbarplot(Variants_genes, x = "Genes", y = "Numbers", label = as.logical(input$Variants_genes_label),
                    width = input$Variants_genes_bar_width, position = position_dodge(),
                    fill = "Genes", ggtheme = theme_classic())+
    xlab(NULL) + ylab("Variants Genes Numbers of biotypes")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(color = "black", size = input$Variants_genes_label_text_size),
          axis.title = element_text(color = "black", size = input$Variants_genes_title_text_size),
          text = element_text(face = "bold", family = "Times", color = "black"))

  if (!is.null(input$variants_genes_ggText)){
    add_funcs <- strsplit(input$variants_genes_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  incProgress(0.8, detail = "Analyse data...")
  return(p)
  })
})

output$Variants_Plots <- renderPlot({
  Variants_plot()
})

#2-2.4、 下载突变基因柱状图
output$Variants_genes_Download <- downloadHandler(
  req(input$Variants_Genes_download_width, input$Variants_Genes_download_height),
  filename = function(){
    paste(paste("7", input$Variants_sampleID,input$Variants_type_id, "Variants_Genes_Summerise_plot",  sep = "_"), "pdf",  sep = ".")
  },
  content = function(file){
    pdf(file, width = input$Variants_Genes_download_width, height = input$Variants_Genes_download_height)
    print(Variants_plot())
    dev.off()
  }
)

