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
  req(raw_variants_list(), input$Variants_sampleID)
  if (input$Variants_type_id == "all") {
    simplifed_df1 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".indel")]]
    colnames(simplifed_df1)[colnames(simplifed_df1) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df1)[colnames(simplifed_df1) == input$variants_gene_column] <- "gene_column"
    simplifed_df1 <- simplifed_df1 %>% group_by(feature_column, gene_column) %>% count()

    simplifed_df2 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".snp")]]
    colnames(simplifed_df2)[colnames(simplifed_df2) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df2)[colnames(simplifed_df2) == input$variants_gene_column] <- "gene_column"
    simplifed_df2 <- simplifed_df2 %>% group_by(feature_column, gene_column) %>% count()

    simplifed_df <- rbind(simplifed_df1, simplifed_df2)
  }else {
    simplifed_df <- raw_variants_list()[[paste0(input$Variants_sampleID, ".", input$Variants_type_id)]]
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_gene_column] <- "gene_column"
    simplifed_df <- simplifed_df %>% group_by(feature_column, gene_column) %>% count()
  }

  virtualSelectInput(
    inputId = "Variants_bar_position_ID",  label = "Select Position To Analyse:",
    choices = unique(simplifed_df$feature_column),  selected = unique(simplifed_df$feature_column), multiple = T, zIndex = 4, search = F, width = "100%"
  )
})

Variants_genes <- eventReactive(input$plot_Variants_genes, {
  raw_variants_list <- raw_variants_list()

  if (input$Variants_type_id == "all") {
    simplifed_df1 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".indel")]]
    colnames(simplifed_df1)[colnames(simplifed_df1) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df1)[colnames(simplifed_df1) == input$variants_gene_column] <- "gene_column"
    simplifed_df1 <- simplifed_df1 %>% group_by(feature_column, gene_column) %>% count()

    simplifed_df2 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".snp")]]
    colnames(simplifed_df2)[colnames(simplifed_df2) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df2)[colnames(simplifed_df2) == input$variants_gene_column] <- "gene_column"
    simplifed_df2 <- simplifed_df2 %>% group_by(feature_column, gene_column) %>% count()

    simplifed_df <- rbind(simplifed_df1, simplifed_df2)
  }else {
    simplifed_df <- raw_variants_list()[[paste0(input$Variants_sampleID, ".", input$Variants_type_id)]]
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_gene_column] <- "gene_column"
    simplifed_df <- simplifed_df %>% group_by(feature_column, gene_column) %>% count()
  }

  simplifed_df <- simplifed_df[simplifed_df$feature_column %in% input$Variants_bar_position_ID, ]

  no_splite_df <- as.data.frame(simplifed_df[grep(";", simplifed_df$gene_column, invert = T), ])

  if(";" %in% simplifed_df$gene_column){
    need_splite_df <- simplifed_df[grep(";", simplifed_df$gene_column), ]
    splited_df <- lapply(1:nrow(need_splite_df), function(x){
      df <- data.frame(feature_column = need_splite_df[x, "feature_column"],
                       gene_column = stringr::str_split(need_splite_df[x, "gene_column"], ";") %>% unlist,
                       n = need_splite_df[x, "n"])
    }) %>% bind_rows()
    combined_df <- rbind(no_splite_df, splited_df)
  }else{
    combined_df <- no_splite_df
  }

  colnames(combined_df) <- c("Position", "Genes", "Numbers")
  combined_df <- combined_df[combined_df$Genes !=  "NONE", ]

  df <- combined_df %>% group_by(Position, Genes) %>% summarise_at(.vars = "Numbers", sum)

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
  Variants_genes <- Variants_df[order(Variants_df$Numbers, decreasing = T), ][1:input$Variants_genes_numbers ,]
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

