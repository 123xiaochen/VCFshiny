
#2-3、构建各组变异基因热图数据-----------------------------------------------------------------------------------------------------------------------
output$Variants_heatmap_sampleID <- renderUI({
  virtualSelectInput(
    inputId = "Variants_heatmap_sampleID",  label = "Select samples:",
    choices = unique(gsub("\\..*","",names(raw_variants_list()))),
    selected = unique(gsub("\\..*","",names(raw_variants_list()))),
    multiple = T, search = F, width = "100%"
  )
})

output$Variants_heatmap_feature_column <- renderUI({
  virtualSelectInput(
    inputId = "variants_heatmap_feature_column",  label = "Select Genomic Feature Column:",
    choices = colnames(raw_variants_list()[[1]]),
    selected = colnames(raw_variants_list()[[1]])[6], multiple = F, search = F, width = "100%"
  )
})

output$Variants_heatmap_gene_column <- renderUI({
  virtualSelectInput(
    inputId = "variants_heatmap_gene_column",  label = "Select Related Gene Column:",
    choices = colnames(raw_variants_list()[[1]]),
    selected = colnames(raw_variants_list()[[1]])[7], multiple = F, search = F, width = "100%"
  )
})

#2-3.1、 构建选择位置输入框
output$Variants_position_id <- renderUI({
  if (input$Variants_heatmap_types == "all") {
    simplifed_df1 <- raw_variants_list()[paste0(input$Variants_heatmap_sampleID, ".indel")] %>% dplyr::bind_rows()
    simplifed_df2 <- raw_variants_list()[paste0(input$Variants_heatmap_sampleID, ".snp")] %>% dplyr::bind_rows()
    simplifed_df <- rbind(simplifed_df1, simplifed_df2)
  }else {
    simplifed_df <- raw_variants_list()[paste0(input$Variants_heatmap_sampleID, ".", input$Variants_heatmap_types)] %>% dplyr::bind_rows()
  }
  colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_feature_column] <- "feature_column"
  no_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column, invert = T), ])
  need_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column), ])

  if(nrow(need_splite_df) > 0){
    splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% unique()
    combined_df <- rbind(no_splite_df, splited_df)
  }else{
    combined_df <- no_splite_df
  }

  virtualSelectInput(
    inputId = "Variants_position_ID",  label = "Select Position To Analyse:",
    choices = unique(combined_df$feature_column),  selected = unique(combined_df$feature_column), multiple = T, zIndex = 4,search = F, width = "100%"
  )
})



Variants_heatmap_data <- eventReactive(input$Variants_heatmap, {
  Variants_heatmap_df <- lapply(input$Variants_heatmap_sampleID, function(x){
    if (input$Variants_heatmap_types == "all") {
      simplifed_df1 <- raw_variants_list()[paste0(x, ".indel")] %>% dplyr::bind_rows()
      simplifed_df2 <- raw_variants_list()[paste0(x, ".snp")] %>% dplyr::bind_rows()
      simplifed_df <- rbind(simplifed_df1, simplifed_df2)
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_feature_column] <- "feature_column"
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_gene_column] <- "gene_column"
    }else {
      simplifed_df <- raw_variants_list()[paste0(x, ".", input$Variants_heatmap_types)] %>% dplyr::bind_rows()
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_feature_column] <- "feature_column"
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_gene_column] <- "gene_column"
    }

    need_splite_df <- as.data.frame(simplifed_df[(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)) ,])
    no_splite_df <- as.data.frame(simplifed_df[!(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)) ,])

    if(nrow(need_splite_df) > 0){
      splited_df <- need_splite_df  %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% tidyr::separate_rows(gene_column, sep = ";") %>% unique()
      combined_df <- rbind(splited_df,no_splite_df)
    }else{
      combined_df <- no_splite_df
    }
    combined_df <- combined_df %>% dplyr::group_by(feature_column, gene_column) %>% dplyr::count()
    combined_df <- combined_df[combined_df$feature_column %in% input$Variants_position_ID, ]
    colnames(combined_df) <- c("Position", "Genes", "Numbers")
    combined_df <- combined_df[combined_df$Genes !=  "NONE", ]
    combined_df <- combined_df[combined_df$Genes !=  "", ]

    combined_df <- combined_df %>% dplyr::group_by(Genes) %>% dplyr::summarise_at(.vars = "Numbers", sum) %>% as.data.frame()

    IntOGen <- readRDS(system.file("extdata", "IntOGen_cancer_driver_gene.rds", package = "VCFshiny"))
    combined_df <- combined_df[combined_df$Genes %in% IntOGen$Symbol, ]
    combined_df <- combined_df[order(combined_df$Numbers, decreasing = T), ]
    colnames(combined_df) <- c("Genes", x)
    combined_df <- na.omit(combined_df)
  }) %>% plyr::join_all(by = "Genes", type = "full")
  Variants_heatmap_df[is.na(Variants_heatmap_df)] <- 0
  return(Variants_heatmap_df)
})


output$Variants_Heatmap_data <- DT::renderDataTable(
  return(Variants_heatmap_data()),
  options = list(
    scrollX = TRUE,
    pageLength = 5
  )
)

output$heatmap_gene_tab <- downloadHandler(
  filename = function()  {paste0("8_Heatmap info table",".csv")},
  content = function(file) {
    write.csv(Variants_heatmap_data(), file, row.names = F)
  }
)

#2-3.2 、 绘制热图
Heat_Plot_2 <- eventReactive(input$Variants_heatmap, {
  withProgress(message = "processing", min = 0, max = 1, {

  Variants_heatmap_data <- as.data.frame(Variants_heatmap_data())
  Variants_heatmap_data <- Variants_heatmap_data %>% column_to_rownames(var = "Genes")

  pheatmap::pheatmap(Variants_heatmap_data, cluster_rows = F, cluster_cols = F,
                     color = grDevices::colorRampPalette(c("GhostWhite", "red"))(50),
                     show_rownames = as.logical(input$Variants_heatmap_show_rownames),
                     show_colnames = as.logical(input$Variants_heatmap_show_colnames),
                     breaks = c(seq(0, input$Variants_heatmap_colorValue, input$Variants_heatmap_colorValue / 50)),
                     legend_breaks = seq(0, input$Variants_heatmap_colorValue, 2),
                     legend_labels = c(seq(0, input$Variants_heatmap_colorValue - 2, 2) %>% as.character, paste0(input$Variants_heatmap_colorValue, "+")))
  })
})


output$Heatmap_plot_2 <- renderPlot({
  Heat_Plot_2()
})

#2-3.3、 下载热图
output$Variants_heatmap_Download <- downloadHandler(
  filename = function(){
    paste(paste("8", input$Variants_heatmap_types, "Variants_Heatmap_Summerise_plot",  sep = "_"), "pdf", sep = ".")
  },
  content = function(file){
    pdf(file, width = input$Variants_heatmap_download_width, height = input$Variants_heatmap_download_height)
    print(Heat_Plot_2())
    dev.off()
  }
)
