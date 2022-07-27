#2-4、选择组进行差异分析 -------------------------------------------------------------------------------------------------------

#2-4.1、构建选择控件
output$DESeq_group_1 <- renderUI({
  ALL_variants_vcf <- ALL_variants_vcf()
  groups <- stringr::str_remove(names(ALL_variants_vcf()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
  selectInput("DESeq_group_ID1", "Select Your Reference Group:", choices = groups, width = "100%")
})

output$DESeq_group_2 <- renderUI({
  ALL_variants_vcf <- ALL_variants_vcf()
  groups <- stringr::str_remove(names(ALL_variants_vcf()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
  selectInput("DESeq_group_ID2", "Select Your Test Group:", choices = groups, width = "100%")
})

output$DESeq_position_id <- renderUI({
  ALL_variants_vcf <- ALL_variants_vcf()
  position <- unique((ALL_variants_vcf[[input$Variants_sampleID]])$Func.refGene)
  selectInput("DESeq_position_ID", "Select Position To Analyse:", choices = position, multiple = T ,width = "100%")
})

#2-4.2、提取两组间差异基因
DE_genes_df <- reactive({
  Variants_ALL_genes <- Variants_ALL_genes()
  samples_1 <- stringr::str_subset(names(Variants_ALL_genes),pattern = input$DESeq_group_ID1, negate = F) %>%
    stringr::str_subset(pattern = input$DESeq_Type_ID, negate = F)
  samples_2 <- stringr::str_subset(names(Variants_ALL_genes),pattern = input$DESeq_group_ID2, negate = F) %>%
    stringr::str_subset(pattern = input$DESeq_Type_ID, negate = F)

  merge_data_1 <- lapply(samples_1, function(x){
    df <- Variants_ALL_genes[[x]]
    df <- df[df$Position == input$DESeq_position_ID, ]
  }) %>% bind_rows()
  merge_data_1 <- as.data.frame(merge_data_1)
  merge_data_1 <- merge_data_1 %>% group_by(Genes, Numbers) %>% summarise_if(is.numeric, mean) %>% summarise_each(funs(sum))
  colnames(merge_data_1) <- c("Genes", "Numbers_1")
  merge_data_1$Numbers_1 <- round(merge_data_1$Numbers_1/length(samples_1))


  merge_data_2 <- lapply(samples_2, function(x){
    df <- Variants_ALL_genes[[x]]
    df <- df[df$Position == input$DESeq_position_ID, ]
  }) %>% bind_rows()
  merge_data_2 <- as.data.frame(merge_data_2)
  merge_data_2 <- merge_data_2 %>% group_by(Genes, Numbers) %>% summarise_if(is.numeric, mean) %>% summarise_each(funs(sum))
  colnames(merge_data_2) <- c("Genes","Numbers_2")
  merge_data_2$Numbers_2 <- round(merge_data_2$Numbers_2/length(samples_2))


  merge_df <- merge(merge_data_1, merge_data_2, by = "Genes", all = T)
  merge_df[is.na(merge_df)] <- 0
  merge_df <- merge_df[merge_df$Genes != "NONE", ]
  merge_df$Change <- log2(merge_df$Numbers_2+1)-log2(merge_df$Numbers_1+1)
  return(merge_df)
})

#2-4.4、GO富集分析
enrich_plot_data <- eventReactive(input$plot_enrichPlot,{
  withProgress(message = "processing", min = 0, max = 1, {

  req(input$enrich_Species_Orgdb ,input$enrich_Fun, input$enrich_geneID)
  library(input$enrich_Species_Orgdb, character.only = T)
  DE_genes_df <- DE_genes_df()

  up_gene <- DE_genes_df[DE_genes_df$Change > 1, "Genes"]
  down_gene <- DE_genes_df[DE_genes_df$Change < -1, "Genes"]
  incProgress(0.2, detail = "Analyse Data ...")
  if(input$enrich_Fun == "enrichGO"){
    req(input$enrich_ont)
    UD_List <- list(Up = up_gene, Down = down_gene)
    enrich_results <- clusterProfiler::compareCluster(UD_List, fun = input$enrich_Fun, OrgDb = input$enrich_Species_Orgdb,
                                                      keyType = input$enrich_geneID, pvalueCutoff = 0.05, ont = input$enrich_ont)
    incProgress(0.4, detail = "Analyse Data ...")
  }else{
    if(input$enrich_Species_Orgdb == "org.Hs.eg.db"){
      enrich_Species_organism = "hsa"
    }else if(input$enrich_Species_Orgdb == "org.Rn.eg.db"){
      enrich_Species_organism = "rno"
    }else if(input$enrich_Species_Orgdb == "org.Mm.eg.db"){
      enrich_Species_organism = "mmu"
    }else if(input$enrich_Species_Orgdb == "org.Ss.eg.db"){
      enrich_Species_organism = "ssc"
    }else if(input$enrich_Species_Orgdb == "org.Dr.eg.db"){
      enrich_Species_organism = "dre"
    }else if(input$enrich_Species_Orgdb == "org.Bt.eg.db"){
      enrich_Species_organism = "bta"
    }
    incProgress(0.4, detail = "Analyse Data...")
    if(input$enrich_geneID != "ENTREZID"){
      up_gene <- clusterProfiler::bitr(up_gene, fromType = input$enrich_geneID, toType = "ENTREZID", OrgDb = input$enrich_Species_Orgdb)$ENTREZID
      down_gene <- clusterProfiler::bitr(down_gene, fromType = input$enrich_geneID, toType = "ENTREZID", OrgDb = input$enrich_Species_Orgdb)$ENTREZID
    }
    UD_List <- list(Up = up_gene, Down = down_gene)
    enrich_results <- clusterProfiler::compareCluster(UD_List, fun = input$enrich_Fun, organism = enrich_Species_organism, pvalueCutoff = 0.05)
  }
  incProgress(0.6, detail = "Analyse Data...")
  return(enrich_results)
  })
})



enrich_Plot <- eventReactive(input$plot_enrichPlot ,{
  req(input$showCategory_numbers, input$enrichplot_fontSize)
  p <- enrichplot::dotplot(enrich_plot_data(), showCategory = input$showCategory_numbers, font.size = input$enrichplot_fontSize)+
    scale_y_discrete(labels = function(x) str_wrap(x, width = 100))

  if (!is.null(input$Enrich_AddCodes)){
    add_funcs <- strsplit(input$Enrich_AddCodes, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})


observeEvent(input$plot_enrichPlot, {
  req(input$Enrich_Data_set)
  output$DESeq_data <- DT::renderDataTable(
    return(as.data.frame(enrich_plot_data())), extensions ="Buttons",
    server =  as.logical(input$Enrich_Data_set),
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


output$Enrich_Plot <- renderPlot({
  enrich_Plot()
})

output$enrich_PlotUI <- renderUI({
  shinycssloaders::withSpinner(plotOutput("Enrich_Plot", width = paste0(input$Enrich_plot_width, "%"), height = paste0(input$Enrich_plot_height, "px")))
})



#2-4.4、下载GO富集分析图

output$Enrich_plot_Download <- downloadHandler(
  req(input$DESeq_group_ID1, input$DESeq_group_ID2, input$enrich_Fun, input$Enrich_plot_download_width, input$Enrich_plot_download_height),
  filename = function(){
    paste(input$DESeq_group_ID1,"VS", input$DESeq_group_ID2, input$enrich_Fun, "plot", "pdf", sep = ".")
  },
  content = function(file){
    pdf(file, width = input$Enrich_plot_download_width, height = input$Enrich_plot_download_height)
    print(enrich_Plot())
    dev.off()
  }
)



