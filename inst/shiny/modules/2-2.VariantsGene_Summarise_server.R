#2-2、找出突变基因并绘制柱状图--------------------------------------------------------------------------------------------------------------------------

# 2-2.1、筛选突变基因

output$Variants_sample_id <- renderUI({
  samples <- names(ALL_variants_vcf())
  selectInput("Variants_sampleID", "Select your samples to view:", choices = samples, width = "100%")
})

# output$Variants_barplot_position_id <- renderUI({
#   req(input$Variants_genes_select_position)
#   if(input$Variants_genes_select_position == 'TRUE'){
#     ALL_variants_vcf <- ALL_variants_vcf()
#     position <- unique(ALL_variants_vcf[[input$Variants_sampleID]]$Func.refGene)
#     selectInput("Variants_barplot_position_ID", "Select Position To Analyse:", choices = position, multiple = T ,width = "100%")
#   }
# })

Variants_genes <- reactive({
  ALL_variants_vcf <- ALL_variants_vcf()

  cl <- parallel::makeCluster(6)
  Genes_DF <- parallel::parLapply(cl,names(ALL_variants_vcf), function(x){
    require(dplyr)
    combined_df <- data.frame()

    simplifed_df <- ALL_variants_vcf[[x]] %>% group_by(Func.refGene, Gene.refGene) %>% count()
    need_splite_df <- simplifed_df[grep(";", simplifed_df$Gene.refGene), ]
    no_splite_df <- simplifed_df[grep(";", simplifed_df$Gene.refGene, invert = T), ]

    splited_df <- lapply(1:nrow(need_splite_df), function(y){
      data.frame(Func.refGene = need_splite_df[y, "Func.refGene"],
                 Gene.refGene = stringr::str_split(need_splite_df[y, "Gene.refGene"], ";") %>% unlist,
                 n = need_splite_df[y, "n"])
    }) %>% bind_rows()

    combined_df <- rbind(no_splite_df, splited_df)
    colnames(combined_df) <- c("Position", "Genes", "Numbers")

    combined_df <- combined_df[combined_df$Genes !=  "NONE", ]
  })
  parallel::stopCluster(cl)

  names(Genes_DF) <- names(ALL_variants_vcf)
  return(Genes_DF)
})

# 2-2.2 对筛选突变的同一基因进行合并

Variants_ALL_genes <- reactive({
  Variants_genes <- Variants_genes()
  selected_df <- lapply(names(Variants_genes), function(x){
    df <- Variants_genes[[x]]
    all_df <- df %>% group_by(Position, Genes) %>% summarise_at(.vars = "Numbers", sum)
  })
  names(selected_df) <- names(Variants_genes)
  return(selected_df)
})




observeEvent(input$plot_Variants_genes, {
  req(input$Variants_sampleID, input$Variants_Genes_data_Set)
  output$Variants_df <- DT::renderDataTable(
    return(Variants_ALL_genes()[[input$Variants_sampleID]]), extensions ="Buttons",
    server =  as.logical(input$Variants_Genes_data_Set),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download')
      )
    )
  )
})

#2-2.3、 绘制突变基因柱状图
Variants_plot <- eventReactive(input$plot_Variants_genes, {
  withProgress(message = "processing", min = 0, max = 1, {
  req(input$Variants_genes_numbers, input$Variants_genes_bar_width, input$Variants_genes_text_size, input$Variants_sampleID,input$Variants_genes_label)
  incProgress(0.4, detail = "Analyse Data ...")
  Variants_df <- Variants_ALL_genes()[[input$Variants_sampleID]]
  Variants_genes <- Variants_df[order(Variants_df$Numbers, decreasing = T), ][1:input$Variants_genes_numbers ,]
  incProgress(0.6, detail = "Analyse Data...")
  p <- ggpubr::ggbarplot(Variants_genes, x = "Genes", y = "Numbers", label = as.logical(input$Variants_genes_label),
                    width = input$Variants_genes_bar_width, position = position_dodge(),
                    fill = "Genes", ggtheme = theme_classic())+
    xlab(NULL) + ylab("Variants Genes Numbers of biotypes")+
    theme(text = element_text(face = "bold",family = "Times",size = input$Variants_genes_text_size,color = "black"),legend.position = "none",
          strip.background = element_blank(), axis.text.x = element_text(angle = 45,hjust = 1))

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
    paste("Variants_Genes_Summerise_plot", input$Variants_sampleID, input$Variants_Genes_down_filetype, "pdf", sep = ".")
  },
  content = function(file){
    pdf(file, width = input$Variants_Genes_download_width, height = input$Variants_Genes_download_height)
    print(Variants_plot())
    dev.off()
  }
)

