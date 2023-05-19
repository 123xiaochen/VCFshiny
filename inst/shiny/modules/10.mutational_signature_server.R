output$signature_sample_id <- renderUI({
  virtualSelectInput(
    inputId = "signature_sampleID",  label = "Select samples:",
    choices = unique(gsub("\\..*","",names(raw_variants_list()))),
    selected = unique(gsub("\\..*","",names(raw_variants_list()))),
    multiple = T, search = F, width = "100%"
  )
})

mutational_signature_df <- eventReactive(input$analysis_signature, {
  raw_variants_list <- raw_variants_list()
  user <- lapply(input$signature_sampleID, function(x){
    name <- stringr::str_subset(names(raw_variants_list), pattern = x)
    data <- lapply(name, function(y){
      df1 <- raw_variants_list[[y]][,1:5]
    }) %>% bind_rows()
    data$sample <- x
    return(data)
  }) %>% bind_rows()

  if(length(unique(user$sample)) == 1){
    sendSweetAlert(session = session, title = "Warning", text = "The number of samples should be greater than 1 !", type = "warning")
    return()
  }
  user <- user[user[,1] %in% c("chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr2","chr20","chr21","chr22",
                         "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY" ), ]

  musica_data <- musicatk::create_musica(x = user, genome = musicatk::select_genome(input$signature_genome))
  musicatk::build_standard_table(musica_data, g = musicatk::select_genome(input$signature_genome), table_name = input$Signature_Build_Table_Name, overwrite = T)
  result_data <- musicatk::discover_signatures(musica = musica_data, table_name =  input$Signature_Build_Table_Name, num_signatures = input$num_signatures,
                                                algorithm = input$signature_discover_algorithm, nstart = 10)
  result_data
  return(result_data)
})

# Mutational Signature Plot Display and Download
#draw
mutational_signature_plot <- eventReactive(input$analysis_signature, {
  mutational_signature_df <- mutational_signature_df()
  if(input$signature_compare_cosmic == "FALSE"){
    musicatk::plot_signatures(mutational_signature_df, text_size = input$signature_discover_text_size, facet_size = input$signature_discover_facet_size)
  }else{
    comapre_cosmic <- musicatk::compare_cosmic_v2(mutational_signature_df, threshold = input$signature_discover_compare_threshold, )
  }
})
#display
output$Display_mutational_signature_plot <- renderPlot(
  return(mutational_signature_plot())
)
#download
output$Download_mutational_signature_Plot <- downloadHandler(
  filename = function(){ paste0("10_mutational_signature_",input$Signature_Build_Table_Name, ".pdf")},
  content = function(file){
    pdf(file, width = input$Mutational_Signature_download_width, height = input$Mutational_Signature_download_height)
    print(mutational_signature_plot())
    dev.off()
  }
)

# Mutational Signature Data Display and Download
#display
output$Display_mutational_signature_data <- DT::renderDataTable(
  return(mutational_signature_df()@signatures),
  options = list(
    scrollX = TRUE,
    pageLength = 5
  )
)
#download
output$Download_mutational_signature_data <- downloadHandler(
  filename = function()  {paste0("10_mutational_signature_", input$Signature_Build_Table_Name, ".csv")},
  content = function(file) {
    write.csv(mutational_signature_df()@signatures, file, row.names = T)
  }
)


