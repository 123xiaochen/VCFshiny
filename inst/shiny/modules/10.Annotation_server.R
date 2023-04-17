output$Annovar_anno_Species <- renderUI({
  species_name <- dir("./Annovar/humandb")[grepl(dir("./Annovar/humandb"), pattern = ".txt$|.txt.gz$")] %>% substr(0,4) %>% unique()
  if (is.null(species_name)) {
    selectInput(inputId = "Annovar_species",label = "Select Annotation Species:",choices = c("Please download datebase first !"),
                selected = "Please download datebase first !", multiple = F, width = "100%")
  }else{
    selectInput(inputId = "Annovar_species",label = "Select Annotation Species:",choices = species_name, multiple = F, width = "100%")
  }
 })

output$Annovar_anno_Protocol <- renderUI({
  req(input$Annovar_species)
  datebase_name <- dir("./Annovar/humandb")[grepl(dir("./Annovar/humandb"), pattern = input$Annovar_species) &
                                              grepl(dir("./Annovar/humandb"), pattern = ".txt$|.txt.gz$")]
  if (is.null(datebase_name)) {
    selectInput(inputId = "Annovar_protocol","Select Annotation Protocol:",choices = c("Please download datebase first !"),
                selected = "Please download datebase first !", multiple = F, width = "100%")
  }else{
    datebase_name <- stringr::str_remove(datebase_name, pattern = paste0("^", input$Annovar_species,"_"))
    datebase_name <- stringr::str_remove(datebase_name, pattern = ".txt$|.txt.gz$")
    selectInput(inputId = "Annovar_protocol","Select Annotation Protocol:",choices = datebase_name, multiple = T, width = "100%")
  }
})

# Annovar Datebase Table ---------------------------------------------------------------------------------------------------------
Annovar_datebase_table <- reactive({
  Table <- read.csv("Annovar/Annovar_Datebase_Table.CSV", header = T, sep = ",")
})


output$Annovar_Datebase_Table <- DT::renderDataTable({
  Annovar_datebase_table()
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))

# Download Annovar annotation database-------------------------------------------------------------------------------------------
download_database <- eventReactive(input$Annovar_download_section, {
  withProgress(message = "processing", min = 0, max = 1, {
    cat(">>>  preparing datebase upload  ...\t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
    downdb <- try(system2(command = "perl", args =paste0("./Annovar/annotate_variation.pl -buildver ", input$Annovar_download_species, " -downdb ",
                  input$Annovar_download_web, " ", input$Annovar_download_name, " ./Annovar/humandb"), stdout = T, stderr = T))
    options(timeout=100000)
    if (grepl("http://", downdb) %>% any()) {
      loop <- downdb[grepl("http://", downdb)]
      for (i in loop) {
        destfile <- stringr::str_remove(i, "http://") %>% basename()
        if (!grepl(destfile, pattern = '.idx.gz$')) {
          download.file(url = i, destfile = paste0("./Annovar/humandb/", input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.gz"))
          R.utils::gunzip(paste0("./Annovar/humandb/", input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.gz"), remove = T, overwrite = T)
        }else {
          download.file(url = i, destfile = paste0("./Annovar/humandb/", input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.idx.gz"))
          R.utils::gunzip(paste0("./Annovar/humandb/", input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.idx.gz"), remove = T, overwrite = T)
        }
      }
    }
    df <- as.data.frame(dir("./Annovar/humandb"))
    })
})


output$variantanno_datebase_filename <- DT::renderDataTable({
   download_database()
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))

# annotation about Annovar -------------------------------------------------------------------------------------------------------------------
annotation_Annovar_list <- eventReactive(input$Annovar_section ,{
  withProgress(message = "processing", min = 0, max = 1, {
    cat(">> preparing use Annovar annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")

    if(file.exists(stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz"))){
      unlink(stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz"), recursive = T)
    }

    if(stringr::str_detect(input$input_file_anno$name, pattern = "gz$")){
      utils::untar(input$input_file_anno$datapath, exdir = ".")
    }else if(stringr::str_detect(input$input_file_anno$name, pattern = "zip$")){
      utils::unzip(input$input_file_anno$datapath, exdir = ".")
    }
    incProgress(0.2, detail = "uncompressing file...")
    input_dir <- stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz")

    file_split_name <- dir(input_dir, pattern = ".vcf|.vcf.gz") %>% stringr::str_remove(".vcf.gz|.vcf")
    input_file_name <- dir(input_dir, pattern = ".vcf|.vcf.gz", full.names = T)
    incProgress(0.4, detail = "annotation data...")
    vcf_list <- lapply(input_file_name, function(x){
        name <- paste0(getwd(), "/",stringr::str_remove(string = x, pattern = ".vcf.gz|.vcf"))
        system(paste0("perl ./Annovar/convert2annovar.pl ", "-format vcf4 -allsample -withfreq ", x , " -out ", name))
        system(paste0("perl ./Annovar/table_annovar.pl ", name, " ./Annovar/humandb/", " -buildver ", input$Annovar_species ," -out ", name,
                      " -remove -protocol ", paste(input$Annovar_protocol, collapse = ","), " -operation ",input$Annovar_opreation, " -nastring . "))
        df <- read.table(file = paste0(name, ".hg38_multianno.txt"), header = T, sep = "\t")
        if(!("chr" %in% df[1,1])){
          df[,1] <- paste0("chr", df[,1])}
        return(df)
      })
      unlink(input_dir, recursive = T)
      names(vcf_list) <- file_split_name
      incProgress(0.8, detail = "Data Visualization...")
      return(vcf_list)
    })
})

# annotation about VariantAnnotation----------------------------------------------------------------------------------------------------------------------
annotation_variantsAnno_list <- eventReactive(input$VariantAnnotation_section, {
  withProgress(message = "processing", min = 0, max = 1, {
    cat(">> preparing use Annovar annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")

    if(file.exists(stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz"))){
      unlink(stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz"), recursive = T)
    }

    if(stringr::str_detect(input$input_file_anno$name, pattern = "gz$")){
      utils::untar(input$input_file_anno$datapath, exdir = ".")
    }else if(stringr::str_detect(input$input_file_anno$name, pattern = "zip$")){
      utils::unzip(input$input_file_anno$datapath, exdir = ".")
    }
    incProgress(0.2, detail = "uncompressing file...")
    input_dir <- stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz")
    file_split_name <- dir(input_dir, pattern = ".vcf$|.vcf.gz$") %>% stringr::str_remove(".vcf.gz|.vcf")
    input_file_name <- dir(input_dir, pattern = ".vcf$|.vcf.gz$", full.names = T) %>% stringr::str_remove(".vcf.gz$|.vcf$")

    incProgress(0.4, detail = "annotation data...")
    annotation_list <- lapply(input_file_name, function(x){
      name <- paste0(x, ".vcf")
      if(!file.exists(name)){
        R.utils::gunzip(paste0(x, ".vcf.gz"), remove = T, overwrite = T)
      }

      df <- VariantAnnotation::readVcf(name, genome = input$Variantanno_species)
      if(!("chr1" %in% seqlevels(df))){
        seqlevels(df) <- paste("chr", seqlevels(df), sep = "")
      }
      seqlevels(df, pruning.mode="coarse") <- seqlevels(df)[seqlevels(df) %in% c("chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr2","chr20","chr21","chr22",
                                                                                 "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY" )]
      if(input$Variantanno_species == "hg38"){
        df <- VariantAnnotation::locateVariants(MatrixGenerics::rowRanges(df), TxDb.Hsapiens.UCSC.hg38.knownGene , VariantAnnotation::AllVariants())
      }else if(input$Variantanno_species == "hg19"){
        df <- VariantAnnotation::locateVariants(MatrixGenerics::rowRanges(df), TxDb.Hsapiens.UCSC.hg19.knownGene , VariantAnnotation::AllVariants())
      }
      list <- strsplit(names(GRanges(df)), split = "_|/",fixed = F)
      df$Ref <- sapply(list, "[",2)
      df$Alt <- sapply(list, "[",3)
      df <- as.data.frame(df, row.names = NULL)
      df <- df[!(is.na(df$GENEID)),]
      df$SYMBOLID <- AnnotationDbi::select(org.Hs.eg.db, keys = df$GENEID, keytype = "ENTREZID", columns = "SYMBOL")[,2]
      df <- df[, c("seqnames", "start", "end", "Ref", "Alt", "LOCATION", "SYMBOLID", "GENEID", "LOCSTART","LOCEND", "QUERYID", "TXID")]
      df <- df %>% dplyr::group_by(seqnames, start, end, Ref, Alt, LOCATION) %>% dplyr::summarise(SYMBOLID = paste(na.omit(unique(SYMBOLID)), collapse = ";"),
                                                                                          GENEID = paste(na.omit(unique(GENEID)), collapse = ";"),
                                                                                          LOCSTART = paste(na.omit(unique(LOCSTART)), collapse = ";"),
                                                                                          LOCEND = paste(na.omit(unique(LOCEND)), collapse = ";"),
                                                                                          QUERYID = paste(na.omit(unique(QUERYID)), collapse = ";"),
                                                                                          TXID = paste(na.omit(unique(TXID)), collapse = ";"))
      df <- df[df$SYMBOLID != "", ]
      return(df)
    })
  unlink(input_dir, recursive = T)
  names(annotation_list) <- file_split_name
  incProgress(0.8, detail = "Data Visualization...")
  return(annotation_list)
  })
})


#Variantannotation output----------------------------------------------------------------------------------------------------------------------------
output$VariantsAnno_anno_id <- renderUI({
  samples <- names(annotation_variantsAnno_list())
  selectInput("VariantsAnno_Anno_ID", "Select your samples to view:", choices = samples,  width = "40%")
})

output$variantanno_anno_table <- DT::renderDataTable({
  req(input$VariantsAnno_Anno_ID)
  annotation_variantsAnno_list()[[input$VariantsAnno_Anno_ID]]
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))


output$variantanno_tab_download <- downloadHandler(
  filename = function()  {paste0(input$VariantsAnno_Anno_ID,".txt")},
  content = function(file) {
    write.table(annotation_variantsAnno_list()[[input$VariantsAnno_Anno_ID]], file, sep = "\t", row.names = F)
})

#Annovar output-------------------------------------------------------------------------------------------------------------------------------------
output$Annovar_anno_id <- renderUI({
  samples <- names(annotation_Annovar_list())
  selectInput("Annovar_Anno_ID", "Select your samples to view:", choices = samples,  width = "40%")
})

output$Annovar_anno_table <- DT::renderDataTable({
  annotation_Annovar_list()[[input$Annovar_Anno_ID]]
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))


output$Annovar_tab_download <- downloadHandler(
  filename = function()  {paste0(input$Annovar_Anno_ID,".txt")},
  content = function(file) {
    write.table(annotation_Annovar_list()[[input$Annovar_Anno_ID]], file, sep = "\t", row.names = F)
})


