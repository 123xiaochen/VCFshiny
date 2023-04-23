output$Download_Annovar_Database_warning <- renderUI({
  sendSweetAlert(session = session, title = "Warning", text = "If the current network environment is not good, there may be errors in the download. You can choose to re-download or download the annotation file from the Annovar official website to the Annovar/huamandb folder of the VCFshiny installation folder !", type = "warning")
})

output$Annovar_Download_Name <- renderUI({
  Annovar_database_table <- Annovar_database_table()
  selectInput(inputId = "Annovar_download_name", label = "Select Filter-based Datebase",
              choices = Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species &
                                               Annovar_database_table$Database_Type ==input$Annovar_download_database_type, "Name"] %>% unique(),
              selected = "cytoBand", width = "100%")
})

output$Annovar_anno_gene_Database <- renderUI({
  Annovar_database_table <- Annovar_database_table()
  if("-geneanno" %in% input$Annovar_anno_type){
    database_name <- dir(system.file("Annovar/humandb", package = "VCFshiny"))[grep(dir(system.file("Annovar/humandb", package = "VCFshiny")), pattern = paste0(input$Annovar_species,"(.*).txt$"))] %>%
      stringr::str_remove(pattern = paste0("^", input$Annovar_species,"_")) %>% stringr::str_remove(pattern = ".txt$") %>% unlist()
    database_name1 <- database_name[database_name %in% unique(Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species & Annovar_database_table$Database_Type == "gene-based", "Name"])]

    if (length(database_name) == 0 | length(database_name1) == 0) {
      sendSweetAlert(session = session, title = "Warning", text = "Please download Region-based datebase first !", type = "warning")
    }else{
      selectInput(inputId = "Annovar_gene_datebase", label = "Select Region-based Datebase:", choices = database_name1, width = "100%")
    }
  }
})

output$Annovar_anno_region_Database <- renderUI({
  Annovar_database_table <- Annovar_database_table()
  if("-regionanno" %in% input$Annovar_anno_type){
    database_name <- dir(system.file("Annovar/humandb", package = "VCFshiny"))[grep(dir(system.file("Annovar/humandb", package = "VCFshiny")), pattern = paste0(input$Annovar_species,"(.*).txt$"))] %>%
      stringr::str_remove(pattern = paste0("^", input$Annovar_species,"_")) %>% stringr::str_remove(pattern = ".txt$") %>% unlist()
    database_name1 <- database_name[database_name %in% unique(Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species & Annovar_database_table$Database_Type == "region-based", "Name"])]

    if (length(database_name) == 0 | length(database_name1) == 0) {
      sendSweetAlert(session = session, title = "Warning", text = "Please download Region-based datebase first !", type = "warning")
    }else{
      selectInput(inputId = "Annovar_region_datebase", label = "Select Region-based Datebase:", choices = database_name1, width = "100%")
    }
  }
})


output$Annovar_anno_Filter_Database <- renderUI({
  Annovar_database_table <- Annovar_database_table()
  if("-filter" %in% input$Annovar_anno_type){
    database_name <- dir(system.file("Annovar/humandb", package = "VCFshiny"))[grepl(dir(system.file("Annovar/humandb", package = "VCFshiny")), pattern =  paste0(input$Annovar_species,"(.*).txt$"))] %>%
      stringr::str_remove(pattern = paste0("^", input$Annovar_species,"_")) %>% stringr::str_remove(pattern = ".txt$") %>% unlist()
    database_name1 <- database_name[database_name %in% unique(Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species & Annovar_database_table$Database_Type == "filter-based", "Name"])]

    if (length(database_name) == 0 | length(database_name1) == 0) {
      sendSweetAlert(session = session, title = "Warning", text = "Please download Filter-based datebase first !", type = "warning")
    }else{
      selectInput(inputId = "Annovar_filter_datebase", label = "Select Filter-based Datebase:", choices = database_name1, width = "100%")
    }
  }
})

# Annovar Datebase Table ---------------------------------------------------------------------------------------------------------
Annovar_database_table <- reactive({
  Table <- read.csv(system.file("Annovar","Annovar_Datebase_Table.CSV", package = "VCFshiny"), header = T, sep = ",")
})


output$Annovar_Database_Tab <- DT::renderDataTable({
  Annovar_database_table()
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))


output$Annovar_Database_Tab_Download <- downloadHandler(
  filename = function()  {paste0(input$VariantsAnno_Anno_ID,".txt")},
  content = function(file) {
    write.table(Annovar_database_table(), file, sep = "\t", row.names = F)
})

# Download Annovar annotation database-------------------------------------------------------------------------------------------
download_database <- eventReactive(input$Annovar_download_section, {
  withProgress(message = "Downloading files ...", min = 0, max = 1, {
    cat(">>>  preparing datebase upload  ...\t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")

    ifelse(input$Annovar_download_name == "cytoBand", download_web <- "ucsc", download_web <- "annovar")
    downdb <- try(system2(command = "perl", args = paste0(system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -buildver ", input$Annovar_download_species,
                                                  " -webfrom ", download_web, " -downdb ", input$Annovar_download_name, " ", system.file("Annovar/humandb", package = "VCFshiny")),
                          stdout = T, stderr = T))
    options(timeout=10000000000000)
    if (grepl("http://", downdb) %>% any()) {
      loop <- downdb[grepl("http://", downdb)]
      for (i in loop) {
        destfile <- stringr::str_remove(i, "http://") %>% basename()
        if(grepl(destfile, pattern = 'Version.txt.gz$')){
          download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/", input$Annovar_download_species, "_", input$Annovar_download_name ,"Version", ".txt.gz"))
          # down_txt <- try(download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/", input$Annovar_download_species, "_", input$Annovar_download_name ,"Version", ".txt.gz")))
          # if(grepl("http://", down_txt) %>% any()){ sendSweetAlert(session = session, title = "Warning", text = paste0("Please download database from: ", down_txt[grepl("http://", down_txt)]), type = "warning")}
          R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name,"Version" ,".txt.gz"), package = "VCFshiny"),  overwrite = T)
        }else if (grepl(destfile, pattern = '.txt.gz$')) {
          download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/", input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.gz"))
          R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.gz"), package = "VCFshiny"),  overwrite = T)
        }else if(grepl(destfile, pattern = '.idx.gz$')){
          download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/",input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.idx.gz"))
          R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.idx.gz"), package = "VCFshiny"),  overwrite = T)
        }else if(grepl(destfile, pattern = '.fa.gz$')){
          download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/",input$Annovar_download_species, "_", input$Annovar_download_name,"Mrna.fa.gz"))
          R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name,"Mrna.fa.gz"), package = "VCFshiny"),  overwrite = T)
        }
      }
    }
    df <- as.data.frame(dir(system.file("Annovar/humandb", package = "VCFshiny")))
  })
})


output$Annovar_Download_Tab <- DT::renderDataTable({
  download_database()
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))


output$Annovar_Download_Tab_download <- downloadHandler(
  filename = function()  {paste0(input$VariantsAnno_Anno_ID,".txt")},
  content = function(file) {
    write.table(download_database(), file, sep = "\t", row.names = F)
  })
# annotation about Annovar -------------------------------------------------------------------------------------------------------------------
annotation_Annovar_list <- eventReactive(input$Annovar_section ,{
  withProgress(message = "processing", min = 0, max = 1, {
    cat(">> preparing use Annovar annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")

    if(is.null(input$input_file_anno)){
      sendSweetAlert(session = session, title = "Warning", text = "Please Upload data first !", type = "warning")
      return()
    }

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
      name <- paste0(getwd(), "/", stringr::str_remove(string = x, pattern = ".vcf.gz|.vcf"))
      cat(">> preparing use Annovar convert data ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
      system(paste0("perl ", system.file("Annovar","convert2annovar.pl", package = "VCFshiny"), " -format vcf4 -allsample -withfreq ", x , " -out ", name))

      cat(">> preparing use Annovar Gene-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
      system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -geneanno -dbtype ", input$Annovar_gene_datebase, " -out ", name,
                    " -buildver ", input$Annovar_species ," ", name, " ", system.file("Annovar/humandb", package = "VCFshiny")))
      df1 <- read.table(paste0(name, ".variant_function"), header = F, sep = "\t")[,1:7]
      colnames(df1) <- c("Func.refGene", "Gene.refGene", "Chr","Start","End","Ref","Alt")
      df1 <- df1[,c(3:7,1:2)]

      if(("-regionanno" %in% input$Annovar_anno_type) & !("-filter" %in% input$Annovar_anno_type)){
        cat(">> preparing use Annovar Region-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -regionanno -dbtype ", input$Annovar_region_datebase ," -out ", name,
                      " -buildver ", input$Annovar_species ," ", name ," ", system.file("Annovar/humandb", package = "VCFshiny")))
        df2 <- read.table(paste0(name, ".",input$Annovar_species, "_cytoBand"), header = F, sep = "\t")[,2:7]
        colnames(df2) <-  c("cytoBand", "Chr","Start","End","Ref","Alt")
        df <- dplyr::full_join(df1,df2, by = join_by(Chr, Start, End, Ref, Alt)) %>% na.omit()
        return(df)
      }else if(("-filter" %in% input$Annovar_anno_type) & !("-regionanno" %in% input$Annovar_anno_type)){
        cat(">> preparing use Annovar Filter-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -filter -dbtype ", input$Annovar_filter_datebase,
                      " -out ", name, " -buildver ", input$Annovar_species ," ",  name," ",  system.file("Annovar/humandb", package = "VCFshiny")))
        df3_filtered <- read.table(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_filtered"), header = F, sep = "\t")[,1:5]
        df3_dropped <- read.table(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_dropped"), header = F, sep = "\t")[,2:7]
        colnames(df3_dropped) <- c("datebase", "Chr","Start","End","Ref","Alt")
        df3_dropped <- df3_dropped %>% dplyr::mutate(datebase = as.character(datebase))
        colnames(df3_filtered) <- c("Chr","Start","End","Ref","Alt")
        df3_filtered$datebase <- "."
        df3_filtered <- df3_filtered %>% dplyr::select(datebase, everything())
        df3 <- rbind(df3_dropped, df3_filtered)
        colnames(df3)[1] <- input$Annovar_filter_datebase
        df <- dplyr::full_join(df1, df3, by = join_by(Chr, Start, End, Ref, Alt)) %>%  na.omit()
        return(df)
      }else{
        cat(">> preparing use Annovar Region-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -regionanno -dbtype ", input$Annovar_region_datebase ," -out ", name,
                      " -buildver ", input$Annovar_species ," ",  name," ",  system.file("Annovar/humandb", package = "VCFshiny")))
        df2 <- read.table(paste0(name, ".",input$Annovar_species, "_cytoBand"), header = F, sep = "\t")[,2:7]
        colnames(df2) <-  c("cytoBand", "Chr","Start","End","Ref","Alt")

        cat(">> preparing use Annovar Filter-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -filter -dbtype ", input$Annovar_filter_datebase,
                      " -out ", name, " -buildver ", input$Annovar_species ," ",  name," ",  system.file("Annovar/humandb", package = "VCFshiny")))
        df3_filtered <- read.table(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_filtered"), header = F, sep = "\t")[,1:5]
        df3_dropped <- read.table(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_dropped"), header = F, sep = "\t")[,2:7]
        colnames(df3_dropped) <- c("datebase", "Chr","Start","End","Ref","Alt")
        colnames(df3_filtered) <- c("Chr","Start","End","Ref","Alt")
        df3_filtered$datebase <- "."
        df3_filtered <- df3_filtered %>% dplyr::select(datebase, everything())
        df3 <- rbind(df3_dropped, df3_filtered)
        colnames(df3)[1] <- input$Annovar_filter_datebase

        df <- dplyr::full_join(df1,df2, by = join_by(Chr, Start, End, Ref, Alt)) %>% full_join(df3, by = join_by(Chr, Start, End, Ref, Alt)) %>% na.omit()
        return(df)
      }
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

    if(is.null(input$input_file_Variantanno)){
      sendSweetAlert(session = session, title = "Warning", text = "Please Upload data first !", type = "warning")
      return()
    }

    if(file.exists(stringr::str_remove(string = input$input_file_Variantanno$name, pattern = ".gz|.zip|.tar.gz"))){
      unlink(stringr::str_remove(string = input$input_file_Variantanno$name, pattern = ".gz|.zip|.tar.gz"), recursive = T)
    }

    if(stringr::str_detect(input$input_file_Variantanno$name, pattern = "gz$")){
      utils::untar(input$input_file_Variantanno$datapath, exdir = ".")
    }else if(stringr::str_detect(input$input_file_Variantanno$name, pattern = "zip$")){
      utils::unzip(input$input_file_Variantanno$datapath, exdir = ".")
    }
    incProgress(0.2, detail = "uncompressing file...")
    input_dir <- stringr::str_remove(string = input$input_file_Variantanno$name, pattern = ".gz|.zip|.tar.gz")
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
  selectInput("VariantsAnno_Anno_ID", "Select your samples to view:", choices = samples,  width = "100%")
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


