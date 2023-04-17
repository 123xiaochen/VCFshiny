# 第一部分 ：数据的读入  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#读取数据-------------------------------
raw_variants_list <- eventReactive(input$sample_data, {
  withProgress(message = "processing", min = 0, max = 1, {
    cat(">> preparing data upload ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
    req(input$use_example, input$sample_data)
    if(file.exists(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"))){
      unlink(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"), unlink = T)
    }
    if (input$use_example == "TRUE") {
      vcf_example <- readRDS(system.file("extdata", "example_data.rds", package = "VCFshiny"))
      return(vcf_example)
    }else if(input$use_example == "FALSE"){
      req(input$input_file, input$sample_data)
      if(stringr::str_detect(input$input_file$name, pattern = "gz$")){
        utils::untar(input$input_file$datapath, exdir = ".")
      }else if(stringr::str_detect(input$input_file$name, pattern = "zip$")){
        utils::unzip(input$input_file$datapath, exdir = ".")
      }

      incProgress(0.2, detail = "uncompressing file...")
      input_dir <- stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz")
      file_split_name <- strsplit(list.files(input_dir), ".", fixed = T)

      #读入文件
      if(file_split_name[[1]][length(file_split_name[[1]])] == "gz"){
        file_path <- dir(input_dir, pattern = "*.gz$", full.name = T) #构建文件夹路径
        file_names <- dir(input_dir, pattern = "*.gz$") %>%　stringr::str_remove(".vcf.gz")#构建文件名
        ALL_df_List <- lapply(file_path, function(x){ ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix) })
        names(ALL_df_List) <- (file_names)
      }else if(file_split_name[[1]][length(file_split_name[[1]])] == "vcf"){
        file_path <- dir(input_dir, pattern = "*.vcf$", full.name = T) #构建文件夹路径
        file_names <- dir(input_dir, pattern = "*.vcf$") %>% stringr::str_remove(".vcf") #构建去后缀后的文件名
        ALL_df_List <- lapply(file_path, function(x){ ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix) })
        names(ALL_df_List) <- (file_names)
      }else if(file_split_name[[1]][length(file_split_name[[1]])] == "txt"){
        file_path <- dir(input_dir, pattern = "*.txt$", full.name = T)#构建文件夹路径
        file_names <- dir(input_dir, pattern = "*.txt$") %>%　stringr::str_remove(".txt")#构建去后缀后的文件名
        ALL_df_List <- lapply(file_path, function(x){ ; ALL <- read.table(x, header = T, sep = "\t")})
        names(ALL_df_List) <- (file_names)
      }

      unlink(strsplit(input$input_file$name,".",fixed = T)[[1]][1], recursive = T)
      cat(">> preparing data classification ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
      incProgress(0.3, detail = "loading file...")

      #提取Ensembl-VEP注释结果
      if(stringr::str_detect(ALL_df_List[[1]][1,ncol(ALL_df_List[[1]])], pattern = "CSQ=")){
        cl <- parallel::makeCluster(4)
        ALL_List <- parallel::parLapply(cl, 1:length(ALL_df_List), function(x){
          Select_df <- lapply(1:nrow(ALL_df_List[[x]]), function(y){
            data <- ALL_df_List[[x]]$INFO[y] %>% strsplit("\\|") %>% unlist()
            number <- seq(1, length(data), 38)
            df <- data.frame(ALL_df_List[[x]][y, 1:5],
                             consequence = paste(na.omit(unique(data[number + 1][data[number + 1] != ""])), collapse = ';'),
                             symbol = paste(na.omit(unique(data[number + 3][data[number + 3] != ""])), collapse = ';'))
          }) %>% dplyr::bind_rows()
          return(Select_df)
        })
        parallel::stopCluster(cl)
        names(ALL_List) <- names(ALL_df_List)
        ALL_df_List <- ALL_List
      }
      incProgress(0.4, detail = "displaying file...")
      #分割SNP、Indel 数据
      if("snp" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]] |
         "indel" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]]){ #检测是否已经分过SNP、Indel了
        return(ALL_df_List)
      }else{
        snp_list <- lapply(ALL_df_List, function(x){
          df <- x[nchar(x[, 4]) == nchar(x[, 5]), ]
        })
        names(snp_list) <- paste(names(ALL_df_List), "snp", sep = ".")

        indel_list <- lapply(ALL_df_List, function(x){
          df <- x[nchar(x[, 4]) != nchar(x[, 5]), ]
        })
        names(indel_list) <- paste(names(ALL_df_List),"indel", sep = ".")
        raw_variant_list <- append(indel_list, snp_list)
        return(raw_variant_list)
      }

    }
  })
})



#对读取数据根据group交互选择---------------------------------------------------------------------------
output$sample_id <- renderUI({
  samples <- names(raw_variants_list())
  selectInput("sampleID", "Select your samples to view:", choices = samples,  width = "40%")
})

#对读取数据根据group交互选择后并进行显示
output$raw_sample <- DT::renderDataTable({
  req(input$sampleID)
  raw_variants_list()[[input$sampleID]]
}, rownames = T, options = list(scrollX = TRUE, pageLength =5))
