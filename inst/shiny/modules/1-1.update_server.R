# 第一部分 ：数据的读入  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#数据的读入以及显示 --------------------------------------------------------------------------------------------
#构建选择分组进行展示控件

output$chrom_data <- renderUI({
  req(input$Species)
  if(input$Species == "others"){
    fileInput("chrom_files", "Input your circle chrom data:", multiple = F, width = "100%")
  }
})

#读取数据-------------------------------
# ALL_variants_vcf <- eventReactive(input$sample_data, {
#   withProgress(message = "processing", min = 0, max = 1, {
#     req(input$use_example, input$sample_data)
#     if (input$use_example) {
#       vcf_example <- readRDS("../extdata/example_data.rds")
#       return(vcf_example)
#     }else {
#       req(input$input_file, input$sample_data)
#       if(strsplit(input$input_file$name,".",fixed = T)[[1]][length(strsplit(input$input_file$name,".",fixed = T)[[1]])] == "gz"){
#         untar(input$input_file$datapath, exdir = ".")
#       }else if(strsplit(input$input_file$name,".",fixed = T)[[1]][length(strsplit(input$input_file$name,".",fixed = T)[[1]])] == "zip"){
#         utils::unzip(input$input_file$datapath, exdir = ".")
#       }
#       incProgress(0.2, detail = "uncompressing file...")
#
#       file_split_name <- strsplit(list.files(stringr::str_remove(input$input_file$name, pattern = ".gz|.zip|.tar.gz")), ".", fixed = T) #压缩包内部文件名字
#
#       if(file_split_name[[1]][length(file_split_name[[1]])] == "gz"){
#         file_path <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"), pattern = "*.gz$", full.name = T) #构建文件夹路径
#         file_names <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"), pattern = "*.gz$") %>%　stringr::str_remove(".vcf.gz")#构建文件名
#         ALL_df_List <- lapply(file_path, function(x){
#           ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix)#读取vcf.gz
#         })
#         names(ALL_df_List) <- (file_names)
#       }else if(file_split_name[[1]][length(file_split_name[[1]])] == "vcf"){
#         file_path <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.vcf$", full.name = T) #构建文件夹路径
#         file_names <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.vcf$") %>% stringr::str_remove(".vcf") #构建去后缀后的文件名
#         ALL_df_List <- lapply(file_path, function(x){
#           ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix)#读取vcf
#         })
#         names(ALL_df_List) <- (file_names)
#       }else if(file_split_name[[1]][length(file_split_name[[1]])] == "txt"){
#         file_path <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.txt$", full.name = T)#构建文件夹路径
#         file_names <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.txt$") %>%　stringr::str_remove(".txt")#构建去后缀后的文件名
#         ALL_df_List <- lapply(file_path, function(x){
#           req( input$separator)
#           ALL <- read.table(x, header = T, sep = input$separator)#读取txt
#         })
#         names(ALL_df_List) <- (file_names)
#       }
#       #print(input$input_file)
#       incProgress(0.4, detail = "loading file...")
#
#       #对数据的其中两列改名，方便统一处理数据
#       ALL_df_List1 <- lapply(names(ALL_df_List),function(x){
#         if("Start" %in% names(ALL_df_List[[x]]) & "End" %in% names(ALL_df_List[[x]])){
#           df <- ALL_df_List[[x]]
#           df %>% dplyr::rename("REF" = "Ref") %>% dplyr::rename("ALT" = "Alt")
#         }else{
#           ALL_df_List[[x]]
#         }
#       })
#       names(ALL_df_List1) <- file_names
#       #分割SNP、Indel 数据
#       if("snp" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]] | "indel" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]]){ #检测是否已经分过SNP、Indel了
#         return(ALL_df_List1)
#       }else{
#         ALL_df_List1.snp <- lapply((1:length(ALL_df_List1)), function(x){
#           df <- ALL_df_List1[[x]]
#           df <- df[nchar(df$REF) == nchar(df$ALT), ]
#         })
#         names(ALL_df_List1.snp) <- paste(names(ALL_df_List),"snp", sep = ".")
#
#         ALL_df_List1.indel <- lapply((1:length(ALL_df_List1)), function(x){
#           df <- ALL_df_List1[[x]]
#           df <- df[nchar(df$REF) != nchar(df$ALT), ]
#         })
#         names(ALL_df_List1.indel) <- paste(names(ALL_df_List),"indel", sep = ".")
#
#         ALL_df_List1 <- append(ALL_df_List1.indel,ALL_df_List1.snp)
#
#         return(ALL_df_List1)
#       }
#     }
#   })
# })
#



#对读取数据根据group交互选择--------------
output$sample_id <- renderUI({
  samples <- names(ALL_variants_vcf())
  selectInput("sampleID", "Select your samples to view:", choices = samples, width = "40%")
})

#对读取数据根据group交互选择后并进行显示
output$sample <- DT::renderDataTable({
  ALL_variants_vcf <- ALL_variants_vcf()
  return(ALL_variants_vcf[[input$sampleID]])
}, rownames = T, options = list(scrollX = TRUE, pageLength = 5))

#SNP数据筛选
SNP_data <- reactive({
  SNP_ALL_list <- lapply(stringr::str_subset(names(ALL_variants_vcf()),pattern = ".snp",negate = F),function(x){
    SNP_ALL <- ALL_variants_vcf()[[x]]
  })
  names(SNP_ALL_list) <- stringr::str_subset(names(ALL_variants_vcf()),pattern = ".snp",negate = F)
  return(SNP_ALL_list)
})
