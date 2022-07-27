options(shiny.maxRequestSize=1000*1024^2)
source("./library.R")

server <- function(input,output,session){

#第一部分: 数据的读取  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
output$chrom_data <- renderUI({
  req(input$Species)
  if(input$Species == "others"){
    fileInput("chrom_files", "Input your circle chrom data:",multiple = F,width = "100%")
  }
})

#读取数据
ALL_variants_vcf <- eventReactive(input$sample_data, {
  withProgress(message = "processing", min = 0, max = 1, {
  req(input$use_example)
  print(input$use_example)

  if (input$use_example){
    vcf_example <- readRDS("../extdata/example_data.rds")
    return(vcf_example)
  }else{
      req(input$input_file)
      if(strsplit(input$input_file$name,".",fixed = T)[[1]][length(strsplit(input$input_file$name,".",fixed = T)[[1]])] == "gz"){
        untar(input$input_file$datapath, exdir = ".")
      }else if(strsplit(input$input_file$name,".",fixed = T)[[1]][length(strsplit(input$input_file$name,".",fixed = T)[[1]])] == "zip"){
        utils::unzip(input$input_file$datapath, exdir = ".")
      }
      incProgress(0.2, detail = "uncompressing file...")

      file_split_name <- strsplit(list.files(stringr::str_remove(input$input_file$name, pattern = ".gz|.zip|.tar.gz")), ".", fixed = T) #压缩包内部文件名字
      if(file_split_name[[1]][length(file_split_name[[1]])] == "gz"){
        file_path <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"), pattern = "*.gz$", full.name = T) #构建文件夹路径
        file_names <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"), pattern = "*.gz$") %>%　stringr::str_remove(".vcf.gz")#构建文件名
        ALL_df_List <- lapply(file_path, function(x){
          ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix)#读取vcf.gz并
        })
        names(ALL_df_List) <- (file_names)
      }else if(file_split_name[[1]][length(file_split_name[[1]])] == "vcf"){
        file_path <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.vcf$", full.name = T) #构建文件夹路径
        file_names <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.vcf$") %>% stringr::str_remove(".vcf") #构建去后缀后的文件名
        ALL_df_List <- lapply(file_path, function(x){
          ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix)#读取vcf.gz
        })
        names(ALL_df_List) <- (file_names)
      }else if(file_split_name[[1]][length(file_split_name[[1]])] == "txt"){
        file_path <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.txt$", full.name = T)#构建文件夹路径
        file_names <- dir(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.txt$") %>%　stringr::str_remove(".txt")#构建去后缀后的文件名
        ALL_df_List <- lapply(file_path, function(x){
          req( input$separator)
          ALL <- read.table(x, header = T, sep = input$separator)#读取txt.gz,并对Ref、Alt两列改名，方便后面使用
        })

        names(ALL_df_List) <- (file_names)
      }
      #删除解压的文件夹
      unlink(strsplit(input$input_file$name,".",fixed = T)[[1]][1], recursive = T)

      incProgress(0.4, detail = "loading file...")
      #对数据的其中两列改名
      ALL_df_List1 <- lapply(names(ALL_df_List),function(x){
        if("Start" %in% names(ALL_df_List[[x]]) & "End" %in% names(ALL_df_List[[x]])){
          df <- ALL_df_List[[x]]
          df %>% dplyr::rename("REF" = "Ref") %>% dplyr::rename("ALT" = "Alt")
        }else{
          ALL_df_List[[x]]
        }
      })
      names(ALL_df_List1) <- file_names
      #分割SNP、Indel 数据
      if("snp" %in% strsplit(names(ALL_df_List)[1],".",fixed = T)[[1]] | "indel" %in% strsplit(names(ALL_df_List)[1],".",fixed = T)[[1]]){ #检测是否已经分过SNP、Indel了
        return(ALL_df_List1)
      }else{
        ALL_df_List1.snp <- lapply((1:length(ALL_df_List1)),function(x){
          df <- ALL_df_List1[[x]]
          df <- df[nchar(df$REF) == nchar(df$ALT),]
        })
        names(ALL_df_List1.snp) <- paste(names(ALL_df_List),"snp", sep = ".")
        ALL_df_List1.indel <- lapply((1:length(ALL_df_List1)),function(x){
          df <- ALL_df_List1[[x]]
          df <- df[nchar(df$REF) != nchar(df$ALT),]
        })
        names(ALL_df_List1.indel) <- paste(names(ALL_df_List),"indel", sep = ".")
        ALL_df_List1 <- append(ALL_df_List1.indel,ALL_df_List1.snp)
        return(ALL_df_List1)
      }
  }
   })
})

  source("modules/1-1.update_server.R", local = TRUE)
  source("modules/1-2.summarise_barplot_server.R", local = TRUE)
  source("modules/1-3.snp_heatmap_server.R", local = TRUE)
  source("modules/1-4.snp_All_Type_server.R", local = TRUE)
  source("modules/1-5.Venn_server.R", local = TRUE)
  source("modules/1-6.circle_server.R", local = TRUE)
#第二部分//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  source("modules/2-1.Distribution_server.R", local = TRUE)
  source("modules/2-2.VariantsGene_Summarise_server.R", local = TRUE)
  source("modules/2-3.VariantsGene_Heatmap_server.R", local = TRUE)
  source("modules/2-4.VariantsGene_GO.KEGG_server.R", local = TRUE)
}






