options(shiny.maxRequestSize=1000*1024^2)
source("./library.R")

server <- function(input,output,session){
  
# 第一部分 ：数据的读入  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  #数据的读入以及显示 --------------------------------------------------------------------------------------------
  #构建选择分组进行展示控件
   output$chrom_data <- renderUI({
    req(input$Species)
    if(input$Species == "others"){
      fileInput("chrom_files", "Input your circle chrom data:",multiple = F,width = "100%")
    }
  })
  #读取数据
  ALL_variants_vcf <- eventReactive(input$sample_data,{
    withProgress(message = "processing", min = 0, max = 1, {
      req(input$file1)
      if(strsplit(input$file1$name,".",fixed = T)[[1]][length(strsplit(input$file1$name,".",fixed = T)[[1]])] == "gz"){
        untar(input$file1$datapath, exdir = ".")
      }else if(strsplit(input$file1$name,".",fixed = T)[[1]][length(strsplit(input$file1$name,".",fixed = T)[[1]])] == "zip"){
        utils::unzip(input$file1$datapath, exdir = ".")
      }
      incProgress(0.2, detail = "uncompressing file...")

      file_split_name <- strsplit(list.files(stringr::str_remove(input$file1$name, pattern = ".gz|.zip|.tar.gz")),".",fixed = T)#压缩包内部文件名字
      if(file_split_name[[1]][length(file_split_name[[1]])] == "gz"){
        file_path <- dir(stringr::str_remove(string = input$file1$name, pattern = ".gz|.zip|.tar.gz"), pattern = "*.gz$", full.name = T)#构建文件夹路径
        file_names <- dir(stringr::str_remove(string = input$file1$name, pattern = ".gz|.zip|.tar.gz"), pattern = "*.gz$") %>%　stringr::str_remove(".vcf.gz")#构建文件名
        ALL_df_List <- lapply(file_path, function(x){
          ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix)#读取vcf.gz并
        })
        names(ALL_df_List) <- (file_names)
      }else if(file_split_name[[1]][length(file_split_name[[1]])] == "vcf"){
        file_path <- dir(stringr::str_remove(string = input$file1$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.vcf$", full.name = T)#构建文件夹路径
        file_names <- dir(stringr::str_remove(string = input$file1$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.vcf$") %>%　stringr::str_remove(".vcf")#构建去后缀后的文件名
        ALL_df_List <- lapply(file_path, function(x){
          ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix)#读取vcf.gz
        })
        names(ALL_df_List) <- (file_names)
      }else if(file_split_name[[1]][length(file_split_name[[1]])] == "txt"){
        file_path <- dir(stringr::str_remove(string = input$file1$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.txt$", full.name = T)#构建文件夹路径
        file_names <- dir(stringr::str_remove(string = input$file1$name, pattern = ".gz|.zip|tar.gz"), pattern = "*.txt$") %>%　stringr::str_remove(".txt")#构建去后缀后的文件名
        ALL_df_List <- lapply(file_path, function(x){
          req( input$separator)
          ALL <- read.table(x, header = T, sep = input$separator)#读取txt.gz,并对Ref、Alt两列改名，方便后面使用
        })
        names(ALL_df_List) <- (file_names)
      }
      print(input$file1)
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
    if("snp" %in% strsplit(names(ALL_df_List)[1],".",fixed = T)[[1]] | "indel" %in% strsplit(names(ALL_df_List)[1],".",fixed = T)[[1]]){   #检测是否已经分过SNP、Indel了
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
    })
  })


  #对读取数据根据group交互选择
  output$sample_id <- renderUI({
    samples <- names(ALL_variants_vcf())
    selectInput("sampleID", "Select your samples to view:", choices = samples, width = "30%")
  })

  #对读取数据根据group交互选择后并进行显示
  output$sample <- DT::renderDataTable({
    ALL_variants_vcf <- ALL_variants_vcf()
    return(ALL_variants_vcf[[input$sampleID]])
  }, rownames = T, options = list(scrollX = TRUE))

  #SNP数据筛选
  SNP_data <- reactive({
    SNP_ALL_list <- lapply(stringr::str_subset(names(ALL_variants_vcf()),pattern = ".snp",negate = F),function(x){
      SNP_ALL <- ALL_variants_vcf()[[x]]
    })
    names(SNP_ALL_list) <- stringr::str_subset(names(ALL_variants_vcf()),pattern = ".snp",negate = F)
    return(SNP_ALL_list)
  })

#第二部分: 数据处理以及作图  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  #1、SNP+Indel突变柱状图 --------------------------------------------------------------------------------------------------------------------------
  
    #1.1 、 对输入数据进行数据总结并显示
  ALL_data <- reactive({
    ALL_Variants_summary <- data.frame()
    for(i in 1:length(ALL_variants_vcf())){
      SNV_numbers = dim(ALL_variants_vcf()[[i]])[1]
      samples = strsplit(names(ALL_variants_vcf())[i], ".",fixed = T)[[1]][1]
      group =  gsub("-[0-9]$|[0-9]$","",samples)
      type = strsplit(names(ALL_variants_vcf())[i], ".",fixed = T)[[1]][2]
      ALL_Variants_summary <- rbind(ALL_Variants_summary,data.frame(group,samples,type,SNV_numbers))
    }
    return(ALL_Variants_summary)
  })
  
  observeEvent(input$Data_summarise_plot,{
    output$information <- DT::renderDataTable(
      return(ALL_data()),extensions ="Buttons",
      server =  as.logical(input$ALL_SNP_INDEL_data_Set),
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = list('copy','print',list(
          extend = "collection",
          buttons = c('csv','excel','pdf'),
          text = 'Download'
        )
        )
      )
    )
  })
  
    #1.2 、 构建选择参考组控件
  output$Summariseplot_stat.test.group <- renderUI({
    if(!is.null(ALL_variants_vcf())){
      stat.test.group <- unique(ALL_data()$group)
      selectInput("Summariseplot_stat.test.GROUP","Select your reference group:" , choices = stat.test.group, width = "100%")
    }
  })

  
    #1.3 、 绘制SNP+Indel突变柱状图
  Data_SummarisePlot <- eventReactive(input$Data_summarise_plot,{
    req(input$Summariseplot_stat.test.GROUP,input$text_size,input$bar_width,input$error_bar_width,input$error_bar_size,
        input$brack.size,input$tip.length,input$jitter_size,input$jitter_width, input$Summariseplot_label)

    stat.test <- ALL_data() %>% group_by(type) %>% rstatix::pairwise_t_test(SNV_numbers ~ group) %>% rstatix::add_significance()
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "group")
    stat.test <- stat.test[stat.test$group1 == input$Summariseplot_stat.test.GROUP, ]
    #选择WT为对照

    ggpubr::ggbarplot(ALL_data(),x = "group",y =  "SNV_numbers" , facet.by = "type" , palette = "Set1",label = as.logical(input$Summariseplot_label),
                      fill = "group" , add = "mean_sd" ,  width = input$bar_width , position = position_dodge(),
                      add.params = list(width = input$error_bar_width ,size = input$error_bar_size), ggtheme = theme_classic())+
      ggpubr::stat_pvalue_manual(stat.test, bracket.size = input$brack.size, tip.length = input$tip.length, bracket.nudge.y = 0)+
      scale_alpha_continuous(expand = expansion(mult = c(0.1, 0.1)))+
      facet_wrap( ~ type, ncol = 4, scales = "free")+
      xlab(NULL)+ylab("WGS Total SNV numbers")+
      theme(text = element_text(face = "bold", family = "Times", size = input$text_size, color = "black"), legend.position = "none",
            strip.background = element_blank())+
      geom_jitter(size = input$jitter_size,width = input$jitter_width)
  })
  
  output$SummarisePlot <- renderPlot({
    Data_SummarisePlot()
  })

  output$summarise_plot <- downloadHandler(
    filename = function(){
      paste("Summarise_plot",input$summarise_down_filetype,sep = ".")
    },
    content = function(file){
      if(input$summarise_down_filetype == "pdf"){
        pdf(file,width = input$summarise_download_width,height = input$summarise_download_height)
      }else if(input$summarise_down_filetype == "png"){
        png(file,width = input$summarise_download_width,height = input$summarise_download_height)
      }else{
        jpeg(file,width = input$summarise_download_width,height = input$summarise_download_height)
      }
      print(Data_SummarisePlot())
      dev.off()
    }
  )


  #2 、SNP 全突变热图 -------------------------------------------------------------------------------------------------------------------------
  
    #2.1 、构建突变热图作图表格并构建下载控件
  
  # output$Heatmap_COLOR <- renderUI({
  #   req(input$heatmap_color)
  #   div(
  #     dropdown(
  #       label = "Set Colors", width = "100%",  right = T, icon = icon("cog", lib = "glyphicon"),
  #         if(isTRUE(input$heatmap_color)){
  #           tags$div(
  #             textInput("Heatmap_Low_Color", "Please Input Your Low Color:",width = "100%"),
  #             textInput("Heatmap_High_Color", "Please Input Your High Color:", width = "100%")
  #           )
  #         }else{
  #           tags$div(
  #             input$Heatmap_Low_Color = "white", input$Heatmap_High_Color = "#2A71AF"
  #           )
  #         }
  #     )
  #   )
  # })
  
  SNP_heatmap_Data <-reactive({
    SNP_heatmap_data <- data.frame()
    for(x in 1:length(SNP_data())){
      seq = c("A","T","C","G")
      for(y in seq){
        for(z in seq){
          SNP_numbers <- sum(SNP_data()[[x]]$REF == y & SNP_data()[[x]]$ALT == z)
          REF <- y
          ALT <- z
          samples <- strsplit(names(SNP_data())[x], ".",fixed = T)[[1]][1]
          group <- gsub("-[0-9]$|[0-9]$","",samples)
          freq <- SNP_numbers / dim(SNP_data()[[x]])[1]
          SNP_heatmap_data <- rbind(SNP_heatmap_data,data.frame(samples,group,REF,ALT,SNP_numbers,freq))
        }
      }
    }
    return(SNP_heatmap_data)
  })
  
  
  observeEvent(input$Heatplot,{
    output$snp_heatmap <- DT::renderDataTable(
      return(SNP_heatmap_Data()),extensions ="Buttons",
      server =  as.logical(input$Heatmap_data_Set),
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = list('copy','print',list(
          extend = "collection",
          buttons = c('csv','excel','pdf'),
          text = 'Download'
        )
        )
      ))
  })
  
    # 2.2 、绘制热图并构建下载图片控制下载图片大小控件
  Heatmapplot <- eventReactive(input$Heatplot,{
    req(input$heatmap_digits,input$heatmap_ncol,input$heatmap_text.size)
    SNP_data <- SNP_heatmap_Data() %>% group_by(group,REF,ALT) %>% summarise_if(is.numeric,mean)
    SNP_data$freq <- SNP_data$freq*100
    
    
    
    
    ggplot2::ggplot(SNP_data,aes(ALT,REF,fill = freq))+
      geom_tile()+
      geom_text(aes(label = round(freq, digits = input$heatmap_digits)))+
      facet_wrap(~group,ncol = input$heatmap_ncol)+
      scale_fill_gradient(low = "white" , high = "#2A71AF")+
      theme_test()+
      theme(text = element_text(face = "bold", family = "Times", size = input$heatmap_text.size, color = "black"))
  })

  output$Heat_Plot <- renderPlot({
    Heatmapplot()
  })

  output$heatmap_plot <- downloadHandler(
    filename = function(){
      paste("Heatmap_plot", input$heatmap_down_filetype, sep = ".")
    },
    content = function(file){
      if(input$heatmap_down_filetype == "pdf"){
        pdf(file, width = input$heatmap_download_width, height = input$heatmap_download_height)
      }else if(input$heatmap_down_filetype == "png"){
        png(file, width = input$heatmap_download_width, height = input$heatmap_download_height)
      }else{
        jpeg(file, width = input$heatmap_download_width, height = input$heatmap_download_height)
      }
      print(Heatmapplot())
      dev.off()
    }
  )

  # 3、SNP各个变异类型统计柱状图 -------------------------------------------------------------------------------------------------------------------------
    # 3.1 、给数据加一列type ，构建SNP各个变异类型统计数据
  SNP_type_data <- reactive({
    SNP_ACTG_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "A" & SNP_heatmap_Data()$ALT == "C")| (SNP_heatmap_Data()$REF == "T" & SNP_heatmap_Data()$ALT == "G"),]
    SNP_ACTG_data$type <- "A>C/T>G"
    SNP_AGTC_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "A" & SNP_heatmap_Data()$ALT == "G")| (SNP_heatmap_Data()$REF == "T" & SNP_heatmap_Data()$ALT == "C"),]
    SNP_AGTC_data$type <- "A>G/T>C"
    SNP_ATTA_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "A" & SNP_heatmap_Data()$ALT == "T")| (SNP_heatmap_Data()$REF == "T" & SNP_heatmap_Data()$ALT == "A"),]
    SNP_ATTA_data$type <- "A>T/T>A"
    SNP_CAGT_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "C" & SNP_heatmap_Data()$ALT == "A")| (SNP_heatmap_Data()$REF == "G" & SNP_heatmap_Data()$ALT == "T"),]
    SNP_CAGT_data$type <- "C>A/G>T"
    SNP_CGGC_Data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "C" & SNP_heatmap_Data()$ALT == "G")| (SNP_heatmap_Data()$REF == "G" & SNP_heatmap_Data()$ALT == "C"),]
    SNP_CGGC_Data$type <- "C>G/G>C"
    SNP_CTGA_data <- SNP_heatmap_Data()[(SNP_heatmap_Data()$REF == "C" & SNP_heatmap_Data()$ALT == "T")| (SNP_heatmap_Data()$REF == "G" & SNP_heatmap_Data()$ALT == "A"),]
    SNP_CTGA_data$type <- "C>T/G>A"
    SNP_type_data<- rbind(SNP_ACTG_data,SNP_AGTC_data,SNP_ATTA_data,SNP_CAGT_data,SNP_CGGC_Data,SNP_CTGA_data)

    SNP_type_data <- SNP_type_data %>% group_by(type,group,samples) %>% summarise_if(is.numeric,sum)

    return(SNP_type_data)
  })
  
  
  observeEvent(input$ALL_Variantsplot,{
    output$SNP_ALL_variants_data <- DT::renderDataTable(
      return(SNP_type_data()),extensions ="Buttons",
      server =  as.logical(input$ALL_Variants_data_Set),
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = list('copy','print',list(
          extend = "collection",
          buttons = c('csv','excel','pdf'),
          text = 'Download'
        )
        ))
      )
  })
    # 3.2、 绘制SNP各个变异类型统计柱状图
  ALL_Variants_plot <- eventReactive(input$ALL_Variantsplot,{
    req(input$ALL_Variants_plot_Text_size,input$ALL_Variants_plot_bar.width,input$ALL_Variants_plot_Error_bar.width,input$ALL_Variants_plot_Error_bar.size, input$ALL_Variants_plot_label)
    SNP_type_data <- SNP_type_data()
    stat.test1 <- SNP_type_data %>% group_by(type) %>% rstatix::pairwise_t_test(freq~ group) %>% rstatix::add_significance()
    stat.test1 <- stat.test1 %>% rstatix::add_xy_position(x = "type")
    stat.test1 <- stat.test1[stat.test1$group1 == "WT",]
    ggpubr::ggbarplot(SNP_type_data,x = "type",y = "freq", palette = "Set1", label = as.logical(input$ALL_Variants_plot_label), lab.nb.digits = input$ALL_Variants_plot_label_digits,
                      add = "mean_sd",fill = "group",width = input$ALL_Variants_plot_bar.width, position = position_dodge(),
                      add.params = list(width = input$ALL_Variants_plot_Error_bar.width, size = input$ALL_Variants_plot_Error_bar.size), ggtheme = theme_classic())+
      stat_pvalue_manual(stat.test1, bracket.size = 0.5,tip.length = 0.01,bracket.nudge.y = 0)+
      xlab(NULL)+ylab("Freq of different types of SNVs versus total SNVs")+
      theme(text = element_text(face = "bold",family = "Times",size = input$ALL_Variants_plot_Text_size,color = "black"),
            strip.background = element_blank())
  })

  output$SNP_type_plot <- renderPlot({
    ALL_Variants_plot()
  })

  output$ALL_Variants_plot <- downloadHandler(
    filename = function(){
      paste("Heatmap_plot",input$ALL_Variants_down_filetype,sep = ".")
    },
    content = function(file){
      if(input$ALL_Variants_down_filetype == "pdf"){
        pdf(file,width = input$ALL_Variants_download_width,height = input$ALL_Variants_download_height)
      }else if(input$ALL_Variants_down_filetype == "png"){
        png(file,width = input$ALL_Variants_download_width,height = input$ALL_Variants_download_height)
      }else{
        jpeg(file,width = input$ALL_Variants_download_width,height = input$ALL_Variants_download_height)
      }
      print(ALL_Variants_plot())
      dev.off()
    }
  )
  
  #4、韦恩图（为做圈图取重复样本的交叉数据）---------------------------------------------------------------------------------------------------------------
  
    #4.1、添加行名
  venn_ALL_data <- reactive({
    ALL_variants_vcf <- ALL_variants_vcf()
    venn_ALL_data <- lapply(names(ALL_variants_vcf),function(x){
      if("POS" %in% (names(ALL_variants_vcf[[x]]))){        #判断是哪种vcf文件
        rownames(ALL_variants_vcf[[x]]) <- paste(ALL_variants_vcf[[x]]$CHROM,ALL_variants_vcf[[x]]$POS,ALL_variants_vcf[[x]]$REF,ALL_variants_vcf[[x]]$ALT,sep = "_")
        ALL_variants_vcf[[x]]
      }else if("Start" %in% (names(ALL_variants_vcf[[x]])) & "End" %in% (names(ALL_variants_vcf[[x]]))){
        rownames(ALL_variants_vcf[[x]]) <-
          paste(ALL_variants_vcf[[x]]$Chr, ALL_variants_vcf[[x]]$Start, ALL_variants_vcf[[x]]$End, ALL_variants_vcf[[x]]$REF, ALL_variants_vcf[[x]]$ALT,sep = "_")
        ALL_variants_vcf[[x]]
      }
    })
    names(venn_ALL_data) <- names(ALL_variants_vcf)
    #print("添加行名OK")
    return(venn_ALL_data)
  })
  
  
  
  
  
  
    #4.2 、根据输入筛选数据并作Venn图
  
  output$venn_group_id <- renderUI({
    if(!is.null(ALL_variants_vcf())){
      group <- stringr::str_remove(names(ALL_variants_vcf()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
      selectInput("venn_group_ID","Select your group to analyse:", choices = group)
    }
  })
  
  VENN_plot <- eventReactive(input$venn_star,{
    req(input$venn_group_ID, input$venn_type_id, input$venn_box, input$venn_ellipse, input$venn_ilcs, input$venn_sncs)
    
    venn_ALL_data <- venn_ALL_data()
    samples <- stringr::str_subset(names(venn_ALL_data),pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
    venn_list <- lapply(samples, function(x){
      venn_ALL_data[[x]] %>% row.names()
    })
    #print(input$venn_sncs)
    #print("venn数据准备OK")
    names(venn_list) <- samples
    venn::venn(venn_list, zcolor = "style", box = as.logical(input$venn_box),
               ellipse = as.logical(input$venn_ellipse),ilcs = (input$venn_ilcs), sncs = (input$venn_sncs)
    )
  })
  
  output$Display_venn_plot <- renderPlot({
    VENN_plot()
  })
  
  output$Venn_download <- downloadHandler(
    req(input$VennPlot_down_filetype,input$VennPlot_download_width,input$VennPlot_download_height),
    filename = function(){
      paste("venn_plot", input$VennPlot_down_filetype, sep = ".")
    },
    content = function(file){
      if(input$VennPlot_down_filetype == "pdf"){
        pdf(file, width = input$VennPlot_download_width, height = input$VennPlot_download_height)
      }else if(input$VennPlot_down_filetype == "png"){
        png(file, width = input$VennPlot_download_width, height = input$VennPlot_download_height)
      }else{
        jpeg(file, width = input$VennPlot_download_width, height = input$VennPlot_download_height)
      }
      print(VENN_plot())
      dev.off()
    }
  )
  
    #4.3 、提取对应的交叉数据并展示
  venn_table <- reactive({
    req(input$venn_group_ID, input$venn_type_id)
    samples <- stringr::str_subset(names(venn_ALL_data()),pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
    
    venn_binded <- lapply(samples, function(x){
      df <- ALL_variants_vcf()[[x]]
      return(df)
    }) %>% bind_rows()
    
    
    if("POS" %in% (names(venn_binded))){
      venn_binded$Merge_Name <- paste(venn_binded$CHROM, venn_binded$POS, venn_binded$REF, venn_binded$ALT,sep = "_")
    }else if("Start" %in% (names(venn_binded)) & "End" %in% (names(venn_binded))){
      venn_binded$Merge_Name <- paste(venn_binded$Chr, venn_binded$Start, venn_binded$End, venn_binded$REF, venn_binded$ALT,sep = "_")
    }
    duplicated_row <- (venn_binded$Merge_Name %>% table() %>% as.data.frame() %>% dplyr::filter(Freq == max(Freq)))[,1]
    duplicated_row <- as.character(duplicated_row)
    venn_binded <- venn_binded[(venn_binded$Merge_Name %in% duplicated_row) , ] %>% unique()
    venn_binded <- subset(venn_binded,select = -c(Merge_Name))
    return(venn_binded)
  })
  
  observeEvent(input$venn_star, {
    output$venn_Table <- DT::renderDataTable(
      return(venn_table()) , extensions ="Buttons",
      server =  as.logical(input$Venn_data_Set),
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = list('copy','print',list(
          extend = "collection",
          buttons = c('csv','excel','pdf'),
          text = 'Download'))
      )
    )
  })
  
  
    #4.4 、 提取所有交叉数据（圈图准备数据）
  
  circle_table <- reactive({
    req(input$circle_type_ID)
    group_table <- lapply((stringr::str_remove(names(venn_ALL_data()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()),function(x){
      sample <- stringr::str_subset(names(venn_ALL_data()),pattern = x, negate = F) %>% stringr::str_subset(pattern = input$circle_type_ID, negate = F)
      circle_data <- lapply(sample,function(y){
        df <- ALL_variants_vcf()[[y]]
      }) %>% bind_rows()
      
      if("POS" %in% (names(circle_data))){
        circle_data$Merge_Name <- paste(circle_data$CHROM, circle_data$POS, circle_data$REF, circle_data$ALT,sep = "_")
      }else if("Start" %in% (names(circle_data)) & "End" %in% (names(circle_data))){
        circle_data$Merge_Name <- paste(circle_data$Chr, circle_data$Start, circle_data$End, circle_data$REF, circle_data$ALT,sep = "_")
      }
      duplicated_row <- (circle_data$Merge_Name %>% table() %>% as.data.frame() %>% dplyr::filter(Freq == max(Freq)))[,1]
      duplicated_row <- as.character(duplicated_row)
      circle_data <- circle_data[(circle_data$Merge_Name %in% duplicated_row) , ] %>% unique()
      circle_data <- subset(circle_data,select = -c(Merge_Name))
      return(circle_data)
    })
    names(group_table) <- stringr::str_remove(names(venn_ALL_data()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
    #print("提取所有交叉数据OK")
    return(group_table)
  })
  
  # 输出所有交叉数据
  # output$circle_Table <- DT::renderDataTable({
  #   circle_table()[[1]]
  # }, options = list(scrollX = TRUE))
  
  
  #5、圈图 -----------------------------------------------------------------------------------------------------------------------------------------------
  
  #5.1 提取百万reads中突变数量
  circle_list <-reactive({
    circle_table <- circle_table()
    if("POS" %in% names(circle_table[[1]])){
      circle_DATA <- lapply(names(circle_table),function(x){
        circle_df <- circle_table[[x]]
        circle_data <- lapply(circle_df$CHROM %>% unique,function(y){
          min_pos <- min(circle_df[circle_df$CHROM == y, "POS"] %>% as.numeric)
          max_pos <- max(circle_df[circle_df$CHROM == y, "POS"] %>% as.numeric)
          idx_seq <- seq(min_pos, max_pos, by = 1000000)
          if (idx_seq[length(idx_seq)] == max_pos & idx_seq[length(idx_seq)] == min_pos) {  #判断min到max增量为1000000的最后一个数是不是同时等于min/max
            idx_seq <- c(idx_seq, max_pos)   #单数剧补一个
          }else if (idx_seq[length(idx_seq)] != max_pos) {
            idx_seq <- c(idx_seq, max_pos)   #没完全覆盖的补一个
          }
          chr_data <- circle_df[circle_df$CHROM == y, ]
          cal_data <- lapply(1:(length(idx_seq)-1), function(z){   #统计每百万中的突变数量
            M_data <- chr_data[as.numeric(chr_data$POS) >= idx_seq[z] & as.numeric(chr_data$POS) < idx_seq[z+1], ]
            M_value <- dim(M_data)[1]
            data.frame(Chr = y, Start = idx_seq[z], End = idx_seq[z+1], Value = M_value)
          }) %>% bind_rows()
          cal_data <- cal_data[cal_data$Value > 0, ]
        }) %>% bind_rows()
        
        circle_data$Value <- log10(circle_data$Value + 1)
        return(circle_data)
      })
    }else if("Start" %in% (names(circle_table[[1]])) & "End" %in% (names(circle_table[[1]]))){
      circle_DATA <- lapply(names(circle_table),function(x){
        circle_df <- circle_table[[x]]
        circle_data <- lapply(circle_df$Chr %>% unique,function(y){
          min_pos <- min(circle_df[circle_df$Chr == y, "Start"] %>% as.numeric)
          max_pos <- max(circle_df[circle_df$Chr == y, "End"] %>% as.numeric)
          #print(min_pos)
          #print(max_pos)
          idx_seq <- seq(min_pos, max_pos, by = 1000000)   #从min到max 增量为1000000
          if (idx_seq[length(idx_seq)] == max_pos & idx_seq[length(idx_seq)] == min_pos) {  #判断min到max增量为1000000的最后一个数是不是同时等于min/max
            idx_seq <- c(idx_seq, max_pos)   #单数剧补一个
          }else if (idx_seq[length(idx_seq)] != max_pos) {
            idx_seq <- c(idx_seq, max_pos)   #没完全覆盖的补一个
          }
          chr_data <- circle_df[circle_df$Chr == y, ]
          #print(idx_seq)
          cal_data <- lapply(1:(length(idx_seq)-1), function(z){   #统计每百万中的突变数量
            M_data <- chr_data[as.numeric(chr_data$Start) >= idx_seq[z] & as.numeric(chr_data$End) < idx_seq[z+1], ]
            M_value <- dim(M_data)[1]
            data.frame(Chr = y, Start = idx_seq[z], End = idx_seq[z+1], Value = M_value)
          }) %>% bind_rows()
          cal_data <- cal_data[cal_data$Value > 0, ]
        }) %>% bind_rows()
        
        circle_data$Value <- log10(circle_data$Value + 1)
        return(circle_data)
      })
    }
    names(circle_DATA) <- names(circle_table)
    print("提取每百万READs数OK")
    return(circle_DATA)
  })
  
  #输出每百万区间突变表格
  # output$circle_Table <- DT::renderDataTable({
  #   circle_list()[[1]]
  # }, options = list(scrollX = TRUE))

  
  #5.2  设置圈图元素
  output$circle_type_Basics_set <- renderUI({
    req(input$circle_type)
    div(
      dropdown(
        label = "Set Circle Type Basics Elements", width = "100%",  right = T, icon = icon("cog", lib = "glyphicon"),
        if(input$circle_type == "points"){
          tags$div(
            h2("Set Points"),
            sliderInput("points.size", "Points Size", min = 0, max = 1, value = 0.3, step = 0.05),
            selectInput("points.pch", "Points Pch", choices = c(0, 1:25), selected = 16),
            sliderInput("points.alpha", "Points Alpha", min = 0, max = 1, value = 0.5, step = 0.05)
          )
        }else if(input$circle_type == "lines"){
          tags$div(
            h2("Set Lines"),
            radioButtons("lines_area", "Lines_Area", c("TRUE","FALSE"), selected = "FALSE",inline = T),
            selectInput("lines_type", "Lines_Type", choices = c("l", "o", "h", "s")),
            selectInput("lines_lty", "Lines_Lty", choices = c(1:6), selected = 1),
            sliderInput("lines_lwd", "Lines_Lwd", min = 0, max = 5, value = 2, step = 0.1)
          )
        }else if(input$circle_type == "rectangles"){
          tags$div(
            h2("Set Rectangles"),
            sliderInput("rectangles_ytop", "Rectangles Ytop", min = 0, max = 5, value = 2, step = 0.1),
            sliderInput("rectangles_ybottom", "Rectangles Ybottom", min = 0, max = 5, value = 1, step = 0.1)
          )
        }
      )
    )
  })
  
  
  #5.3 绘制圈图
  Circle_Plot <- eventReactive(input$circle_star,{
    #withProgress(message = "processing", min = 0, max = 1, {
    req(input$circle_type, input$Species, input$chrom_plotType, input$circle_type_ID, input$circle_text_size,
        input$track.height, input$track.margin1, input$track.margin2, input$track.gap.degree,
        input$start.degree, input$legend.x.position, input$legend.y.position)
    
    circle_list <- circle_list()   #提取每百万reads数列表
    circle_table <- circle_table() #提取venn交集数据表
    
    par(mar = c(1,1,1,1), lwd = 1, cex = 1.5)
    circlize::circos.par(track.height = input$track.height, start.degree = as.numeric(input$start.degree),
                         track.margin = c(as.numeric(input$track.margin1),as.numeric(input$track.margin2)),gap.after = as.numeric(input$track.gap.degree))
    
    if(input$Species == "others"){
      cytoband.df = read.table(input$chrom_files$datapath, colClasses = c("character", "numeric","numeric", "character", "character"), sep = "\t")
      circos.initializeWithIdeogram(cytoband.df, plotType = input$chrom_plotType)
    }else{
      circos.initializeWithIdeogram(species = input$Species, plotType = input$chrom_plotType)
    }
    
    text(0, 0, input$circle_type_ID , cex = 2*input$circle_text_size)
    
    col_fun = colorRamp2(c(0,length(circle_list)), c("blue", "red"))(1:length(circle_list))
    
    if(input$circle_type == "points"){
      req(input$points.size,input$points.alpha, input$points.pch)
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicPoints(region, value, pch = as.numeric(input$points.pch), cex = as.numeric(input$points.size),
                                                             alpha = as.numeric(input$points.alpha) , col = col_fun[x], ...)
                                      },bg.border = "grey")
        
      }
    }else if(input$circle_type == "lines"){
      req(input$lines_type, input$lines_area, input$lines_lwd, input$lines_lty)
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicLines(region, value, type = input$lines_type, col = col_fun[x], border = col_fun[x], area = as.logical(input$lines_area),
                                                            lwd = as.numeric(input$lines_lwd), lty = as.numeric(input$lines_lty), ...)
                                      },bg.border = "grey")
      }
    }else if(input$circle_type == "rectangles"){
      req(input$rectangles_ytop ,input$rectangles_ybottom)
      print(input$Rect_up_color)
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicRect(region, value, col = col_fun[x],border = col_fun[x],
                                                           ytop = as.numeric(input$rectangles_ytop) , ybottom = as.numeric(input$rectangles_ybottom),
                                                           ...)
                                      },bg.border = "grey")
      }
    }
    #incProgress(0.4, detail = "uncompressing file...")
    legend(x = as.numeric(input$legend.x.position), y = as.numeric(input$legend.y.position), pch = 15, cex = input$circle_text_size, legend = names(circle_list), col = col_fun)
    #})
    
    circos.clear()
  })
  
  
  output$cirecle_PlotOutput <- renderPlot({
    Circle_Plot()
  })
  
  output$CirclePlot_download <- downloadHandler(
    req(input$CirclePlot_down_filetype, input$CirclePlot_download_width, input$CirclePlot_download_height),
    filename = function(){
      paste("Circle_plot",input$CirclePlot_down_filetype, sep = ".")
    },
    content = function(file){
      if(input$CirclePlot_down_filetype == "pdf"){
        pdf(file,width = input$CirclePlot_download_width, height = input$CirclePlot_download_height)
      }else if(input$CirclePlot_down_filetype == "png"){
        png(file,width = input$CirclePlot_download_width, height = input$CirclePlot_download_height)
      }else{
        jpeg(file,width = input$CirclePlot_download_width, height = input$CirclePlot_download_height)
      }
      print(Circle_Plot())
      dev.off()
    }
  )
  
#第二部分//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  #6、构建位置分布图--------------------------------------------------------------------------------------------------------------------------------------
    #6.1 构建位置分布数据
  distribution_binded <- reactive({
    req(input$distribution_type_id)
    distribution_bind <- data.frame()
    sample_ID <-stringr::str_subset(names(ALL_variants_vcf()),pattern = input$distribution_type_id, negate = F)
    groups_ID <- stringr::str_remove(names(ALL_variants_vcf()),"-[0-9].snp$|-[0-9].indel$|[0-9].snp$|[0-9].indel$") %>% unique()
    
    for(x in sample_ID){
        data <- ALL_variants_vcf()[[x]]
        sample_name <- strsplit(x, ".",fixed = T)[[1]][1]
        group <- gsub("-[0-9]$|[0-9]$","",sample_name)
        data <- data %>% dplyr::group_by(Func.refGene) %>% summarise(percentage = (n()/nrow(data))*100)
        distribution_bind <- rbind(distribution_bind, data.frame(data, sample_name, group))
    }
    return(distribution_bind)
  })
  
  observeEvent(input$plot_distribution, {
    output$distribution_data <- DT::renderDataTable(
      return(distribution_binded()), extensions ="Buttons",
      server =  as.logical(input$Distribution_data_Set),
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = list('copy','print',list(
          extend = "collection",
          buttons = c('csv','excel','pdf'),
          text = 'Download')
        )
      )
    )
  })
    #6.2 做位置分布柱状图
  distribution_plot <- eventReactive(input$plot_distribution, {
    req(input$distribution_text_size, input$distribution_bar_width,input$distribution_error_bar_size, input$distribution_error_bar_width,
        input$distribution_jitter_size, input$distribution_jitter_width)
    distribution_binded <- distribution_binded()
    ggpubr::ggbarplot(distribution_binded,x = "Func.refGene", y = "percentage", palette = "Set1",
                      add = c("mean_sd"), fill = "group", width = input$distribution_bar_width, position = position_dodge(),
                      add.params = list(width = input$distribution_error_bar_width, shape = "group", size = input$distribution_error_bar_size), ggtheme = theme_classic())+
      xlab(NULL) + ylab("Distribution Proportion of biotypes (%)")+
      theme(text = element_text(face = "bold",family = "Times",size = input$distribution_text_size,color = "black"),legend.position = "right",
            strip.background = element_blank(),axis.text.x = element_text(angle = 45,hjust = 1))+
      geom_jitter(size = input$distribution_jitter_size,width = input$distribution_jitter_width)
  })
  
  output$Distribution_Plot <- renderPlot({
    distribution_plot()
  })
  
  output$Distribution_Download <- downloadHandler(
    filename = function(){
      paste("ALL_Variants_Distribution_plot",input$Distribution_down_filetype,sep = ".")
    },
    content = function(file){
      if(input$Distribution_down_filetype == "pdf"){
        pdf(file,width = input$Distribution_download_width,height = input$Distribution_download_height)
      }else if(input$Distribution_down_filetype == "png"){
        png(file,width = input$Distribution_download_width,height = input$Distribution_download_height)
      }else{
        jpeg(file,width = input$Distribution_download_width,height = input$Distribution_download_height)
      }
      print(distribution_plot())
      dev.off()
    }
  )
  
  
  #7 、找出突变基因并绘制柱状图--------------------------------------------------------------------------------------------------------------------------
  
    # 7.1、筛选突变基因
  
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
  
  # 7.2 对筛选突变的同一基因进行合并
  
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
        buttons = list('copy','print',list(
          extend = "collection",
          buttons = c('csv','excel','pdf'),
          text = 'Download')
        )
      )
    )
  })
  
    #7.3、 绘制突变基因柱状图
  Variants_plot <- eventReactive(input$plot_Variants_genes, {
    req(input$Variants_genes_numbers, input$Variants_genes_bar_width, input$Variants_genes_text_size, input$Variants_sampleID,input$Variants_genes_label)
    Variants_df <- Variants_ALL_genes()[[input$Variants_sampleID]]
    Variants_genes <- Variants_df[order(Variants_df$Numbers, decreasing = T), ][1:input$Variants_genes_numbers ,]
    
    ggpubr::ggbarplot(Variants_genes, x = "Genes", y = "Numbers", label = as.logical(input$Variants_genes_label),
                      width = input$Variants_genes_bar_width, position = position_dodge(),
                      fill = "Genes", ggtheme = theme_classic())+
      xlab(NULL) + ylab("Variants Genes Numbers of biotypes")+
      theme(text = element_text(face = "bold",family = "Times",size = input$Variants_genes_text_size,color = "black"),legend.position = "none",
             strip.background = element_blank(), axis.text.x = element_text(angle = 45,hjust = 1))
  })
 
  
  
  output$Variants_Plots <- renderPlot({
    Variants_plot()
  })
  
  output$Variants_genes_Download <- downloadHandler(
    filename = function(){
      paste(paste(input$Variants_sampleID, "Variants_Genes_Summerise_plot", sep = "_"), input$Variants_Genes_down_filetype, sep = ".")
    },
    content = function(file){
      if(input$Variants_Genes_down_filetype == "pdf"){
        pdf(file, width = input$Variants_Genes_download_width, height = input$Variants_Genes_download_height)
      }else if(input$Variants_Genes_down_filetype == "png"){
        png(file, width = input$Variants_Genes_download_width, height = input$Variants_Genes_download_height)
      }else{
        jpeg(file, width = input$Variants_Genes_download_width, height = input$Variants_Genes_download_height)
      }
      print(Variants_plot())
      dev.off()
    }
  )
  
  
  #8 、构建各组变异基因热图数据-----------------------------------------------------------------------------------------------------------------------
  
    #8.1、 构建选择位置输入框
  output$Variants_position_id <- renderUI({
    ALL_variants_vcf <- ALL_variants_vcf()
    position <- unique(ALL_variants_vcf[[input$Variants_sampleID]]$Func.refGene)
    selectInput("Variants_position_ID", "Select Position To Analyse:", choices = position, multiple = T ,width = "100%")
  })
  
  
  Variants_heatmap_data <- reactive({
    req(input$Variants_heatmap_types, input$Variants_position_ID, input$Variants_heatmap_numbers)
    
    Variants_ALL_genes <- Variants_ALL_genes()
    sample_ID <- stringr::str_subset(names(Variants_ALL_genes), pattern = input$Variants_heatmap_types, negate = F)
    
    
    Variants_heatmap_df <- lapply(sample_ID, function(x){
      
      df <- Variants_ALL_genes[[x]]
      df <- df[df$Position == input$Variants_position_ID, ]
      df <- df[order(df$Numbers, decreasing = T), ][1:input$Variants_heatmap_numbers, ]
      name <- gsub("-", "_", x)
      colnames(df) <- c("Position", "Genes", name)
      df <- as.data.frame(df)
    }) %>% plyr::join_all(by = "Genes", type = "full")
    
    Variants_heatmap_df[is.na(Variants_heatmap_df)] <- 0
    return(Variants_heatmap_df)
  })
  
  
  observeEvent(input$Variants_heatmap, {
  output$Variants_Heatmap_data <- DT::renderDataTable(
    return(Variants_heatmap_data()), extensions ="Buttons",
    server =  as.logical(input$Variants_Genes_Heatmap_data),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      buttons = list('copy','print',list(
        extend = "collection",
        buttons = c('csv','excel','pdf'),
        text = 'Download')
      )
    )
  )
  })
  
    #8.2 、 绘制热图
  Variants_heatmap_plot <- eventReactive(input$Variants_heatmap, {
    req(input$Variants_heatmap_show_rownames, input$Variants_heatmap_show_colnames)
    
    Variants_heatmap_data <- Variants_heatmap_data()
    
    Variants_heatmap_data <- Variants_heatmap_data[, -(colnames(Variants_heatmap_data) == "Position")]
    Variants_heatmap_data <- Variants_heatmap_data %>% dplyr::group_by(Genes) %>% dplyr::summarise_each(funs(sum))
    
    Variants_heatmap_data <- as.data.frame(Variants_heatmap_data)
    rownames(Variants_heatmap_data) <- Variants_heatmap_data$Genes
    Variants_heatmap_data <- Variants_heatmap_data[, -(colnames(Variants_heatmap_data) == "Genes")]
    Variants_heatmap_data <- Variants_heatmap_data[apply(Variants_heatmap_data, 1, function(x) sd(x) != 0), ]
   
    pheatmap::pheatmap(Variants_heatmap_data, scale = "row",
                       show_rownames = as.logical(input$Variants_heatmap_show_rownames),
                       show_colnames = as.logical(input$Variants_heatmap_show_colnames),
                       treeheight_row = input$Variants_heatmap_treeheight_row,
                       treeheight_col = input$Variants_heatmap_treeheight_col)
  })
  
  
  
  output$Variants_Heatmap_Plot <- renderPlot({
    Variants_heatmap_plot() 
  })

  output$Variants_heatmap_Download <- downloadHandler(
    filename = function(){
      paste(paste("Variants_Heatmap_Summerise_plot", sep = "_"), input$Variants_heatmap_down_filetype, sep = ".")
    },
    content = function(file){
      if(input$Variants_heatmap_down_filetype == "pdf"){
        pdf(file, width = input$Variants_heatmap_download_width, height = input$Variants_heatmap_download_height)
      }else if(input$Variants_heatmap_down_filetype == "png"){
        png(file, width = input$Variants_heatmap_download_width, height = input$Variants_heatmap_download_height)
      }else{
        jpeg(file, width = input$Variants_heatmap_download_width, height = input$Variants_heatmap_download_height)
      }
      print(Variants_plot())
      dev.off()
    }
  )
  
  
  #9 、 选择组进行差异分析 -------------------------------------------------------------------------------------------------------
    
    #9.1 、 构建选择控件
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
  
   #9.2 、 提取两组间差异基因
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
  
    #9.4 、 GO富集分析
  
  enrich_plot <- eventReactive(input$plot_enrichPlot ,{
    req(input$enrich_Species_Orgdb ,input$enrich_Fun, input$enrich_geneID)
    library(input$enrich_Species_Orgdb, character.only = T)
    DE_genes_df <- DE_genes_df()
    
    up_gene <- DE_genes_df[DE_genes_df$Change > 1, "Genes"]
    down_gene <- DE_genes_df[DE_genes_df$Change < -1, "Genes"]
    if(input$enrich_Fun == "enrichGO"){
      req(input$enrich_ont)
      UD_List <- list(Up = up_gene, Down = down_gene)
      print(UD_List)
      enrich_results <- clusterProfiler::compareCluster(UD_List, fun = input$enrich_Fun, OrgDb = input$enrich_Species_Orgdb, 
                                               keyType = input$enrich_geneID, pvalueCutoff = 0.05, ont = input$enrich_ont)
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
      
      if(input$enrich_geneID != "ENTREZID"){
        up_gene <- clusterProfiler::bitr(up_gene, fromType = input$enrich_geneID, toType = "ENTREZID", OrgDb = input$enrich_Species_Orgdb)$ENTREZID
        down_gene <- clusterProfiler::bitr(down_gene, fromType = input$enrich_geneID, toType = "ENTREZID", OrgDb = input$enrich_Species_Orgdb)$ENTREZID
      }
       # print(up_gene)
       # print(down_gene)
       UD_List <- list(Up = up_gene, Down = down_gene)
       # print(UD_List)
       enrich_results <- clusterProfiler::compareCluster(UD_List, fun = input$enrich_Fun, organism = enrich_Species_organism, pvalueCutoff = 0.05)
    }
    enrichplot <- enrichplot::dotplot(enrich_results, showCategory = input$showshowCategory_numbers)+
      scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 100))
  })

  
  
  
  observeEvent(input$plot_enrichPlot, {
    #req(input$DESeq_group_ID1,input$DESeq_group_ID1)
    output$DESeq_data <- DT::renderDataTable({
      DE_genes_df <- DE_genes_df()
      DE_genes_df <- DE_genes_df[abs(DE_genes_df$Change) > 1, ]
      colnames(DE_genes_df) <- c("Gene_ID", input$DESeq_group_ID1, input$DESeq_group_ID2, "log2Change")
      return(DE_genes_df)
    })
  })
      
   output$enrich_Plot <- renderPlot({
     enrich_plot()
   })
   
   
   output$Enrich_plot_Download <- downloadHandler(
     filename = function(){
       paste(paste(input$DESeq_group_ID1,"VS", input$DESeq_group_ID2, input$enrich_Fun, "plot", sep = "_"), input$Enrich_plot_down_filetype, sep = ".")
     },
     content = function(file){
       if(input$Enrich_plot_down_filetype == "pdf"){
         pdf(file, width = input$Enrich_plot_download_width, height = input$Enrich_plot_download_height)
       }else if(input$Enrich_plot_down_filetype == "png"){
         png(file, width = input$Enrich_plot_download_width, height = input$Enrich_plot_download_height)
       }else{
         jpeg(file, width = input$Enrich_plot_download_width, height = input$Enrich_plot_download_height)
       }
       print(Variants_plot())
       dev.off()
     }
  )
   
   
   
  
   
   
   
}
    
    
    
    
      
    
    