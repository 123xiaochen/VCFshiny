#1-6、圈图 -----------------------------------------------------------------------------------------------------------------------------------------------

#1-6.1 提取百万reads中突变数量
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


#1-6.2  设置圈图元素
output$circle_type_Basics_set <- renderUI({
  req(input$circle_type)
  div(
      # h4("Set Colors:"),
      uiOutput("Circle_Elements_Colors"),
      if(input$circle_type == "points"){
        tags$div(
          h4("Set Points:"),
          sliderInput("points.size", "Points Size:", min = 0, max = 1, value = 0.3, step = 0.05),
          selectInput("points.pch", "Points Pch:", choices = c(0, 1:25), selected = 16),
          sliderInput("points.alpha", "Points Alpha:", min = 0, max = 1, value = 0.5, step = 0.05)
        )
      }else if(input$circle_type == "lines"){
        tags$div(
          h4("Set Lines:"),
          radioButtons("lines_area", "Lines_Area:", c("TRUE","FALSE"), selected = "FALSE",inline = T),
          selectInput("lines_type", "Lines_Type:", choices = c("l", "o", "h", "s")),
          selectInput("lines_lty", "Lines_Lty:", choices = c(1:6), selected = 1),
          sliderInput("lines_lwd", "Lines_Lwd:", min = 0, max = 5, value = 2, step = 0.1)
        )
      }else if(input$circle_type == "rectangles"){
        tags$div(
          h4("Set Rectangles:"),
          sliderInput("rectangles_ytop", "Rectangles Ytop:", min = 0, max = 5, value = 2, step = 0.1),
          sliderInput("rectangles_ybottom", "Rectangles Ybottom:", min = 0, max = 5, value = 1, step = 0.1)
        )
      }
  )
})


output$Circle_Elements_Colors <- renderUI({
  ALL_data <- ALL_data()#1-1 Server中
  div(
    textInput("circle_elements_color", "Set Elements Colors:", value = as.character(paste(colorRamp2(c(0,length(unique(ALL_data$group))),c("blue", "red"))(1:length(unique(ALL_data$group))), collapse = ",")),
              width = "100%")
  )
})


#1-6.3 绘制圈图
Circle_Plot <- eventReactive(input$circle_star,{
  withProgress(message = "Loading Circle data...", min = 0, max = 1, {
  req(input$circle_type, input$Species, input$chrom_plotType, input$circle_type_ID, input$circle_text_size,
      input$track.height, input$track.margin1, input$track.margin2, input$track.gap.degree,
      input$start.degree, input$legend.x.position, input$legend.y.position, input$circle_elements_color)

  circle_list <- circle_list() #提取每百万reads数列表
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
  incProgress(0.4, detail = "Analyse Circle data ...")

  text(0, 0, input$circle_type_ID , cex = 2*input$circle_text_size)

  col_fun <- strsplit(input$circle_elements_color, ",")
  if(input$circle_type == "points"){
    req(input$points.size,input$points.alpha, input$points.pch)
    for(x in 1:length(circle_list)){
      circlize::circos.genomicTrack(circle_list[[x]],
                                    panel.fun = function(region, value, ...) {
                                      i = getI(...)
                                      circos.genomicPoints(region, value, pch = as.numeric(input$points.pch), cex = as.numeric(input$points.size),
                                                           alpha = as.numeric(input$points.alpha) , col = col_fun[[1]][x], ...)
                                    },bg.border = "grey")

    }
  }else if(input$circle_type == "lines"){
    req(input$lines_type, input$lines_area, input$lines_lwd, input$lines_lty)
    for(x in 1:length(circle_list)){
      circlize::circos.genomicTrack(circle_list[[x]],
                                    panel.fun = function(region, value, ...) {
                                      i = getI(...)
                                      circos.genomicLines(region, value, type = input$lines_type, col = col_fun[[1]][x], border = col_fun[[1]][x], area = as.logical(input$lines_area),
                                                          lwd = as.numeric(input$lines_lwd), lty = as.numeric(input$lines_lty), ...)
                                    },bg.border = "grey")
    }
  }else if(input$circle_type == "rectangles"){
    req(input$rectangles_ytop ,input$rectangles_ybottom)
    for(x in 1:length(circle_list)){
      circlize::circos.genomicTrack(circle_list[[x]],
                                    panel.fun = function(region, value, ...){
                                      i = getI(...)
                                      circos.genomicRect(region, value, col = col_fun[x],border = col_fun[[1]][x],
                                                         ytop = as.numeric(input$rectangles_ytop) , ybottom = as.numeric(input$rectangles_ybottom),
                                                         ...)
                                    },bg.border = "grey")
    }
  }
  incProgress(0.6, detail = "Plot Circle plot ...")
  legend(x = as.numeric(input$legend.x.position), y = as.numeric(input$legend.y.position), pch = 15, cex = input$circle_text_size, legend = names(circle_list), col = col_fun[[1]])
  })
  circos.clear()
})


output$cirecle_PlotOutput <- renderPlot({
  Circle_Plot()
})


#1-6.4下载图片
output$CirclePlot_download <- downloadHandler(
  req(input$circle_type_ID, input$circle_type, input$CirclePlot_download_width, input$CirclePlot_download_height),
  filename = function(){
    paste("Circle_plot",input$circle_type_ID, input$circle_type ,"pdf", sep = ".")
  },
  content = function(file){
    pdf(file,width = input$CirclePlot_download_width, height = input$CirclePlot_download_height)
    #重新作图下载
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
    col_fun <- strsplit(input$circle_elements_color, ",")
    if(input$circle_type == "points"){
      req(input$points.size,input$points.alpha, input$points.pch)
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicPoints(region, value, pch = as.numeric(input$points.pch), cex = as.numeric(input$points.size),
                                                             alpha = as.numeric(input$points.alpha) , col = col_fun[[1]][x], ...)
                                      },bg.border = "grey")

      }
    }else if(input$circle_type == "lines"){
      req(input$lines_type, input$lines_area, input$lines_lwd, input$lines_lty)
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicLines(region, value, type = input$lines_type, col = col_fun[[1]][x], border = col_fun[[1]][x], area = as.logical(input$lines_area),
                                                            lwd = as.numeric(input$lines_lwd), lty = as.numeric(input$lines_lty), ...)
                                      },bg.border = "grey")
      }
    }else if(input$circle_type == "rectangles"){
      req(input$rectangles_ytop ,input$rectangles_ybottom)
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicRect(region, value, col = col_fun[[1]][x],border = col_fun[[1]][x],
                                                           ytop = as.numeric(input$rectangles_ytop) , ybottom = as.numeric(input$rectangles_ybottom),
                                                           ...)
                                      },bg.border = "grey")
      }
    }
    legend(x = as.numeric(input$legend.x.position), y = as.numeric(input$legend.y.position), pch = 15, cex = input$circle_text_size, legend = names(circle_list), col = col_fun[[1]])

    circos.clear()

    dev.off()
  }
)
