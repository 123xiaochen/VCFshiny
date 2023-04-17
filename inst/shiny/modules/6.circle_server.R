#1-6、圈图 -----------------------------------------------------------------------------------------------------------------------------------------------
#1-5.4 、 提取所有交叉数据（圈图准备数据）
output$circle_group <- renderUI({
  if (input$circle_type_ID == "snp") {
    observe(SNP_data())
    virtualSelectInput(
      inputId = "Circle_Group",  label = "Sample groups:",
      choices = unique(gsub("-[0-9].snp$|_[0-9].snp$|.snp", "", names(SNP_data()))),
      selected = unique(gsub("-[0-9].snp$|_[0-9].snp$|.snp", "", names(SNP_data()))),
      multiple = T, search = F, width = "100%"
    )
  }else {
    observe(Indel_data())
    virtualSelectInput(
      inputId = "Circle_Group",  label = "Sample groups:",
      choices = unique(gsub("-[0-9].indel$|_[0-9].indel$|.indel","",names(Indel_data()))),
      selected = unique(gsub("-[0-9].indel$|_[0-9].indel$|.indel","",names(Indel_data()))),
      multiple = T, search = F, width = "100%"
    )
  }
})

circle_list <- eventReactive(input$circle_star, {
  if (input$circle_type_ID == "snp") {
    variants_list <- SNP_data()
  }else {
    variants_list <- Indel_data()
  }

  cbind_list <- lapply(input$Circle_Group, function(x){
    rbind_df <- variants_list[grepl(pattern = x, x = names(variants_list))] %>% dplyr::bind_rows()
    rbind_df[!rbind_df %>% duplicated.data.frame(), ]
  })
  names(cbind_list) <- input$Circle_Group
  circle_vperM_list <- lapply(cbind_list, function(x){
    circle_df <- x
    circle_vperM_df <- lapply(circle_df[, 1] %>% unique,function(y){
      min_pos <- min(circle_df[circle_df[, 1] == y, 2] %>% as.numeric())
      max_pos <- max(circle_df[circle_df[, 1] == y, 2] %>% as.numeric())
      idx_seq <- seq(min_pos, max_pos, by = 1000000)
      if (idx_seq[length(idx_seq)] == max_pos & idx_seq[length(idx_seq)] == min_pos) {  #判断min到max增量为1000000的最后一个数是不是同时等于min/max
        idx_seq <- c(idx_seq, max_pos)   #单数剧补一个
      }else if (idx_seq[length(idx_seq)] != max_pos) {
        idx_seq <- c(idx_seq, max_pos)   #没完全覆盖的补一个
      }
      chr_df <- circle_df[circle_df[, 1] == y, ]
      chr_vperM_df <- lapply(1:(length(idx_seq)-1), function(z){   #统计每百万中的突变数量
        vPerM <- chr_df[as.numeric(chr_df[, 2]) >= idx_seq[z] & as.numeric(chr_df[, 2]) < idx_seq[z+1], ]
        data.frame(Chr = y, Start = idx_seq[z], End = idx_seq[z+1], Value = dim(vPerM)[1])
      }) %>% dplyr::bind_rows()
      chr_vperM_df <- chr_vperM_df[chr_vperM_df$Value > 0, ]
    }) %>% dplyr::bind_rows()

    circle_vperM_df$Value <- log10(circle_vperM_df$Value + 1)
    if(!("chr" %in% circle_vperM_df[1,1])){
      circle_vperM_df[,1] <- paste0("chr",circle_vperM_df[,1])
    }
    return(circle_vperM_df)
  })
  return(circle_vperM_list)
})

# #1-6.3 绘制圈图
Circle_Plot <- eventReactive(input$circle_star,{
  withProgress(message = "Loading Circle data...", min = 0, max = 1, {

  circle_list <- circle_list() #提取每百万reads数列表
  par(mar = c(2,2,2,2), lwd = 1, cex = 1.5)
  circlize::circos.par(track.height = input$track.height, start.degree = as.numeric(input$start.degree),
                       track.margin = c(as.numeric(input$track.margin1),as.numeric(input$track.margin2)), gap.after = as.numeric(input$track.gap.degree))

  if(input$Species == "others"){
    cytoband.df = read.table(input$chrom_files$datapath, colClasses = c("character", "numeric","numeric", "character", "character"), sep = "\t")
    chr <- cytoband.df$chr
    circos.initializeWithIdeogram(cytoband.df, plotType = input$chrom_plotType)
  }else{
    chr <- circlize:::usable_chromosomes(input$Species)
    circos.initializeWithIdeogram(species = input$Species, plotType = input$chrom_plotType)
  }

  incProgress(0.4, message  = "Analyse Circle data ...")
  text(0, 0, input$circle_type_ID , cex = input$circle_center_text_size)

  if (length(names(circle_list)) < 3) {
    col_fun <- RColorBrewer::brewer.pal(length(names(circle_list)), "Set1")[1:length(names(circle_list))]
  }else {
    col_fun <- RColorBrewer::brewer.pal(length(names(circle_list)), "Set1")
  }

  if(input$circle_type == "points"){
    for(x in 1:length(circle_list)){
      circlize::circos.genomicTrack(circle_list[[x]],
                                    panel.fun = function(region, value, ...) {
                                      i = getI(...)
                                      circos.genomicPoints(region, value, pch = as.numeric(input$points.pch), cex = as.numeric(input$points.size),
                                                           alpha = as.numeric(input$points.alpha) , col = col_fun[x], ...)
                                    },bg.border = "grey")
    }
  }else if(input$circle_type == "lines"){
    for(x in 1:length(circle_list)){
      circlize::circos.genomicTrack(circle_list[[x]],
                                    panel.fun = function(region, value, ...) {
                                      i = getI(...)
                                      circos.genomicLines(region, value, type = input$lines_type, col = col_fun[x], border = col_fun[x], area = as.logical(input$lines_area),
                                                          lwd = as.numeric(input$lines_lwd), lty = as.numeric(input$lines_lty), ...)
                                    },bg.border = "grey")
    }
  }else if(input$circle_type == "heatmap"){
    col.max <- lapply(circle_list, function(x){ x$Value %>% max() }) %>% unlist() %>% max()
    for(x in 1:length(circle_list)){
      circos.genomicHeatmap(circle_list[[x]][circle_list[[x]][, 1] %in% chr, ], col = colorRamp2(c(0, col.max), c("white", col_fun[x])), connection_height = NULL)
    }
  }
  incProgress(0.6, message = "Drawwing Circle plot ...")
  legend(x = as.numeric(input$legend.x.position), y = as.numeric(input$legend.y.position), pch = 15, cex = input$circle_legend_text_size, legend = names(circle_list), col = col_fun )
  })
  circos.clear()
})


output$cirecle_PlotOutput <- renderPlot({
  Circle_Plot()
})


#1-6.4下载图片
output$CirclePlot_download <- downloadHandler(
  filename = function(){
    paste(paste("5", input$circle_type_ID, input$circle_type, "Circle_plot", sep = "_"),"pdf" , sep = ".")
  },
  content = function(file){
    pdf(file,width = input$CirclePlot_download_width, height = input$CirclePlot_download_height)
     #重新作图下载
    circle_list <- circle_list() #提取每百万reads数列表

    par(mar = c(1,1,1,1), lwd = 1, cex = 1.5)
    circlize::circos.par(track.height = input$track.height, start.degree = as.numeric(input$start.degree),
                         track.margin = c(as.numeric(input$track.margin1),as.numeric(input$track.margin2)),gap.after = as.numeric(input$track.gap.degree))

    if(input$Species == "others"){
      cytoband.df = read.table(input$chrom_files$datapath, colClasses = c("character", "numeric","numeric", "character", "character"), sep = "\t")
      chr <- cytoband.df$chr
      circos.initializeWithIdeogram(cytoband.df, plotType = input$chrom_plotType)
    }else{
      # print(input$Species)
      chr <- circlize:::usable_chromosomes(input$Species)
      circos.initializeWithIdeogram(species = input$Species, plotType = input$chrom_plotType)
    }

    text(0, 0, input$circle_type_ID , cex = input$circle_center_text_size)

    col_fun <- RColorBrewer::brewer.pal(length(names(circle_list)), "Set1")
    if(input$circle_type == "points"){
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicPoints(region, value, pch = as.numeric(input$points.pch), cex = as.numeric(input$points.size),
                                                             alpha = as.numeric(input$points.alpha) , col = col_fun[x], ...)
                                      },bg.border = "grey")
      }
    }else if(input$circle_type == "lines"){
      for(x in 1:length(circle_list)){
        circlize::circos.genomicTrack(circle_list[[x]],
                                      panel.fun = function(region, value, ...) {
                                        i = getI(...)
                                        circos.genomicLines(region, value, type = input$lines_type, col = col_fun[x], border = col_fun[x], area = as.logical(input$lines_area),
                                                            lwd = as.numeric(input$lines_lwd), lty = as.numeric(input$lines_lty), ...)
                                      },bg.border = "grey")
      }
    }else if(input$circle_type == "heatmap"){
      col.max <- lapply(circle_list, function(x){ x$Value %>% max() }) %>% unlist() %>% max()
      for(x in 1:length(circle_list)){
        circos.genomicHeatmap(circle_list[[x]][circle_list[[x]][, 1] %in% chr, ], col = colorRamp2(c(0, col.max), c("white", col_fun[x])), connection_height = NULL)
      }
    }

    legend(x = as.numeric(input$legend.x.position), y = as.numeric(input$legend.y.position), pch = 15, cex = input$circle_legend_text_size, legend = names(circle_list), col = col_fun )

    circos.clear()

    dev.off()
  }
)
