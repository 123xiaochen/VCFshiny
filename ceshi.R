file <- dir("merge.data/", pattern = "*", full.names = T)
data <- lapply(file, function(x){
  df <- read.table(x, sep = "\t", header = T)
  
})

df <- data[[1]]


#1


Genes_df <- data.frame()

for(x in 1:length(data)){
  df <- data[[x]]
  for(y in 1:nrow(df)){
    gene1 <- stringr::str_split(df[y,]$Gene.refGene, ";" , simplify = F)[[1]]
    #sample_name <- strsplit("AGBE-OT-NT1.indel", ".",fixed = T)[[1]][1]
    #groups <-  gsub("-[0-9]$|[0-9]$","",sample_name)
    #Genes_df <- rbind(Genes_df, data.frame(gene1))
    Genes_df <- bind_rows(Genes_df, data.frame(gene1))
    
    #Genes_df_1 <- Genes_df %>% table() %>% as.data.frame() %>% dplyr::filter()
  }
  #Genes_df <- Genes_df[Genes_df$gene1 != "NONE", ]
}




for(x in 1:nrow(df)){
  #print(stringr::str_subset(df[x,], pattern = ";", negate = F))
  #if(length(strsplit(df[x,]$Gene.refGene, ";" ,fixed = T)[[1]]) > 1){
  gene1 <- stringr::str_split(df[x,]$Gene.refGene, ";" , simplify = F)[[1]]
  #gene1 <- strsplit(df[x,]$Gene.refGene, ";" , fixed  = F)[[1]]
  #}else{
  #  gene1 <- df[x,]$Gene.refGene
  #}
  sample_name <- strsplit("AGBE-OT-NT1.indel", ".",fixed = T)[[1]][1]
  groups <-  gsub("-[0-9]$|[0-9]$","",sample_name)
  #Genes_df <- rbind(Genes_df, data.frame(gene1))
  Genes_df <- bind_rows(Genes_df, data.frame(gene1, sample_name, groups))
#Genes_df_1 <- Genes_df %>% table() %>% as.data.frame() %>% dplyr::filter()
}
#2

for(x in nrow(df)){
  gene1 <- strsplit(df[x,]$Gene.refGene, ";", fixed = T)[[1]]
}










for(x in 1:nrow(df)){
  gene_long <- stringr::str_subset(df$Gene.refGene, pattern = ";", negate = F)
  gene_short <- df$Gene.refGene
}
  for(y in gene_long){
    gene1 <- strsplit(y , ";" ,fixed = T)[[1]]
    sample_name <- strsplit("AGBE-OT-NT1.indel", ".",fixed = T)[[1]][1]
    groups <-  gsub("-[0-9]$|[0-9]$","",sample_name)
    Genes_df <- rbind(Genes_df, data.frame(gene1,sample_name, groups))
  }
  for(z in gene_short){
    gene1 <- z
  }

sample_name <- strsplit("AGBE-OT-NT1.indel", ".",fixed = T)[[1]][1]
groups <-  gsub("-[0-9]$|[0-9]$","",sample_name)
Genes_df <- rbind(Genes_df, data.frame(gene1,sample_name, groups))

