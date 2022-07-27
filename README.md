# WGS-shiny
WGS-shiny,an R shiny application that can be launched easily from a local web browser for analyzing Whole Genomes Sequenced VCF data or Annovar annotation TXT data. This application is started from data input, then through select display input data and General Analysis, Extension Analysis. General Analysis include All Snp + Indel Variants summarise, All Snp Heatmap Analysis, All Snp Variants Type summarisis, Snp or Indel Cross-analysis was repeated for each sample, Snp or Indel circos plot analysis. Extension Analysis require input data must through Annovar annotation, which include Statistical analysis of Snp or Indel Variants Position distribution, All samples Variants Summarise, Statistical heatmap analysis of mutant genes in each samples, GO/KEGG enrichment analysis of mutant genes in each samples. WGS-shiny provides a clear analysis flow and a user-friendly GUI interface but keeps the most important parameters of involved functions, which suit both non-programming experiences and researchers and expert bioinformatics researchers. User can accomplish a standard WGS Analyse in hours depending on the size of their dataset. 

## Analyse Flowchart
![image](https://user-images.githubusercontent.com/95121465/181185498-f3db2952-f1e8-4848-85cc-263c31b6b13d.png)

## Features

## Installing
#### 1、Install the dependencies from Bioconductor:
'''
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
    
## BiocManager::install("BiocUpgrade") ## you may need this
bio_pkgs <- c("STRINGdb", "dplyr", "ggplot2", "circlize", "ggpubr", "shinydashboard", "clusterProfiler", "shiny", "enrichplot", "shinyWidgets", "tidyverse","shinyBS")
             
for (i in bio_pkgs) {
  if (!requireNamespace(i, quietly=TRUE))
  BiocManager::install(i)
}
'''

#### 2、Install the WGS-shiny package from github:

'''
## you may need install devtools first
##install.packages("devtools")
devtools::install_github("123xiaochen/WGS-shiny")
'''
## Getting Start

## Documentation

## Development
WGS-shiny development takes place on Github: https://github.com/123xiaochen/WGS-shiny

Please submit any reproducible bugs you encounter to the issue tracker

We will also put most commonly encountered issues in the FAQ page.
