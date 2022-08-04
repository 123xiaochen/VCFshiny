# WGS-shiny
WGS-shiny,an R shiny application that can be launched easily from a local web browser for analyzing Whole Genomes Sequenced VCF data or Annovar annotation TXT data. This application is started from data input, then through select display input data and General Analysis, Extension Analysis. General Analysis include All Snp + Indel Variants summarise, All Snp Heatmap Analysis, All Snp Variants Type summarisis, Snp or Indel Cross-analysis was repeated for each sample, Snp or Indel circos plot analysis. Extension Analysis require input data must through Annovar annotation, which include Statistical analysis of Snp or Indel Variants Position distribution, All samples Variants Summarise, Statistical heatmap analysis of mutant genes in each samples, GO/KEGG enrichment analysis of mutant genes in each samples. WGS-shiny provides a clear analysis flow and a user-friendly GUI interface but keeps the most important parameters of involved functions, which suit both non-programming experiences and researchers and expert bioinformatics researchers. User can accomplish a standard WGS Analyse in hours depending on the size of their dataset. 

## Analyse Flowchart
![image](https://user-images.githubusercontent.com/95121465/181185498-f3db2952-f1e8-4848-85cc-263c31b6b13d.png)

## Installing

#### Install the WGS-shiny package from github:

- you may need install devtools first
```
##install.packages("devtools")
devtools::install_github("123xiaochen/WGS-shiny")
```

## Getting Start
### Input data Requirements
- The input file requires all data to be stored in a compressed folder in the format of the file name.

#### Input Compress Files Requirements
![image](https://github.com/123xiaochen/WGS-shiny/blob/main/inst/shiny/www/images/Fold_Format.png)
- The compressed file name must be the same as the name of the compressed folder.
- The compressed file can be in *.tar. gz or *.zip format.

#### Input File Name Requirements 
![image](https://github.com/123xiaochen/WGS-shiny/blob/main/inst/shiny/www/images/Input%20File%20Format.png)
- The first box represents the sample name, which can be the group of experiments and the number of repetitions, connected by the character "-" or "_".
- The second box represents the data type, which can be snp or indel data. When snp and indel are not classified in the data, this box can be absent.
- The third box represents the data format, which can be vcf files, vcf. gz compressed files, and Annovar annotated TXT files.
- The contents of the three boxes are connected by ".".

### Start your anlysis

## Documentation

## Development
WGS-shiny development takes place on Github: https://github.com/123xiaochen/WGS-shiny

Please submit any reproducible bugs you encounter to the <a href="https://github.com/123xiaochen/WGS-shiny/issues" target="_blank">issue tracker</a>

We will also put most commonly encountered issues in the FAQ page.
