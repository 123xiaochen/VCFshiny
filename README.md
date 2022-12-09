# WGS-shiny
VCFshiny is a R shiny application that can be easily launched from a local web browser to analyze whole-genome sequencing data after variant calling for scientists without programming expertise. In this tutorial, we will go through the installation and usage of each module step by step using the example dataset we provided at github page.

## Analyse Flowchart
![image](https://user-images.githubusercontent.com/95121465/181185498-f3db2952-f1e8-4848-85cc-263c31b6b13d.png)

## How to start
This is an instruction of how to install and run VCFshiny software locally.

### Requirement:
- R(>=4.2.0)
- Shiny(>=1.6.0)

### How to install shiny packages:
 (1) Open R.
 (2) User can install the shiny package by the following commmand in R:install.package(“shiny”).

### How to install and run VCFshiny locally:
Download VCFshiny from github to your local computer before running it.
 (1) Open R.
 (2) Run VCFshiny by the following commands in R: shiny::runApp('inst/shiny')
 
## Prepare data

### Source of VCF input data
The Variant Call Format (VCF) is used to record gene sequence variations. It is also the first file format to be understood for genome population correlation analysis. First, the whole genome sequencing file is mapping to the reference, and then the resulting bam file is comprehensively analyzed using variant calling software such as GATK and the reference genome data to produce the VCF result.
### Source of TXT input data
TXT files are one of several output formats annotated by Annovar (Wang K, Li M, Hakonarson H. 2010), which is able to analyze genetic variations in various genomes using the latest data. Since the input data VCF file of Annovar software only contains the starting position of the mutation, it is necessary to adjust the input data before using, and add the end position of the mutation after the actual position of the mutation. Gene-based annotations reveal variant's direct relationship with known genes and its functional impact, while region-based annotations reveal Variant's relationship with specific segments of different genomes.
### Input Compress Files Requirements
![image](https://github.com/123xiaochen/WGS-shiny/blob/main/inst/shiny/www/images/Fold_Format.png)
- The compressed file name must be the same as the name of the compressed folder.
- The compressed file can be in *.tar. gz or *.zip format.

### Input File Name Requirements 
![image](https://github.com/123xiaochen/WGS-shiny/blob/main/inst/shiny/www/images/Input%20File%20Format.png)
- The first box represents the sample name, which can be the group of experiments and the number of repetitions, connected by the character "-" or "_".
- The second box represents the data type, which can be snp or indel data. When snp and indel are not classified in the data, this box can be absent.
- The third box represents the data format, which can be vcf files, vcf. gz compressed files, and Annovar annotated TXT files.
- The contents of the three boxes are connected by ".".

## Start your anlysis
![image](https://github.com/123xiaochen/WGS-shiny/blob/main/inst/shiny/www/images/startPage.png)
## Documentation
The documentation is available at <a href="https://github.com/123xiaochen/WGS-shiny" target="_blank"> here </a>, the doc include a tutorial and example gallery.
## Development
WGS-shiny development takes place on Github: https://github.com/123xiaochen/WGS-shiny

Please submit any reproducible bugs you encounter to the <a href="https://github.com/123xiaochen/WGS-shiny/issues" target="_blank">issue tracker</a>

We will also put most commonly encountered issues in the FAQ page.
