# The package Bibliodata

Elie Robert

## Introduction

This R package can be used to store one or several datasets corresponding to matrix of type features*samples associated to data.frame of metadata and eventually dataframes corresponding to results of differential analysis between pairs of conditions. It allows to perform graphical representations of these data (violinplots or pheatmap representations) for  specified features. It can be used for example to store and visualize bulk-RNAseq datasets eventually by using a shiny application that can be launch with a function defined  in this package. Each dataset can be stored in an object of a R S4-class called Datasettorep and several objects of this class can be stored in object of another R S4-class called Bibliodata.An object of class Bibliodata can be provided as input of the function launchShiny_dashboard which allows to launch a Shiny app. This app makes easier the generation of graphical representations of type violinplots or pheatmap.

## Installation of the package 

To install this package run :

``` devtools::install_github("ERIGR/bibliodata") ```

in R. This should be enough to install the package. If you encounter some problems at this step, don't hesitate to open an issue. I will try to answer as soon as possible.

Furthermore, to test how the package works with example, you should install the readr package if necessary and then load it.

To install this package if required :

``` install.packages("readr") ```

To load it :

``` require(readr) ```


## Use of the package

### Generation of Datasettorep objects. 

To generate a Bibliodata object that can be use interactively by running the function launchShiny_dashboard, one should create at least one Datasettorep object.
This can be done by using the builder function datasettorep.

Below, a first example using a dataset stored within the package.

``` 
expr_mat <- readr::read_rds(FOXA2_RNAseq_count_matrix_example()) 

diff_results <- readr::read_rds(FOXA2_differential_dataframe_example())

rownames(diff_results) <- diff_results$ID

FOXA2_RNAseq <- datasettorep(counts = expr_mat, 
metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)), 
colcond = "condition", 
colsignificativity = "adj.P.Val" , 
stattest = list("WT_vs_FOXA2ko" = diff_results),
significativity_symbols = list(c("***","*","NS"),c(0,0.01,0.05,1)),
description = "The normalized counts matrix (limma normalization) for this dataset 
(GSE119931; Balestrieri C et al, 2019, EMBO J) 
has been retrieved from the github of the package DecoupleR.
https://github.com/saezlab/decoupleR/tree/master/inst/extdata")

```

Of note, the rownames of the dataframe provided as value of the metadata argument should match the colnames of the numerical matrix provided as value of the argument counts. It should correspond to the names of the samples for which we have the bulk RNAseq measures for a large set of genes. The dataframe metadata has a column named condition. This is the column used to specify the experimental condition associated to each sample as the value of the argument colcond is indeed condition. If the metadata dataframe contains several columns another column of class factor or character, another column might be used by changing the value of the argument colcond. However, in this case check or change the list given as value of the argument stattest which will probably become inappropriate as such (because groups of samples will not be the same).

The stattest list contains if possible differential results between each possible pair of conditions (here only one WT vs FOXA2ko). The list contains one data.frame containing results for each possible comparison. Here, only one comparison is possible and consequently there is only one dataframe in the list provided. 

The nomenclature to name features (here genes) should be the same for the dataframes of stattest and for the matrix used of value of counts (rownames of the dataframes of stattest and rownames of the counts matrix).

The value given as value of the argument colsignificativity specifies which column of the dataframes of the list of the slot datframes give the level of significance which will be eventually used to assess the level of significance of a difference between two conditions for a given feature (here genes). This can be used to print stars of significance in the generated violinplots.

significativity_symbols is a list that is used to specify character chains (most generally one or several stars or something like ns) which be associated to intervals of values used to assess the significance. For instance in the example above, three stars will be associated to values between 0 and 0.01. One star only for values between 0.01 and 0.05 and the character chain NS for values superior to 0.05.

Of note the slot description (argument description) allows to provide a description of the data and how they have been generated (description of the pipeline to generate the normalized counts and perform the differential analysis for example). It can only be used to say from where come the data. This slot will then be available using :

``` 

FOXA2_RNAseq@description

```

Let's generate now a new Datasettorep object :

```

normalized_counts_EG <- readr::read_rds(EG_RNAseq_count_matrix_example())

metadata_EG <- readr::read_rds(EG_RNAseq_metadata_example())

DESeq2_EG <- readr::read_rds(EG_RNAseq_differential_analysis_example())

conv_dataframe <- GTF_ensembl_to_conv_df(GTF_homo_sapiens_example())

EG_HEL_dataset  <- new("Datasettorep",
counts = normalized_counts_EG,
metadata = metadata_EG,
stattest = DESeq2_EG,
colcond = "condition",
colsignificativity = "padj",
significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
conv_df = conv_dataframe,
description = "RNAseq of HEL cells (cell line derived from a patient 
suffering of an erythroleukemia). 
There is four conditions is this dataset : control HEL cells, HEL cells carrying the ETO2-GLIS2 
fusion (introduced by CRISPR-Cas9 editing), 
HEL cells carrying the ETO2-GLIS2 fusion but with a truncated 
C265G domain (zinc finger of the GLIS2 part of the protein), 
HEL cells carrying the ETO2 GLIS2 fusion but with a truncation 
of the dimerization domain of the ETO2 part of the protein
(Aid et al, Leukemia, 2022 ; Thirant et al, cancer Cell 2017). 
Quantification of reads has been performed with Salmon and subsequent analysis 
(Variance Stabilizing Transformation to obtain the normalized matrix 
and the differential expression analysis) have been performed by DESeq2)")

```

Here another argument is used, the argument conv_df.
This argument allows to provide a dataframe that gives the correspondance between two different nomenclatures (here the ENSEMBL nomenclature and the gene symbol nomenclature). The first column give the name of features (here genes) in the nomenclature used in the counts matrix and the second the nomenclature gene symbol which is used by biologist. This will be used by the shiny app to do the correspondance between gene symbols provided by the users (biologists for example) and the nomenclature used for the matrix and the differential test. Every character chains in the first column are unique while it might not be the case in the second column.

Now we can generate a bibliodata object made of two Datasettorep objects :

```
example_bibliodata <- bibliodata(biblio = list("FOXA2_RNAseq" = FOXA2_RNAseq,
"EG_HEL" = EG_HEL_dataset))

```

Then, we can launch the shiny application useful to generate easily violinplots representations or pheatmap representations :

```

launchShiny_dashboard(example_bibliodata)


```

To avoid to have to download too huge files, original files have been subsetted so with the example's datasets and the example GTF,
you can only represent few genes :
FOXA2
NOC2CL
PLEKHN1
RHBDL2
PLEKHH2
CLU
FHL1
ERG
GATA1
RBP4

Genes that are not in the count matrix and the differential results will not be represented (variable between the two example datasets). All other genes will be printed in the graphical representations.




 
 




