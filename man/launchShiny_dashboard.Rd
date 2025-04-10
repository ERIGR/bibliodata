% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launchShiny_dashboard.R
\name{launchShiny_dashboard}
\alias{launchShiny_dashboard}
\title{Shiny app to generate graphical representations of datasets}
\usage{
launchShiny_dashboard(
  object,
  colors = c("white", "grey", "aquamarine", "bisque", "black", "blue", "blue4", "brown",
    "cyan", "gold", "gold4", "green", "khaki", "magenta", "navyblue", "orange", "pink",
    "pink4", "purple", "red", "sienna", "yellow")
)
}
\arguments{
\item{object}{An object of class bibliodata that contains a set of datasets of interest for the user.}

\item{colors}{Character vector A set of colors (for example some of those provided as output of the function grDevices::colors). The user should take enough colors to be sure that he will be able 
pick a color for each conditions associated to the datasets he wants to explore with this application.}
}
\description{
This function launch a shiny app that allows to the user to generate either violinplots representations (based on the package ggplot2) or heatmap representations of each of the 
dataset stored in the bibliodata object provided by the user as value of the argument bibliodata.
}
\examples{


require(readr)
require(R.utils)
expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 

diff_results <- read_rds(FOXA2_differential_dataframe_example())

rownames(diff_results) <- diff_results$ID

FOXA2_RNAseq <- datasettorep(counts = expr_mat, 
metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)), 
colcond = "condition", 
colsignificativity = "adj.P.Val" , 
stattest = list("WT_vs_FOXA2ko" = diff_results),
significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
description = "The normalized counts matrix (limma normalization) for this dataset 
(GSE119931; Balestrieri C et al, 2019, EMBO J) 
has been retrieved from the github of the package DecoupleR.
https://github.com/saezlab/decoupleR/tree/master/inst/extdata")


normalized_counts_EG <- read_rds(EG_RNAseq_count_matrix_example())

metadata_EG <- read_rds(EG_RNAseq_metadata_example())

DESeq2_EG <- read_rds(EG_RNAseq_differential_analysis_example())

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
HEL cells carrying the ETO2-GLIS2 fusion 
but with a truncated C265G domain (zinc finger of the GLIS2 part of the protein), 
HEL cells carrying the ETO2 #'GLIS2 fusion but with 
a truncation of the dimerization domain of the ETO2 part of the protein
(Aid et al, Leukemia, 2022 ; Thirant et al, cancer Cell 2017). 
Quantification of reads has been performed #' with Salmon and subsequent analysis 
(Variance Stabilizing Transformation to obtain the normalized matrix 
and the differential expression analysis) have been performed by DESeq2)")

example_bibliodata <- bibliodata(biblio = list("FOXA2_RNAseq" = FOXA2_RNAseq,
"EG_HEL" = EG_HEL_dataset))

res <- withTimeout({
 launchShiny_dashboard(example_bibliodata,colors = c("red","blue","green",
"purple","orange","yellow","black","grey"))
}, timeout = 0.75, onTimeout = "silent")

}
