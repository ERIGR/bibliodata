
#' Builder function to generate a new object of class Datasettorep
#' @description Builder function to generate a new object of class Datasettorep
#' @rdname datasettorep
#'
#'
#'@return an object of class Datasettorep
#'
#'@param counts a matrix (class matrix) that stored values of each variables (rows) for each sample (column) 
#'
#'@param metadata a data.frame (class data.frame) that gives the condition associated to each sample (rows). This data.frame should contain at least a column (whose name is given by the slot colcondition) 
#' that gives the condition associated to each sample. Rownames of the metadata data.frame should correspond to colnames of the matrix counts. (samples should be ranged in the same order as in the matrix #' counts)
#'
#'@param stattest a list (class list). Each element of this list is a data.frame (class data.frame) with at least one column (whose name is given the slot colsignificativity) that gives the level of  #'significativity (for instance, p-value or fdr) of the difference between two conditions for each feature. That is why each element of the list should correspond to results of statistical analysis #'comparing features between two conditions (conditions specified thanks to the data.frame metadata) 
#'
#'@param conv_df a data.frame that gives the corresponding name of each feature in another nomenclature when needed (for example ensembl gene ID and usual symbol gene name). The first column should #'correspond to names in the the nomenclature used in the matrix given as value of the slot counts. Each character chain in this column should be unique. The second column contains corresponding names in #'the other  nomenclature. The same name can be found several times in this column.
#' 
#'
#'@param colcond a character chain (class character) that gives the name of the column of metadata that should be use to specify the condition associated to each sample.
#' 
#'@param colsignificativity a character chain (class character) that gives the name of the column in each data.frame that provides the level of significativity of the difference of 
#' a feature between two conditions.
#' 
#'@param significativity_symbols a list (class list) that gives the symbol associated to each level of significativity when printing violinplot.
#' 
#'@param description a character chain (character chain) that allows to describe for instance how the dataset has been generated. Can be useful when the dataset has been generated a long time ago to store important informations about the generation of the data (computational and statistical methods used for example as well as the version of these tools)
#'
#'@author Elie Robert
#'
#' @examples
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' FOXA2_RNAseq <- datasettorep(counts = expr_mat, 
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)), 
#' colcond = "condition", 
#' colsignificativity = "adj.P.Val" , 
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata")
#'
#' str(FOXA2_RNAseq)
#' 
#'@export
#'


datasettorep <- function(counts,metadata,stattest = list(),colcond,colsignificativity = character(),
conv_df = data.frame(),
significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
description = ""){#accolade ouvrante de la fonction constructeur grand public dataseToRep

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Datasettorep : builder ~~~~~~~~~~~~~~~~~~ \n")

return(new("Datasettorep",counts = counts,metadata = metadata,stattest = stattest,colcond = colcond,colsignificativity = colsignificativity,conv_df = conv_df,significativity_symbols = significativity_symbols,description = description))

#accolade fermante de la fonction constructeur grand public dataseToRep
}
