### file where generics are defined ###########


#### generics plutot Datasettorep 

#' Accessor for the 'counts' slot of a Datasettorep object. 
#'
#' @description Accessors for the 'counts' slot of a Datasettorep object. The counts slot holds the count data as a matrix. The matrix (class matrix) contains values for each variables (rows) for each #' sample #' (column) get_counts allows to retrieve counts data from an object of class Datasettorep.
#'
#' @rdname get_counts-methods
#'
#' @docType methods
#' 
#' @param object \code{Datasettorep} object
#'
#' @return matrix (class matrix) containing the counts data (rows corresponds to variables and columns to samples)
#'
#' @author Elie Robert
#'
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "adj.P.Val",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#'
#' get_counts(FOXA2_RNAseq)
#'
#' @export
#'
setGeneric("get_counts",function(object){standardGeneric("get_counts")})

#' @rdname get_counts-methods
#' @aliases get_counts,Datasettorep
#' @export 

setMethod("get_counts","Datasettorep",function(object){return(object@counts)})


#' Accessor for the 'metadata' slot of a \code{Datasettorep} object
#' @description get_metadata allows to retrieve metadata from an object of class Datasettorep
#' The metadata slot holds a data.frame (class data.frame) that gives the condition associated to each sample (rows).
#' This data.frame should contain at least a column (whose name is given by the slot \code{colcondition}) 
#' that gives the condition associated to each sample. 
#' Rownames of the metadata data.frame should correspond to colnames of the matrix counts
#' (samples should be ranged in the same order as in the matrix counts).
#'
#'
#' @rdname get_metadata-methods
#'
#' @docType methods
#'
#' @param object a \code{Datasettorep} object
#'
#' @return a data.frame containing the metadata associated to the counts data. 
#' This data.frame gives the condition associated to each sample (rows).
#' This data.frame contains at least a column (whose name is given by the slot \code{colcondition}) 
#' that gives the condition associated to each sample.
#' Rownames of the metadata data.frame corresponds to colnames of the matrix counts.
#'
#'
#' @author Elie Robert
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "adj.P.Val",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#'
#' get_metadata(FOXA2_RNAseq)
#'
#' @export

setGeneric("get_metadata",function(object){standardGeneric("get_metadata")}) 

#' @rdname get_metadata-methods
#' @aliases get_metadata,Datasettorep
#' @export

setMethod("get_metadata","Datasettorep",function(object){return(object@metadata)})






#' Accessor for the 'stattest' slot of a \code{Datasettorep} object
#'
#' @description The stattest slot is a list (class list). Each element of this list is a data.frame (class data.frame) 
#' with at least one column (whose name is given the slot colsignificativity) 
#' that gives the level of significance (for instance, p-value or fdr) 
#' of the difference between two conditions for each feature. 
#' That is why each element of the list should correspond to results of statistical analysis 
#' comparing features between two conditions 
#' (conditions specified thanks to the data.frame corresponding to the slot \code{metadata})
#' getstattest allows to retrieve the list of statistical results from an object of class Datasettorep. 
#'
#'
#' @param object \code{Datasettorep} object
#'
#'
#' @rdname getstattest-methods
#'
#' @docType methods
#'
#' @author Elie Robert
#'
#'
#' @return a R list. Each element of this list is a data.frame (class data.frame) 
#' with at least one column (whose name is given the slot colsignificativity)
#' that gives the level of significance (for instance, p-value or fdr) of the difference 
#' between two conditions for each feature.
#' Each element of the list corresponds to results of statistical analysis 
#' comparing features between two conditions
#' (conditions specified thanks to the data.frame corresponding to the slot \code{metadata})
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "adj.P.Val",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#'
#' getstattest(FOXA2_RNAseq)
#'
#' @export

setGeneric("getstattest",function(object){standardGeneric("getstattest")})

#' @rdname getstattest-methods
#' @aliases getstattest,Datasettorep
#' @export

setMethod("getstattest","Datasettorep",function(object){return(object@stattest)})







#'
#' Accessor for the 'colcond' slot of a \code{Datasettorep} object.
#'
#' @description The slot 'colcond' is a character chain (R class character) that gives the name of the column of the slot \code{metadata} 
#' that should be use to specify the condition associated to each sample.
#' getcolcond allows to retrieve the name of the column of the metadata slot of a Datasettorep object that defines conditions associated to each condition.
#'
#'
#' @param object a \code{Datasettorep} object
#'
#' @return a character chain that gives the name of the column of the slot \code{metadata} that is used to specify the condition associated to each sample.
#' 
#' @rdname getcolcond-methods
#'
#' @docType methods
#'
#' @author Elie Robert
#'
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
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "adj.P.Val",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#'
#'
#' getcolcond(FOXA2_RNAseq)   
#'  
#' @export

setGeneric("getcolcond",function(object){standardGeneric("getcolcond")})

#' @rdname getcolcond-methods
#' @aliases getcolcond,Datasettorep
#' @export

setMethod("getcolcond","Datasettorep",function(object){return(object@colcond)})



#' 
#'
#' Accessor for the 'colsignificativity' slot of a \code{Datasettorep} object.
#'
#' @description getcolsignificativity allows to retrieve the name of the column that define the level of significativity 
#' of each test in dataframes of the slot stattest.
#' The slot colsignificativity is a character chain (class character) that gives the name of the column in each data.frame 
#' of the slot \code{stattest} that provides the level of significance of the difference of 
#' a feature between two conditions.
#'
#' @rdname getcolsignificativity-methods
#'
#' @docType methods
#' 
#' @param object a \code{Datasettorep} object
#'
#' @return a character chain (the 'colsignificativity' slot of a \code{Datasettorep} object) that gives the name of the column in each data.frame 
#' of the slot \code{stattest} that provides the level of significance of the difference of 
#' a feature between two conditions.
#'
#' @author Elie Robert
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "adj.P.Val",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#'
#'
#' getcolsignificativity(FOXA2_RNAseq)
#'
#'
#' @export


setGeneric("getcolsignificativity",function(object){standardGeneric("getcolsignificativity")}) 


#' @rdname getcolsignificativity-methods
#' @aliases getcolsignificativity,Datasettorep
#' @export
#'
setMethod("getcolsignificativity","Datasettorep",function(object){return(object@colsignificativity)})

# setMethod("getcolsignificativity","Datasettorep",function(object){return(object@colsignificativity)})


#' Accessor for the 'convdf' slot of a \code{Datasettorep} object.
#'
#' @description The slot convdf is a data.frame (R class data.frame) that gives the corresponding name of each feature in another nomenclature 
#' when needed (for example ensembl gene ID and usual symbol gene name). 
#' The first column should correspond to names in the the nomenclature used in the matrix given as value of the slot counts. 
#' Each character chain in this column should be unique. The second column contains corresponding names in the other  nomenclature. 
#' The same name can be found several times in this column.
#'
#'
#' @param object a \code{Datasettorep} object
#'
#' @return a data.frame (the slot convdf of the \code{Datasettorep} object)that gives the corresponding name of each feature in another nomenclature when needed (for example ensembl gene ID and usual symbol gene name).
#'
#' @author Elie Robert
#'
#' @rdname getconv_df-methods
#' 
#' @docType methods
#'
#' @examples
#'
#' require(readr)
#' normalized_counts_EG <- read_rds(EG_RNAseq_count_matrix_example())
#'
#' metadata_EG <- read_rds(EG_RNAseq_metadata_example())
#'
#' DESeq2_EG <- read_rds(EG_RNAseq_differential_analysis_example())
#'
#' conv_dataframe <- GTF_ensembl_to_conv_df(GTF_homo_sapiens_example())
#'
#' EG_HEL_dataset  <- new("Datasettorep",
#' counts = normalized_counts_EG,
#' metadata = metadata_EG,
#' stattest = DESeq2_EG,
#' colcond = "condition",
#' colsignificativity = "padj",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' conv_df = conv_dataframe,
#' description = "RNAseq of HEL cells (cell line derived from 
#' a patient suffering of an erythroleukemia). 
#' There is four conditions is this dataset : control HEL cells, HEL cells carrying the ETO2-GLIS2 
#' fusion (introduced by CRISPR-Cas9 editing), 
#' HEL cells carrying the ETO2-GLIS2 fusion but with a 
#' truncated C265G domain (zinc finger of the GLIS2 part of the protein), 
#' HEL cells carrying the ETO2 GLIS2 fusion 
#' but with a truncation of the dimerization domain of the ETO2 part of the protein
#' (Aid et al, Leukemia, 2022 ; Thirant et al, cancer Cell 2017). 
#' Quantification of reads has been performed #' with Salmon and subsequent analysis 
#' (Variance Stabilizing Transformation to obtain the normalized matrix 
#' and the differential expression analysis) have been performed by DESeq2)")
#'
#' getconv_df(EG_HEL_dataset)
#'
#' @export

setGeneric("getconv_df",function(object){standardGeneric("getconv_df")})

#' @rdname getconv_df-methods
#' @aliases getconv_df,Datasettorep
#' @export

setMethod("getconv_df","Datasettorep",function(object){return(object@conv_df)})






#' Accessor (getter) for the 'significativity_symbols' slot of a \code{Datasettorep} object.
#' 
#' @description getsignificativity_symbols allows to retrieve the slot significativity_symbols of a Datasettorep object.
#' This method allows to get the value of the 'significativity_symbols' slot of a \code{Datasettorep} object. 
#'
#' significativity_symbols is a list (R class list) that gives the symbol associated to 
#' each level of significativity when printing violinplot. This list is composed of two elements : the first one is a character vector that gives the symbols that will be used 
#  in violinplot representations to indicate the degree of significance of the difference between two conditions. 
#' The second vector is a numerical vector that specifies the limit values to choose what symbols will be use. 
#' For example, if a user provides the list list(c("*","NS"),c(0,0.05,1)) as a value of the parameter significativity_symbols, this means that differences associated
#' to a degree of significance between 0 and 0.05 will be represented with the symbol (character chain) * and differences associated to a degree of significance between 0.05 and 1 
#  will be represent by the character chain NS (in this example, the degree of significance is associated to the metric of p-value or FDR). Numerical values in the second element of the list
#' should be ranked in ascending order.
#'
#' 
#'
#' @rdname getsignificativity_symbols-methods
#'
#' @docType methods 
#'
#' @param object a \code{Datasettorep} object
#'
#' @return a R list that gives the symbol associated to each level of significativity when printing violinplot.
#'
#' @author Elie Robert
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "adj.P.Val",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#'
#' getsignificativity_symbols(FOXA2_RNAseq) 
#'
#'
#' @export

setGeneric("getsignificativity_symbols",function(object){standardGeneric("getsignificativity_symbols")})

#' @rdname getsignificativity_symbols-methods
#' @aliases getsignificativity_symbols,Datasettorep
#' @export

setMethod("getsignificativity_symbols","Datasettorep",function(object){return(object@significativity_symbols)})



#################### setteurs verifier ce qu il faut mettre pour rdname

#' 
#' Accessor (setter) for the 'colsignificativity' slot of a \code{Datasettorep} object.
#' @description setcolsignificativity allows to give the name of the column that will be use to give the level of significativity of a statistical test between two conditions. 
#' This method allows to change the value of the 'colsignificativity' slot of a \code{Datasettorep} object.
#' The slot colsignificativity is a character chain (class character) that gives the name of the column in each data.frame 
#' of the slot \code{stattest} that provides the level of significance of the difference of 
#' a feature between two conditions.
#'
#' @rdname setcolsignificativity-methods
#'
#' @docType methods
#'
#' @param object a \code{Datasettorep} object
#'
#' @param value a character chain which corresponds to the column of each dataframe of the slot 'stattest' that gives the level 
#' of significance of the difference between two conditions for each feature.
#'
#' @return an object of class \code{Datasettorep} with eventually a modified value of the slot colsignificativity. 
#'
#' @author Elie Robert
#'
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "P.Value",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#' 
#' getsignificativity_symbols(FOXA2_RNAseq)
#'
#' setcolsignificativity(FOXA2_RNAseq) <- "adj.P.Val"
#'
#' getsignificativity_symbols(FOXA2_RNAseq)
#'
#' @export

setGeneric("setcolsignificativity<-",function(object,value){standardGeneric("setcolsignificativity<-")})

#' @rdname setcolsignificativity-methods
#' @aliases setcolsignificativity,Datasettorep,character
#' @export
setReplaceMethod(
 f = "setcolsignificativity",
 signature="Datasettorep",
 definition = function(object,value){
 
object@colsignificativity <- value
validObject(object)
return(object)
}
)


#' setter for the significativity_symbols slot
#' @description setsignificativity_symbols allows to specify the list that give symbols associated to each level of significance (used in violinplot representations)
#' This method allows to change the value of the 'significativity_symbols' slot of a \code{Datasettorep} object.
#' significativity_symbols is a list (R class list) that gives the symbol associated to 
#' each level of significativity when printing violinplot. This list is composed of two elements : the first one is a character vector that gives the symbols that will be used 
#  in violinplot representations to indicate the degree of significance of the difference between two conditions. 
#' The second vector is a numerical vector that specifies the limit values to choose what symbols will be use. 
#' For example, if a user provides the list list(c("*","NS"),c(0,0.05,1)) as a value of the parameter significativity_symbols, this means that differences associated
#' to a degree of significance between 0 and 0.05 will be represented with the symbol (character chain) * and differences associated to a degree of significance between 0.05 and 1 
#  will be represent by the character chain NS (in this example, the degree of significance is associated to the metric of p-value or FDR). Numerical values in the second element of the list
#' should be ranked in ascending order.
#'
#' @param object an object of class \code{Datasettorep} object.
#'
#' @param value a R list. This list is composed of two elements : the first one is a character vector that gives the symbols that will be used 
#  in violinplot representations to indicate the degree of significance of the difference between two conditions. 
#' The second vector is a numerical vector that specifies the limit values to choose what symbols will be use.
#' For example, if a user provides the list list(c("*","NS"),c(0,0.05,1)) as a value of the parameter significativity_symbols, this means that differences associated
#' to a degree of significance between 0 and 0.05 will be represented with the symbol (character chain) * and differences associated to a degree of significance between 0.05 and 1 
#  will be represent by the character chain NS (in this example, the degree of significance is associated to the metric of p-value or FDR). Numerical values in the second element of the list
#' should be ranked in ascending order.
#'
#' 
#' @return an object of class Datasettorep with eventually a modified slot significativity_symbols if necessary. 
#'
#' @rdname setsignificativity_symbols-methods
#'
#' @docType methods
#'
#' @author Elie Robert
#'
#' @examples
#'
#' require(readr)
#'
#' expr_mat <- read_rds(FOXA2_RNAseq_count_matrix_example()) 
#'
#' diff_results <- read_rds(FOXA2_differential_dataframe_example())
#'
#' rownames(diff_results) <- diff_results$ID
#'
#' ## génération d un objet de class Datasettorep associé au jeu de données GLIS RNAseq 
#' FOXA2_RNAseq <- new("Datasettorep",
#' counts = expr_mat,
#' metadata = data.frame(condition = c(rep("WT",3),rep("FOXA2ko",3)),row.names = colnames(expr_mat)),
#' stattest = list("WT_vs_FOXA2ko" = diff_results),
#' colcond = "condition",
#' colsignificativity = "P.Value",
#' significativity_symbols = list(c("*","NS"),c(0,0.05,1)),
#' description = "The normalized counts matrix (limma normalization) for this dataset 
#' (GSE119931; Balestrieri C et al, 2019, EMBO J) 
#' has been retrieved from the github of the package DecoupleR.
#' https://github.com/saezlab/decoupleR/tree/master/inst/extdata ")
#' 
#' getsignificativity_symbols(FOXA2_RNAseq)
#' 
#' setsignificativity_symbols(FOXA2_RNAseq) <- list(c("***","**","NS"),c(0,0.01,0.05,1))
#'
#' getsignificativity_symbols(FOXA2_RNAseq)
#'
#' @export

setGeneric("setsignificativity_symbols<-",function(object,value){standardGeneric("setsignificativity_symbols<-")})

#' @rdname setsignificativity_symbols-methods
#' @aliases setsignificativity_symbols,Datasettorep,list
#' @export

setReplaceMethod(
 f = "setsignificativity_symbols",
 signature="Datasettorep",
 definition = function(object,value){
 
 object@significativity_symbols <- value
 validObject(object)
 return(object)
 
 }
)


#################### generics plutot Bibliodata ##################

#' Retrieve a specific Datasettorep object from a Bibliodata object 
#'
#' @description get_specific_datasettorep allows to retrieve a specific Datasettorep object from a Bibliodata object.
#'
#' @param object an object of class Bibliodata
#'
#' @param value character chain or numeric value
#' 
#' @name get_specific_datasettorep
#'
#' @export

#' @param object an object of class Bibliodata
#'
#' @param value either of class character or of class vector. The name provided as value of the argument value should be the name of an element of the list corresponding 
#' to the attribute biblio of the concerned Bibliodata object otherwise it should be the index of the element you want to select
#'
#' @return a single datasettorep object
#'
#' @rdname get_specific_datasettorep-methods
#'
#' @docType methods
#' 
#' @examples
#'
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
#'
#' normalized_counts_EG <- read_rds(EG_RNAseq_count_matrix_example())
#'
#' metadata_EG <- read_rds(EG_RNAseq_metadata_example())
#'
#' DESeq2_EG <- read_rds(EG_RNAseq_differential_analysis_example())
#'
#' conv_dataframe <- GTF_ensembl_to_conv_df(GTF_homo_sapiens_example())
#'
#' EG_HEL_dataset  <- new("Datasettorep",
#' counts = normalized_counts_EG,
#' metadata = metadata_EG,
#' stattest = DESeq2_EG,
#' colcond = "condition",
#' colsignificativity = "padj",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' conv_df = conv_dataframe,
#' description = "RNAseq of HEL cells (cell line derived from a 
#' patient suffering of an erythroleukemia). 
#' There is four conditions is this dataset : control HEL cells, 
#' HEL cells carrying the ETO2-GLIS2 
#' fusion (introduced by CRISPR-Cas9 editing), 
#' HEL cells carrying the ETO2-GLIS2 fusion but with a truncated C265G domain 
#' (zinc finger of the GLIS2 part of the protein), 
#' HEL cells carrying the ETO2 #'GLIS2 fusion but with 
#' a truncation of the dimerization domain of the ETO2 part of the protein
#' (Aid et al, Leukemia, 2022 ; Thirant et al, cancer Cell 2017). 
#' Quantification of reads has been performed #' with Salmon and subsequent analysis 
#' (Variance Stabilizing Transformation to obtain the normalized matrix 
#' and the differential expression analysis) have been performed by DESeq2)")
#'
#' example_bibliodata <- bibliodata(biblio = list("FOXA2_RNAseq" = FOXA2_RNAseq,
#' "EG_HEL" = EG_HEL_dataset))
#' 
#' 
#'
#' str(get_specific_datasettorep(example_bibliodata,"FOXA2_RNAseq")) 
#'
#' str(get_specific_datasettorep(example_bibliodata,1)) 
#'
#' @export

setGeneric("get_specific_datasettorep",function(object,value){standardGeneric("get_specific_datasettorep")})

#' @rdname get_specific_datasettorep-methods
#' @aliases get_specific_datasettorep,Bibliodata,character
#' @export

setMethod("get_specific_datasettorep",signature(object = "Bibliodata", value = "character"),function(object,value){#accolade ouvrante du setMethod
    
    if(class(value) %in% c("character","numeric")==FALSE){#opening bracket instruction if the argument value is not of class character or numeric.
    
    stop("The argument value should be either of class character or numeric")
    
    #closing bracket instruction  if the argument value is not of class character or numeric.
    }

    if(length(value) != 1){#opening bracket instruction if the length of the value of the argument value is not equal to 1

     stop("Only 1 element of class Datasettorep can be selected with the method get_specific_datassettorep")


    #closing bracket instruction if the length of the value of the argument value is not equal to 1.
    }


    if(inherits(value,"numeric")==TRUE){#opening bracket if the argument value is of class numeric
          

    if(value %in% 1:length(object@biblio) == FALSE){

    stop("The argument value should be the index of the element you want to select if it is of class numeric otherwise it should be the name of the element that you want to select")

    }


    #closing bracket instructions if the argument value is of class numeric
    }else{#opening bracket instructions if the argument value is of class character

         if(value %in% names(object@biblio) == FALSE){#opening bracket instruction if the name provided as value of the argument value does 
         #not correspond to the name of an element of the list corresponding to the attribute biblio of the object Bibliodata

        stop("The name provided as value of the argument value should be the name of an element of the list corresponding to the attribute biblio of the concerned Bibliodata object otherwise it should be the index of the element you want to select")

         # closing bracket instruction if the name provided as value of the argument value 
         # does not correspond to the name of an element of the list corresponding to the attribute biblio of the object Bibliodata.
         }
        
    # closing bracket instructions if the argument value is of class character
    }




    return(object@biblio[[value]])
    
    #accolade fermante du setMethod
    })
    
#' @rdname get_specific_datasettorep-methods
#' @aliases get_specific_datasettorep,Bibliodata,numeric
#' @export
    
    
    setMethod("get_specific_datasettorep",signature(object = "Bibliodata", value = "numeric"),function(object,value){#accolade ouvrante du setMethod
    
    if(class(value) %in% c("character","numeric")==FALSE){#opening bracket instruction if the argument value is not of class character or numeric.
    
    stop("The argument value should be either of class character or numeric")
    
    #closing bracket instruction  if the argument value is not of class character or numeric.
    }

    if(length(value) != 1){#opening bracket instruction if the length of the value of the argument value is not equal to 1

     stop("Only 1 element of class Datasettorep can be selected with the method get_specific_datassettorep")


    #closing bracket instruction if the length of the value of the argument value is not equal to 1.
    }


    if(inherits(value,"numeric")==TRUE){#opening bracket if the argument value is of class numeric
          

    if(value %in% 1:length(object@biblio) == FALSE){

    stop("The argument value should be the index of the element you want to select if it is of class numeric otherwise it should be the name of the element that you want to select")

    }


    #closing bracket instructions if the argument value is of class numeric
    }else{#opening bracket instructions if the argument value is of class character

         if(value %in% names(object@biblio) == FALSE){#opening bracket instruction if the name provided as value of the argument value does 
         #not correspond to the name of an element of the list corresponding to the attribute biblio of the object Bibliodata

        stop("The name provided as value of the argument value should be the name of an element of the list corresponding to the attribute biblio of the concerned Bibliodata object otherwise it should be the index of the element you want to select")

         # closing bracket instruction if the name provided as value of the argument value 
         # does not correspond to the name of an element of the list corresponding to the attribute biblio of the object Bibliodata.
         }
        
    # closing bracket instructions if the argument value is of class character
    }




    return(object@biblio[[value]])
    
    #accolade fermante du setMethod
    })

    

#' getter function that allows to subset a Bibliodata object
#' @description subset_bibliodata allows to subset a Bibliodata object to obtain a smaller Bibliodata object.
#' This function is a getter function that allows to subset a Bibliodata object and generate a new object of the same class. This object is a smaller bibliodata object that corresponds to a subset of #' the original bibliodata object.
#'
#' @param object an object of class Bibliodata the user wants to subset.
#'
#' @param sel Character vector either of class character or of class numeric. It corresponds either to a set of names of elements of class Datasettorep contained in the Bibliodata object or to a set #' #' of index (positions) of Datasettorep objects in the Bibliodata object.
#'
#' @return an object of class Bibliodata.
#'
#' @rdname subset_bibliodata-methods
#'
#' @docType methods
#'
#'
#' @author Elie Robert
#'
#' @examples
#'
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
#'
#' normalized_counts_EG <- read_rds(EG_RNAseq_count_matrix_example())
#'
#' metadata_EG <- read_rds(EG_RNAseq_metadata_example())
#'
#' DESeq2_EG <- read_rds(EG_RNAseq_differential_analysis_example())
#'
#' conv_dataframe <- GTF_ensembl_to_conv_df(GTF_homo_sapiens_example())
#'
#' EG_HEL_dataset  <- new("Datasettorep",
#' counts = normalized_counts_EG,
#' metadata = metadata_EG,
#' stattest = DESeq2_EG,
#' colcond = "condition",
#' colsignificativity = "padj",
#' significativity_symbols = list(c("***","**","NS"),c(0,0.01,0.05,1)),
#' conv_df = conv_dataframe,
#' description = "RNAseq of HEL cells (cell line derived from 
#' a patient suffering of an erythroleukemia). 
#' There is four conditions is this dataset : control HEL cells, HEL cells carrying the ETO2-GLIS2 
#' fusion (introduced by CRISPR-Cas9 editing), 
#' HEL cells carrying the ETO2-GLIS2 fusion but with a 
#' truncated C265G domain (zinc finger of the GLIS2 part of the protein), 
#' HEL cells carrying the ETO2 GLIS2 fusion 
#' but with a truncation of the dimerization domain of the ETO2 part of the protein
#' (Aid et al, Leukemia, 2022 ; Thirant et al, cancer Cell 2017). 
#' Quantification of reads has been performed #' with Salmon and subsequent analysis 
#' (Variance Stabilizing Transformation to obtain the normalized matrix 
#' and the differential expression analysis) have been performed by DESeq2)")
#'
#' example_bibliodata <- bibliodata(biblio = list("FOXA2_RNAseq" = FOXA2_RNAseq,
#' "EG_HEL" = EG_HEL_dataset))
#' 
#'
#' str(subset_bibliodata(example_bibliodata,"FOXA2_RNAseq"))
#'
#' str(subset_bibliodata(example_bibliodata,1))
#'
#' @export
#'  

setGeneric("subset_bibliodata",function(object,sel){standardGeneric("subset_bibliodata")})

#' @rdname subset_bibliodata-methods
#' @aliases subset_bibliodata,Bibliodata,character
#' @export

setMethod("subset_bibliodata",signature(object = "Bibliodata", sel = "character"),function(object,sel){#opening bracket of the function that gives the definition of the method subset_bibliodata.

    if(class(sel) %in% c("character","numeric")==FALSE){#opening bracket instruction if the argument value is not of class character or numeric.
    
    stop("The argument sel should be either of class character or numeric")
    
    #closing bracket instruction  if the argument value is not of class character or numeric.
    }

    if(length(unique(sel))!= length(sel)){#opening bracket instructions if the length of all unique elements of the vector sel are not equal to the length of sel 


     stop("Every element of the value of the argument sel should be unique in sel")


    # closing bracket instructions if the length of all unique elements of the vector sel are not equal to the length of sel.
    }

    if(is.numeric(sel)==TRUE){#opening bracket instructions if sel is of class numeric

    if(all(sel %in% 1:length(object@biblio))==FALSE){#opening bracket if some or all positions defined in the vector sel cannot be found in object@biblio
 
     stop("Some element(s) cannot be selected because positions given are either higher than the length of the list or correspond to negative values or null value")

    # closing bracket if some or all positions defined in the vector sel cannot be found in object@biblio.
    }


    #closing bracket instructions if sel is of class numeric
    }else{#accolade ouvrante de la list
     if(is.character(sel) == TRUE){#accolade ouvrante si sel est un vecteur de class character.

    if(all(sel %in% names(object@biblio))== FALSE){#opening bracket : instructions if some names in the vector sel do not correspond to any names of the list attribute object@biblio.

    stop("Some element(s) cannot be selected because character chain(s) provided do(es) not correspond to any names of elements of the list attribute object@biblio.")


    #closing bracket : instructions if some names in the vector sel do not correspond to any names of elements of the list attribute object@biblio.
    }
    #closing bracket if sel is a vector of class character.
     }}


selected <- lapply(1:length(sel),function(z){#opening bracket of the function of the lapply.


return(object@biblio[[sel[z]]])


#closing bracket of the function of the lapply.
})

if(is.null(names(object@biblio))==TRUE){#opening bracket instructions if there is no names associated to elements of the list attribute biblio.

names(selected) <- NULL

# closing bracket instructions if there is no names associated to elements of the list attribute biblio.
}else{#opening bracket instructions if there is names associated to elements of the list attribute biblio.

names_selected <- lapply(1:length(sel),function(z){#opening bracket of the function of the lapply.


return(names(object@biblio[sel[z]]))


#closing bracket of the function of the lapply.
})  

names(selected) <- unlist(names_selected)

rm(names_selected)

#closing bracket instructions if there is names associated to elements of the list attribute biblio.
}

return(new("Bibliodata",biblio = selected))

#closing bracket of the function that gives the definition of the method subset_bibliodata
})


#' @rdname subset_bibliodata-methods
#' @aliases subset_bibliodata,Bibliodata,numeric
#' @export

setMethod("subset_bibliodata",signature(object = "Bibliodata", sel = "numeric"),function(object,sel){#opening bracket of the function that gives the definition of the method subset_bibliodata.

    if(class(sel) %in% c("character","numeric")==FALSE){#opening bracket instruction if the argument value is not of class character or numeric.
    
    stop("The argument sel should be either of class character or numeric")
    
    #closing bracket instruction  if the argument value is not of class character or numeric.
    }

    if(length(unique(sel))!= length(sel)){#opening bracket instructions if the length of all unique elements of the vector sel are not equal to the length of sel 


     stop("Every element of the value of the argument sel should be unique in sel")


    # closing bracket instructions if the length of all unique elements of the vector sel are not equal to the length of sel.
    }

    if(is.numeric(sel)==TRUE){#opening bracket instructions if sel is of class numeric

    if(all(sel %in% 1:length(object@biblio))==FALSE){#opening bracket if some or all positions defined in the vector sel cannot be found in object@biblio
 
     stop("Some element(s) cannot be selected because positions given are either higher than the length of the list or correspond to negative values or null value")

    # closing bracket if some or all positions defined in the vector sel cannot be found in object@biblio.
    }


    #closing bracket instructions if sel is of class numeric
    }else{#accolade ouvrante de la list
     if(is.character(sel) == TRUE){#accolade ouvrante si sel est un vecteur de class character.

    if(all(sel %in% names(object@biblio))== FALSE){#opening bracket : instructions if some names in the vector sel do not correspond to any names of the list attribute object@biblio.

    stop("Some element(s) cannot be selected because character chain(s) provided do(es) not correspond to any names of elements of the list attribute object@biblio.")


    #closing bracket : instructions if some names in the vector sel do not correspond to any names of elements of the list attribute object@biblio.
    }
    #closing bracket if sel is a vector of class character.
     }}


selected <- lapply(1:length(sel),function(z){#opening bracket of the function of the lapply.


return(object@biblio[[sel[z]]])


#closing bracket of the function of the lapply.
})

if(is.null(names(object@biblio))==TRUE){#opening bracket instructions if there is no names associated to elements of the list attribute biblio.

names(selected) <- NULL

# closing bracket instructions if there is no names associated to elements of the list attribute biblio.
}else{#opening bracket instructions if there is names associated to elements of the list attribute biblio.

names_selected <- lapply(1:length(sel),function(z){#opening bracket of the function of the lapply.


return(names(object@biblio[sel[z]]))


#closing bracket of the function of the lapply.
})  

names(selected) <- unlist(names_selected)

rm(names_selected)

#closing bracket instructions if there is names associated to elements of the list attribute biblio.
}

return(new("Bibliodata",biblio = selected))

#closing bracket of the function that gives the definition of the method subset_bibliodata
})































