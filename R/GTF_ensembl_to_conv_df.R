#'
#' @title GTF_ensembl_to_conv_df
#'
#' @description This function allows to generate a dataframe that can be use as slot conv_df of an object of class Datasettorep from a GTF file. 
#'  This function is useful if the user wants to store and represent bulkRNAseq data.
#'
#' @param PathFileGTF a character chain that gives the path to a GTF file.
#'
#' @return an object of class dataframe that can be used as slot conv_df of an object of class Datasettorep. 
#'
#' @importFrom rtracklayer import
#'
#' @export
#'
#' @examples
#'
#' require(readr)
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
#' description = "RNAseq of HEL cells (cell 
#' line derived from a patient 
#' suffering of an erythroleukemia). 
#' There is four conditions is this dataset : 
#' control HEL cells, HEL cells carrying the ETO2-GLIS2 
#' fusion (introduced by CRISPR-Cas9 editing), 
#' HEL cells carrying the ETO2-GLIS2 fusion 
#' but with a truncated C265G domain 
#' (zinc finger of the GLIS2 part of the protein), 
#' HEL cells carrying the ETO2-GLIS2 
#' fusion but with a truncation of the dimerization 
#' domain of the ETO2 part of the protein 
#' (Aid et al, Leukemia, 2022 ; Thirant et al, cancer Cell 2017). 
#' Quantification of reads has been performed 
#' with Salmon and subsequent analysis 
#' (Variance Stabilizing Transformation to obtain 
#' the normalized matrix and 
#' the differential expression analysis) 
#' have been performed by DESeq2)"
#' )
#'
#' 


GTF_ensembl_to_conv_df <- function(PathFileGTF){#accolade ouvrante de la fct GTF_ensembl_to_conv_df

#importation du fichier GTF grace a la fonction import de rtracklayer
gtf_file <- rtracklayer::import(paste0(PathFileGTF))

#on recupere le tableau de donnees dans l objet genere ci dessus.
genes_conv <- gtf_file@elementMetadata

#on convertit genes_conv en dataframe.
genes_conv <- as.data.frame(genes_conv)

#On convertit la colonne gene_id de genes_conv en dataframe. 
genes_conv$gene_id <- as.factor(genes_conv$gene_id)

#generation d un dataframe qui donne pour chaque gene_id
convdf <- aggregate(genes_conv$gene_name,list(gene_ID = genes_conv$gene_id),unique)

#la premiere colonne est nomme gene_id 
colnames(convdf)[1] <- "gene_id"

#La seconde colonne est nomme gene_name.
colnames(convdf)[2] <- "gene_name"

#on renvoie le dataframe qui peut etre utilise comme attribut de conv_df.
return(convdf)
#accolade fermante de la fct GTF_ensembl_to_conv_df
}
