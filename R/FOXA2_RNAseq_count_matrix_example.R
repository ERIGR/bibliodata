#' @title FOXA2_RNAseq_count_matrix_example
#'
#' @description This function gives the path to an rds file containing a matrix corresponding to a bulk RNAseq experiment (GSE119931).
#' The data consists of 3 Wild Type (WT) samples and 3 Knock Outs (KO) of the transcription factor FOXA2.
#' 
#' @export
#' @examples
#' 
#' FOXA2_RNAseq_count_matrix_example()
#'


FOXA2_RNAseq_count_matrix_example <- function(){

    
    print(system.file("extdata","GSE119931_normalized_counts_limma_FOXA2.rds", package = "bibliodata", mustWork = TRUE))
    
    system.file("extdata","GSE119931_normalized_counts_limma_FOXA2.rds", package = "bibliodata", mustWork = TRUE)


}
