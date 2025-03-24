#' @title FOXA2_differential_dataframe_example
#'
#' @description This function gives the path to an rds file containing the differential analysis results between two conditions (WT and FOXA2KO) to a bulk RNAseq experiment (GSE119931).
#' The data consists of 3 Wild Type (WT) samples and 3 Knock Outs (KO) of the transcription factor FOXA2.
#' 
#'
#' @examples
#' 
#' FOXA2_differential_dataframe_example()
#' 
#' @export
#'
#' 
#'


FOXA2_differential_dataframe_example <- function(){

    
    print(system.file("extdata","GSE119931_differential_limma_FOXA2.rds", package = "bibliodata", mustWork = TRUE))
    
    system.file("extdata","GSE119931_differential_limma_FOXA2.rds", package = "bibliodata", mustWork = TRUE)


}
