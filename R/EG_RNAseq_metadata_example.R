#' @title EG_RNAseq_metadata_example
#'
#' @description This function gives the path to an rds file containing a metadata dataframe corresponding to a bulk RNAseq experiment.
#' Samples are HEL (Human Erythroleukemia lineage) cells either control (3 samples), either carrying the ETO2::GLIS2 fusion, either carrying the ETO2::GLIS2 
#' without the zinc finger of the GLIS2 part of the ETO2-GLIS2 fusion or carrying the fusion ETO2::GLIS2 without the dNHR2 part of the ETO2-GLIS2 fusion  
#' (dimerization domain of the ETO2 part of the ETO2-GLIS2 fusion).    
#' 
#' @export
#' 
#' @examples
#' 
#' EG_RNAseq_metadata_example()
#'


EG_RNAseq_metadata_example <- function(){

    print(system.file("extdata","metadata_EG_HEL.rds", package = "bibliodata", mustWork = TRUE))
    
    system.file("extdata","metadata_EG_HEL.rds", package = "bibliodata", mustWork = TRUE)

}
