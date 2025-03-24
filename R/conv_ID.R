#' To obtain correspondances between 2 features nomenclatures
#'
#' @description Function that allows to give the name of features (here generally genes) to another nomenclature.
#'
#' @rdname conv_ID
#'
#' @return a dataframe of two columns with in the first column the feature as provided by the user as input of the argument vec_id and in the second column the name of the feature in the other nomenclature.
#'
#' @param vec_id a vector of class character chain. Each element is the name of a feature in a specific nomenclature whose the user wants to know his name in another nomenclature.
#'
#' @param convTab a dataframe of two columns with in the first column the nomenclature of a large set of features (typically the set of all genes in a specific nomenclature such as Ensembl) 
#' In this column, features are in the nomenclature corresponding to those used as input of vec_id. The second column corresponds to the other nomenclature which will interest the user. 
#'
#' @author Elie Robert
#'
#' @examples
#'
#' conv_dataframe <- GTF_ensembl_to_conv_df(GTF_homo_sapiens_example())
#'
#' conversion <- conv_ID(vec_id = c("KIT","GATA1","GATA2","NFE2"),convTab = conv_dataframe)
#'
#' head(conversion)  
#'
#'@export
#'
#'

conv_ID <- function(vec_id,convTab){
#ci dessus accolade ouvrante de la fonction conv_ID defini dans un setMethod  

merging <- merge(
data.frame(id =convTab[which(convTab[[1]] %in% vec_id),1], 
nom = convTab[which(convTab[[1]] %in% vec_id),2]),data.frame(id = vec_id),by = "id")
  
merging <- merging[match(vec_id,merging$id),]

# conversion en colonne de classe character
merging[,1] <- as.character(merging[,1])
merging[,2] <- as.character(merging[,2])
  
# on renvoie merging
return(merging)

#ci dessous accolade fermante de la fonction conv_ID defini dans un setMethod
}

