#' The Datasettorep S4 R class 
#' @description An object of class Datasettorep allows to store a dataset and to generate basic representations based on the stored dataset, that is violinplot and heatmap representations. 
#' @slot counts The matrix (class matrix) that stored values of each variables (rows) for each sample (column)
#' @slot metadata a data.frame (class data.frame) that gives the condition associated to each sample (rows). This data.frame should contain at least a column (whose name is given by the slot colcondition) #' that gives the condition associated to each sample. Rownames of the metadata data.frame should correspond to colnames of the matrix counts. (samples should be ranged in the same order as in the matrix #' counts)
#' @slot stattest a list (class list). Each element of this list is a data.frame (class data.frame) with at least one column (whose name is given the slot colsignificativity) that gives the level of  #'significativity (for instance, p-value or fdr) of the difference between two conditions for each feature. That is why each element of the list should correspond to results of statistical analysis #'comparing features between two conditions (conditions specified thanks to the data.frame metadata) 
#' @slot conv_df a data.frame that gives the corresponding name of each feature in another nomenclature when needed (for example ensembl gene ID and usual symbol gene name). The first column should #'correspond to names in the the nomenclature used in the matrix given as value of the slot counts. Each character chain in this column should be unique. The second column contains corresponding names in #'the other  nomenclature. The same name can be found several times in this column.
#' @slot colcond a character chain (class character) that gives the name of the column of metadata that should be use to specify the condition associated to each sample.
#' @slot colsignificativity a character chain (class character) that gives the name of the column in each data.frame that provides the level of significativity of the difference of 
#' a feature between two conditions.
#' @slot significativity_symbols a list (class list) that gives the symbol associated to each level of significativity when printing violinplot.
#' @slot description a character chain (character chain) that allows to describe for instance how the dataset has been generated. Can be useful when the dataset has been generated a long time ago to store important informations about the generation of the data (computational and statistical methods used for example as well as the version of these tools)
#' @name Datasettorep-class
#' @rdname Datasettorep-class
#' @concept objects
#' @exportClass Datasettorep


########################################################### Definition of the class Datasettorep ####################################################

setClass("Datasettorep",slots = c(counts = "matrix",metadata = "data.frame",stattest = "list",conv_df = "data.frame",colcond = "character",colsignificativity = "character",significativity_symbols = "list",description = "character"))


################################################################## Global setValidity ##################################################


setValidity("Datasettorep",function(object){#accolade ouvrante setValidity


# Generation d une list qui contiendra les warnings eventuels qui pourront etre affiche a l utilisateur dans le cas ou l instance de l objet est quand meme genere mais 
# que certains elements sont susceptibles de poser des problemes a l utilisateur par la suite.
warning_message <- vector(mode = "list", length = 5)
warning_message <- lapply(warning_message,function(x){return("")})


#Generation d une list qui contiendra les eventuels messages d erreurs a afficher.
error <- vector(mode = "list", length = 39)
error <- lapply(error,function(x){return("")})


##### examen structure et contenu de la matrice ###########################################

### Verification de la class de counts
if(("matrix" %in% class(object@counts)) == FALSE){
#accolade ouvrante instructions si counts n est pas de class matrix

error[[1]] <- paste0("The object counts should be of class matrix. \n")

#accolade fermante instructions si counts n est pas de class matrix
}


### Verification qu il n y a pas de na dans la matrice
if(any(is.na(object@counts))==TRUE){
#accolade ouvrantes instructions s il y a des na dans la matrice counts


error[[2]] <- paste0("The matrix counts cannot contain NA values. \n")


#accolade fermante instructions s il y a des na dans la matrice counts
}


#### Verification qu il n y a pas de valeurs Inf dans la matrice
if(any(is.infinite(object@counts))==TRUE){#accolade ouvrante instructions si counts contient des valeurs infinis 


error[[3]] <- paste0("The matrix cannot contains infinite values. \n")

#accolade fermante instructions si counts contient des valeurs infinis.
}

################ examen structure et contenu du data.frame de metadata
##### Verification 

if(inherits(object@metadata,"data.frame")==FALSE){
#accolade ouvrante instructions si metadata n est pas un dataframe

error[[4]] <- paste0("The object metadata must be of class data.frame. \n")

#accolade fermante instructions si metadata n est pas de class dataframe
}

#On verifie que le dataframe metadata n est pas vide

if(rlang::is_empty(object@metadata)==TRUE){

error[[5]] <- paste0("The dataframe metadata cannot be empty. \n")

}

if(any(is.na(object@metadata))==TRUE){
#accolade ouvrante instructions s il y a des na dans le dataframe de metadata


error[[6]] <- paste0("The data frame metadata cannot contain NA values. \n")


#accolade fermante instructions s il y a des na dans le dataframe de metadata.
}


if(any(apply(object@metadata,2,class) %in% c("character","factor"))==FALSE){
#accolade ouvrante instructions si aucune colonne de metadata n est de class character ou factor


error[[7]] <- paste0("At least one column of metadata should be of class character or factor. \n")


#accolade fermante instructions si aucune colonne de metadata n est de class character ou factor
}


############################################ examen du contenu et de la structure de stattest 

#Verification de la class de stattest

if(inherits(object@stattest,"list")==FALSE){#accolade ouvrante de stattest

error[[8]] <- paste0("The object stattest should be a list. \n")

#accolade fermante de stattest
}


###################### examen de la class de colcond 

if(inherits(object@colcond,"character")==FALSE){#accolade ouvrante instructions si colcond n est pas de class character
error[[9]] <- paste0("The attribute colcond should be of class character. \n")
#accolade fermante instructions si colcond n est pas de class character
}


######################## examen de la class de significativity_symbols

if(inherits(object@significativity_symbols,"list")==FALSE){#accolade ouvrante instruction si significativity_symbols n est pas de class list

error[[10]] <- paste0("You should provide an object of class list as input of the attribute significativity_symbols")

#accolade fermante instruction si significativity_symbols n est pas de class list
}


########### examen de colsignificativity

if(inherits(object@colsignificativity,"character")==FALSE){#accolade ouvrante verification de la class de l attribut colsignificativity


stop("The value of the attribute colsignificativity should be of class character.")

error[[11]] <- paste0("The value of the attribute colsignificativity should be of class character. \n")

#accolade fermante verification de la class de l attribut colsignificativity
}


######################## examen lien entre les attributs colcond et metadata

if( (length(object@colcond)!=1) || (all(object@colcond %in% colnames(object@metadata))==FALSE) || (all(class(object@metadata[[object@colcond]]) %in% c("character","factor"))==FALSE)){
#accolade ouvrante instruction si l attribut colcond ne correspond pas au nom d une colonne de l attribut metadata ou que la colonne ainsi nomme n est pas de class character ou factor

error[[12]] <- paste0("The attribute colcond should correspond to the name of a column (only 1 !) of metadata which should be either of class character or factor. \n")

#on renvoie directement une erreur car certains tests logiques ne fonctionneront pas si le test logique ci dessus ne fonctionne pas.
stop(paste0("The attribute colcond should correspond to the name of a column (only 1 !) of metadata which should be either of class character or factor. \n"))

#accolade fermante instruction si l attribut colcond ne correspond pas au nom d une colonne de l attribut metadata ou que la colonne ainsi nomme n est pas de class character ou factor
}

####################### Verification de la correspondance entre les noms de lignes de metadata et les noms de colonnes de counts

#on verifie que les noms de lignes de metadata correspondent aux noms de colonnes de counts. 

if(all(rownames(object@metadata)==colnames(object@counts))==FALSE){#accolade ouvrante instructions si les noms de lignes de metadata ne correspondent pas aux noms de colonnes de counts


error[[13]] <- paste0("The rownames of the attribute metadata should be the same than colnames of the attribute counts (in the same order). \n")

#accolade fermante instructions si les noms de lignes de metadata ne correspondent pas aux noms de colonnes de counts.
}

######################

#on s assure qu aucun element de la colonne de metadata qui specifie le nom de la condition associe a chaque echantillon ne contient la chaine de caracteres _vs_
if(length(grep("_vs_",object@metadata[[object@colcond]]))!=0){#accolade ouvrante instructions si un element de la colonne de metadata qui permet la specification du nom des conditions contient la chaine de
# caracteres _vs_

error[[14]] <- paste0("No elements of the column of the dataframe of the attribute metadata that specify conditions can contain the character chain _vs_ . \n")

#accolade fermante instructions si un element de la colonne de metadata qui permet la specification du nom des conditions contient la chaine de _vs_
}

#verification que les noms des elements de la list de tests statistiques ne commence pas ou ne termine pas par la chaine de caracteres _vs_


if(length(object@stattest) != 0){
#accolade ouvrante si la longueur de la list stattest est differente de 0

if(length(grep( "^\\_vs_|\\_vs_$",names(object@stattest))) != 0){
#accolade ouvrante instruction si un nom de stattest commence ou termine par la chaine de caracteres _vs_

error[[15]] <- paste0("The names of the elements of the list stattest should not begin or end by the character chain _vs_ . \n")

#accolade fermante instruction si un nom de stattest commence ou se termine par la chaine de caracteres stattest _vs_
}






if(all(sapply(strsplit(names(object@stattest),"_vs_"),length)==2)==FALSE){
#accolade ouvrante instructions si la chaine de caracteres _vs_ n est pas presente ou est  presente plus d une fois dans au moins un des noms des elements de la list stattest 

error[[16]] <- paste0("The character pattern _vs_ should be present but only one time in each name of elements of the list stattest to separate names of conditions compared. \n")

#accolade fermante instructions si la chaine de caracteres _vs_ n est pas presente ou est presente plus d une fois dans au moins un des noms des elements de la list stattest
}






#controle que les noms de conditions compares dans stattest correspondent bien a des noms de conditions specifies dans la colonne donnant les conditions associees a chaque echantillon dans metadata



#controle que les noms de conditions compares dans stattest correspondent bien a des noms de conditions specifies dans la colonne donnant les conditions associees a chaque echantillon dans metadata
if(all(do.call(c,strsplit(names(object@stattest),"_vs_")) %in% unique(as.character(object@metadata[[object@colcond]])))==FALSE){#accolade ouvrante si un ou plusieurs noms de conditions dans les noms des elements de la list stattest ne correspondent pas a des noms de conditions specifies dans la colonne specifiant les vrais noms des conditions dans le dataframe metadata

error[[17]] <- paste0("names of conditions separated by the pattern _vs_ in names of elements of the list stattest should correspond to a name of condition associated to some samples that is given in a specified column of metadata. \n")

#accolade fermante si un ou plusieurs noms de conditions dans les noms des elements de la list stattest ne correspondent pas a des noms de conditions dans la colonne specifiant les vrais noms des conditions dans le dataframe metadata de stattest 
}







#il faut verifier qu une condition ne soit pas comparees a elle meme. 
if(any(sapply(strsplit(names(object@stattest),"_vs_"),function(z){
    
    if(length(z)==2){
    
    return(z[1]==z[2])
    
    }else{return(FALSE)}
    
    
    }))==TRUE){
#accolade ouvrante instructions si une condition semble compare a elle meme

#une condition ne peut pas etre comparee a elle meme
error[[18]] <- paste0("A condition cannot be compared to herself. \n")


#accolade fermante instructions si une condition semble comparee a elle meme
}


# Gerer si plusieurs elements de la list stattest sont nommes de telle sorte qu il semblerait qu une meme comparaison soit associee a plusieurs elements de la list

if(any(unlist(lapply(strsplit(names(object@stattest),"_vs_"),function(z){

return(all(c(paste0(z[1],"_vs_",z[2]),paste0(z[2],"_vs_",z[1])) %in% names(object@stattest) == TRUE)



)}))==TRUE)==TRUE){#accolade ouvrante 
#Erreur a renvoyer si plusieurs elements de la list stattest sont nommes de telle sorte qu il semblerait qu une meme comparaison soit associee a plusieurs elements de la list.

error[[19]] <- "each comparisons should be unique. Several elements of the attribute stattest are named in such way that it seems there is several dataframes of results for the same comparisons"

#accolade fermante 
#Erreur a renvoyer si plusieurs elements de la list stattest sont nommes de telle sorte qu il semblerait qu une meme comparaison soit associee a plusieurs elements de la list.
}




if(length(object@colsignificativity)!=1){#accolade ouvrante si le vecteur correspondant a l attribut colsignificativity est de longueur differente de 1


error[[20]] <- paste0("The attribute colsignificativity should be of length equal to 1 (this is a single character chain) if the length of the attribute stattest is not null. \n")


#si le test logique ci dessus est verifie (et on affiche le message d erreur associe a ce test tout de suite), on arrete directement le code car certains tests logiques ulterieures ne pourront pas etre appliques.
stop(paste0("The attribute colsignificativity should be of length equal to 1 (this is a single character chain) if the length of the attribute stattest is not null. \n"))

#accolade fermante si le vecteur correspondant a l attribut colsignificativity est de longueur differente de 1
}



############ Verification que tous les dataframes de stattest contiennent une colonne dont le nom correspond a la chaine de caracteres associee a l attribut colsignificativity



if(all(sapply(object@stattest,function(z){class(z) == "data.frame"}))==FALSE){
#accolade ouvrante instructions si il existe au moins un element de stattest qui n est pas de class data.frame 

error[[21]] <- paste0("All elements of stattest should be of class data frame. \n")

#accolade fermante instructions si il existe au moins un element de stattest qui n est pas de class data.frame
}



if(all(sapply(object@stattest,function(z){

return(any(colnames(z)==object@colsignificativity))

})

)==FALSE){
#accolade ouvrante instructions si au moins un dataframe de stattest ne contient pas une colonne dont le nom ne correspond pas a la chaine de caracteres associee a la variable colsignificativity

error[[22]] <- paste0("Every dataframe of stattest should contain a column named ", object@colsignificativity, " \n")

#accolade fermante instructions si au moins un dataframe de stattest ne contient pas une colonne dont le nom ne correspond pas a la chaine de caracteres associee a la variable colsignificativity
}

if(all(sapply(object@stattest,function(z){class(z[[object@colsignificativity]])=="numeric"}))==FALSE){
#accolade ouvrante instructions s il existe au moins un dataframe de stattest 
#dont la colonne portant le nom correspondant a la chaine de caractere associee a la variable colsignificativity n est pas de class numeric


error[[23]] <- paste0("Column with name associated to the attribute colsignificativity should be of class numeric. \n")

#accolade fermante instructions s il existe au moins un dataframe de stattest dont la colonne portant le nom correspondant a la chaine de caracteres colsignificativity n est pas de class numeric
}


if(all(unlist(lapply(object@stattest,function(z){all(rownames(object@counts) %in% rownames(z))}))==TRUE)==FALSE){#accolade ouvrante instructions si pour un feature un gene de la matrice de counts
#dans au moins un des elements dataframe de la list stattest, il n y a pas de lignes qui correspondent a ce feature gene (pas de stats pour ce feature pour une comparaison donnee) 

error[[24]] <- paste0("All rownames of the attribute counts should correspond to a rowname in each dataframe of the list attribute stattest (if the attribute stattest is not of length 0). \n")


#accolade fermante instructions si pour un feature un gene de la matrice de counts
#dans au moins un des elements dataframe de la list stattest, il n y a pas de lignes qui correspondent a ce feature gene (pas de stats pour ce feature pour une comparaison donnee) 
}






if(length(object@significativity_symbols)!=2){#accolade ouvrante si l attribut significativity_symbols n est pas une list de longueur 2 


stop("If the attribute stattest is not empty that is not of length 0; the attribute significativity_symbols should be a list of length 2.")

#alors on renvoie un message d erreur
error[[25]] <- paste0("If the attribute stattest is not empty that is not of length 0; the attribute significativity_symbols should be a list of length 2. \n")


#accolade fermante si l attribut significativity_symbols
}


if(inherits(object@significativity_symbols[[1]],"character")==FALSE){
#accolade ouvrante instruction si le premier element de l attribut list significativity_symbols n est pas de class character

error[[26]] <- paste0("if the attribute stattest is not empty that is not of length 0, the first element of the list attribute significativity_symbols should be of class character. \n")

#accolade fermante instruction si le premier element de l attribut list significativity_symbols n est pas de class character
}


if(inherits(object@significativity_symbols[[2]],"numeric")==FALSE){
#accolade ouvrante instruction si le second element de l attribut list significativity_symbols n est pas de class numeric


error[[27]] <- paste0("if the attribute stattest is not empty that is not of length 0, the second element of the list attribute significativity_symbols should be of class numeric. \n")

#accolade fermante instruction si le second element de l attribut list significativity_symbols n est pas de class numeric
}


if(any(is.na(object@significativity_symbols[[1]]))==TRUE){
#accolade ouvrante instruction si le premier element de l attribut list significativity_symbols contient des NA 

error[[28]] <- paste0("if the attribute stattest is not empty that is not of length 0, the first element of the list attribute significativity_symbols should not contain NA. \n")

#accolade fermante instruction si le dernier element de l attribut list significativity_symbols contient des NA.
}

if(any(is.na(object@significativity_symbols[[2]]))==TRUE){
#accolade ouvrante instruction si le second element de l attribut list significativity_symbols contient des NA 

error[[29]] <- paste0("if the attribute stattest is not empty that is not of length 0, the second element of the list attribute significativity_symbols should not contain NA. \n")

#accolade fermante instruction si le second element de l attribut list significativity_symbols contient des NA.
}


if(length(object@significativity_symbols[[1]])<2){
#accolade ouvrante instruction si le premier element de l attribut list significativity_symbols n est pas au moins de longueur 2.    

error[[30]] <- paste0("if the attribute stattest is not empty that is not of length 0, the first element of the list attribute significativity_symbols should be at least of length 2 (character vector). \n")

#accolade fermante instruction si le second element de l attribut list significativity_symbols n est pas au moins de longueur 2.
}

if(length(object@significativity_symbols[[2]])!= (1+length(object@significativity_symbols[[1]]))){
#accolade ouvrante instruction si le second element de l attribut list significativity_symbols n est pas d une longueur egale a la longueur du premier element de l attribut list significativity_symbols + 1.  

error[[31]] <- paste0("if the attribute stattest is not empty that is not of length 0, the second element of the list attribute significativity_symbols should be exactly equal to 1 + length of the first element of significativity_symbols. \n")

#accolade fermante instruction si le second element de l attribut list significativity_symbols n est pas d une longueur egale a la longueur du premier element de l attribut list significativity_symbols + 1.
}


if(all(object@significativity_symbols[[2]] == sort(object@significativity_symbols[[2]],na.last = NA))==FALSE){
#accolade ouvrante instruction si les valeurs dans le second element de l attribut list significativity_symbols ne sont pas tries dans l ordre croissant.

error[[32]] <- paste0("if the attribute stattest is not empty that is not of length 0, the numerical values in the second element of the list attribute significativity_symbols should be sort (increasing order). \n")

#accolade  fermante instruction si les valeurs dans le second element de l attribut list significativity_symbols ne sont pas tries dans l ordre croissant
}

###### ecriture transitoire du code permettant de controler le champs des valeurs possibles dans les colonnes colsignificativity de stattest

if(all(unlist(lapply(object@stattest,function(z){

if(min(z[[object@colsignificativity]],na.rm = TRUE) < min(object@significativity_symbols[[2]],na.rm = TRUE) || max(z[[object@colsignificativity]],na.rm = TRUE) > max(object@significativity_symbols[[2]],na.rm = TRUE)){

return("at least one value outside of the possible range of values. \n")

}else{

return("all values inside the range of possible values")

}



}))== "all values inside the range of possible values")==FALSE){


error[[33]] <- paste0("in each element of the attribute stattest values in the column designed by the attribute colsignificativity should be inside the interval defined by the minimal and the maximal values of the second element of the attribute significativity_symbols. \n")


}


#accolade fermante instructions si la longueur de stattest est differente de 0 
}



if(length(object@stattest)==0){
#accolade ouvrante condition a remplir si la list stattest est vide.

if(identical(object@colsignificativity,character()) == FALSE){#accolade ouvrante instructions si colsignificativity est different de character() alors que la list correspondant a l attribut stattest est de longueur nulle


error[[34]] <- paste0("As the stattest attribute is of length 0, no colname can be given as a value of the attribute colsignificativity. Therefore the attribute colsignificativity should be empty that is equal to character(). \n")

#accolade fermante instructions si colsignificativity est different de NULL alors que la list correspondant a l attribut stattest est de longueur nulle
}


if(identical(object@significativity_symbols,list()) == FALSE){
#accolade ouvrante instruction si l attribut significativity_symbols n est pas vide alors que la longueur de stattest est de 0

error[[35]] <- paste0("As the stattest attribute is of length 0 (empty), there is no statistics available so it is not possible to show the significativity associated to differences between conditions. Therefore the attribute colsignificativity should be empty that is equal to list(). \n")

#accolade fermante instruction si l attribut significativity_symbols n est pas vide alors que la longueur de stattest est de 0 
}

#accolade fermante conditions a remplir si la list stattest est vide
}

###################### verification qu un certain nombre de conditions sont bien remplies par le dataframe qui est fourni en tant qu attribut conv_df.

if(rlang::is_empty(object@conv_df)==FALSE){#accolade ouvrante code execute si conv_df n est pas un dataframe vide

if(ncol(object@conv_df)!=2){#accolade ouvrante instructions si le dataframe conv_df n est pas constitue exactement de deux colonnes.

error[[36]] <- paste0("The dataframe provided as input of the attribute conv_df should contain exactly 2 columns. \n")

#accolade fermante instructions si le dataframe conv_df n est pas constitue exactement de deux colonnes.
}

if(all(apply(object@conv_df,2,class)=="character")==FALSE){#accolade ouvrante instructions si les deux colonnes de conv_df ne sont pas de class character

error[[37]] <- paste0("The two columns of the dataframe provided as input of the attribute conv_df should be of class character.\n")

#accolade fermante instructions si les deux colonnes de conv_df ne sont pas de class character. 
}



if(length(unique(object@conv_df[[1]]))!= length(object@conv_df[[1]])){#accolade ouvrante  instructions si tous les elements de la premiere colonne ne sont pas presents
# qu une seule fois au sein de cette colonne (cad que certains elements sont presents plusieurs fois dans la colonne)

error[[38]] <- paste0("The character chains in the first column of the dataframe provided as input of the attribute conv_df should all be different from others. That is, each element of the colum should be present only one time in this column. \n")

#accolade fermante instructions si tous les elements de la premiere colonne ne sont pas presents qu une seule fois au sein de cette colonne (cad que certains elements sont presents plusieurs fois dans la colonne)
}

if(all(rownames(object@counts) %in% object@conv_df[[1]])==FALSE){
#instructions si tous les noms de lignes de la matrice correspondant a l attribut counts ne correspondent pas a une chaine de caracteres dans la premiere colonne du dataframe correspondant a l attribut conv_df 

error[[39]] <- paste0("all rownames of the matrix given as input of the attribute counts should correspond to a character chain in the first column of the dataframe associated to the attribute conv_df. \n")

#instructions si tous les noms de lignes de la matrice correspondant a l attribut counts ne correspondent pas a une chaine de caracteres dans la premiere colonne du dataframe correspondant a l attribut conv_df                    
}

#accolade fermante code execute si conv_df n est pas un dataframe vide
}



if( all(unlist(error)== "" )==FALSE){
#accolade ouvrante instructions si tous les elements de la list error ne sont pas des chaines de caracteres de longueur nulle

messagetoprint <- do.call(c,error)

stop(paste0(messagetoprint))

#accolade fermante instructions si tous les elements de la list error ne sont pas des chaines de caracteres de longueur nulle.
}else{
#accolade ouvrante instruction si tous les elements de la list error sont des chaines de caracteres de longueur nulle


#################################### check de criteres susceptibles de produire des messages de warnings ########################################
if(all(unlist(lapply(object@stattest,function(z){#accolade ouvrante de la fonction du lapply 

#renvoyer FALSE si toutes les valeurs de la colonne padj ne sont pas differentes de NA.
return(all(is.na(z[[object@colsignificativity]])==FALSE))

#accolade fermante de la fonction du lapply
}))==FALSE)){#accolade ouvrante instructions si au moins un dataframe contient des NA dans la colonne donnee par colsignificativity

warning_message[[1]] <- paste0("In at least one dataframe of stattest, the column ",object@colsignificativity," contains NA values. These NA are considered as equivalent to not significant differences between implicated condition for features associated to them.")

#accolade fermante instructions si au moins un dataframe contient des NA dans la colonne donne par colsignificativity
}


############################ si l attribut colsignificativity est associe a une ou plusieurs valeurs superieures a 1. 

if(all(unlist(lapply(object@stattest,function(z){ ## accolade ouvrante de la fonction du lapply

return(all(z[[object@colsignificativity]] <= 1,na.rm = TRUE))

## accolade fermante de la fonction du lapply
})))==FALSE){#accolade ouvrante instructions si au moins un dataframe contient une ou plusieurs valeurs superieures a 1 dans la colonne dont le nom correspond a l attribut colsignificativity


warning_message[[2]] <- paste0("In at least one dataframe of stattest, the column ", object@colsignificativity, " contains one or several values higher than 1. If these values are supposed to be probabilities, this is strange ...")


#accolade fermante instructions si au moins un dataframe contient une ou plusieurs valeurs superieures a 1 dans la colonne dont le nom correspond a l attribut colsignificativity.
}

###################### si l attribut colsignificativity est associe a une ou plusieurs valeurs negatives ####################""

if(all(unlist(lapply(object@stattest,function(z){ ## accolade ouvrante de la fonction du lapply

return(all(z[[object@colsignificativity]] >= 0,na.rm = TRUE))

## accolade fermante de la fonction du lapply
})))==FALSE){#accolade ouvrante instructions si au moins un dataframe contient une ou plusieurs valeurs superieures a 1 dans la colonne dont le nom correspond a l attribut colsignificativity


warning_message[[3]] <- paste0("In at least one dataframe of stattest, the column ", object@colsignificativity , " contains one or several values lower than 0. If these values are supposed to be probabilities, this is strange ...")


#accolade fermante instructions si au moins un dataframe contient une ou plusieurs valeurs superieures a 1 dans la colonne dont le nom correspond a l attribut colsignificativity.
}

######################## si l attribut significativity_symbols a son second element contient des valeurs inferieures a 0 ou des valeurs superieures a 1 on avertit qu a priori certaines
################ valeurs ne correspondent pas a des probabilites.

if(length(object@stattest)!=0){
#accolade ouvrante verifications a faire uniquement si la longueur de stattest est differente de 0.

if(any(object@significativity_symbols[[2]] < 0) == TRUE || any(object@significativity_symbols[[2]] > 1) == TRUE){#accolade ouvrante instruction si une valeur n est pas comprise entre 0 et 1.


warning_message[[4]] <- paste0("Some values in the second element of significativity_symbols seem to not be probability values because they are either inferior to 0 or superior to 1.")


#accolade fermante instruction si une valeur n est pas comprise entre 0 et 1.
}


###################### si certains dataframes elements de l'attribut stattest contiennent des noms de lignes qui ne correspondent a aucun nom de ligne de la matrice correspondant a l attribut 
########## counts

if(all(unlist(lapply(object@stattest,function(z){return(all(rownames(z) %in% rownames(object@counts)))}))==TRUE)==FALSE){
#accolade ouvrante instructions si certains dataframes de l attribut list stattest contiennent des noms de ligne qui ne correspondent a aucun nom de ligne de la matrice correspondant a l attribut counts

warning_message[[5]] <- paste0("For at least one dataframe of the list attribute stattest, there is rownames that are not matching with any rownames of the matrix attribute counts")


#accolade fermante instructions si certains dataframes de l attribut list stattest contiennent des noms de ligne qui ne correspondent a aucun nom de ligne de la matrice correspondant a l attribut counts.
}

#accolade fermante verifications a faire uniquement si la longueur de stattest est differente de 0. 
}


###################################### code pour afficher les warnings si necessaires #################################################
if(all(unlist(warning_message)=="")==FALSE){#accolade ouvrante instructions si un ou plusieurs elements de l instance generee sont peut etre susceptibles de poser des problemes a l utilisateur.
#merite en tout cas d etre souligne

warningtoprint <- do.call(c,warning_message)

warningtoprint <- warningtoprint[-which(warningtoprint == "")]
warningtoprint <- paste0(warningtoprint,collapse = " \n")
warningtoprint <- paste0(warningtoprint, "\n The instance of the class Datasettorep is still created but be careful.")
#on affiche les messages de warnings



warning(warningtoprint)

#accolade fermante instructions si un ou plusieurs elements de l instance generee sont peut etre susceptibles de poser des problemes a l utilisateur.
#merite en tout cas d etre souligne
}

#on renvoie TRUE car on veut que l instance de l objet que souhaite l utilisateur soit genere.
return(TRUE)

#accolade fermante instruction si tous les elements de la list error sont des chaines de caracteres de longueur nulle.
}



################################################ accolade fermante setValidity ###############################################
}
####################################################### parenthese fermante setValidity ########################################################
)




