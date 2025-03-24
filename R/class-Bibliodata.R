####################################### Bibliodata ####################' The Bibliodata class 


#' The Bibliodata S4 R class
#' @description An object of class Bibliodata allows to store several objects of class Datasettorep. Moreover, it allows to launch a shiny app which is designed to represent violinplots and heatmaps.
#' @slot biblio a list (class list) of objects of class Datasettorep.
#' @name Bibliodata-class
#' @rdname Bibliodata-class
#' @exportClass Bibliodata



setClass("Bibliodata",slots = c(biblio = "list"))

setValidity("Bibliodata",function(object){
#accolade ouvrante setValidity

# Generation d une list qui contiendra les warnings eventuels qui pourront etre affiche a l utilisateur dans le cas ou l instance de l objet est quand meme genere mais 
# que certains elements sont susceptibles de poser des problemes a l utilisateur par la suite.


print("creation de la list qui contiendra les messages de warning eventuels")
warning_message <- vector(mode = "list", length = 1)
warning_message <- lapply(warning_message,function(x){return("")})


#Generation d une list qui contiendra les eventuels messages d erreurs a afficher.
print("Creation de la list qui contiendra les messages d erreurs eventuels.")
error <- vector(mode = "list", length = 3)
error <- lapply(error,function(x){return("")})



##########################################  Verification que la list fournie comme attribut biblio n est pas vide (0 element). #######################################################

if(length(object@biblio)==0){#opening bracket instruction if the length of the list provided as attribute biblio is of length zero.

error[[1]] <- paste0("The length of the list provided as attribute biblio cannot be zero.")

stop("The length of the list provided as attribute biblio cannot be zero.")

#closing bracket instruction if the length of the list provided as attribute biblio is of length zero
}


#verification que tous les elements de l attribut biblio de la class Bibliodata sont de class Datasettorep 

##################################### 2) verification que tous les elements de l attribut biblio de la class Bibliodata sont bien de class Datasettorep. ####################################
if(all(unlist(lapply(object@biblio,class))=="Datasettorep")==FALSE){
#accolade ouvrante test conditionnel

error[[2]] <- paste0("all elements of the list provided as value of the attribute biblio should be of class Datasettorep")

#accolade fermante test conditionnel
}else{

print("all objects in the list attribute biblio are of class Datasettorep as required")

}

################################ 2) Verification que tous les elements de la list en plus d etre de class Datasettorep ont la meme valeur de l attribut significativity_symbols (mais si un ou plusieurs elements ont une list
#vide comme valeur de cette attribut, ces elements ne  sont pas pris en compte. Aucun message d'erreur ne s affichera parce que tous les elements ne sont pas des list vides. C'est sur les 
#elements qui ne sont pas des lists vides que se portera le test) ################################################

significativity_symbols_list <- lapply(object@biblio,function(t){

return(t@significativity_symbols)

})

#controle du fonctionnement du code
print("voici la structure de significativity_symbols_list")
print(str(significativity_symbols_list))


empty_positions <- lapply(1:length(significativity_symbols_list),function(z){#accolade ouvrante de la fonction du lapply qui permet de generer not_empty_positions

return(identical(significativity_symbols_list[[z]],list()))

})

print("voici la structure de empty_positions")
print(str(empty_positions))


#on transforme en vecteur la list empty_positions generee ci-dessus
empty_positions <- unlist(empty_positions)

if(any(empty_positions == TRUE)){#accolade ouvrante instructions si au moins un element de not_empty_positions vaut TRUE (associe a une list vide) 

#on vire les lists vides de significativity_symbols_list.
significativity_symbols_list_not_empty <- significativity_symbols_list[which(empty_positions==FALSE)] 

#accolade fermante instructions si au moins un element de not_empty_positions vaut TRUE (associe a une list vide)
}else{#accolade ouvrante instructions dans le cas contraire

significativity_symbols_list_not_empty <- significativity_symbols_list

#accolade fermante instructions dans le cas contraire.
}

print("Voici la structure de significativity_symbols_list_not_empty")
print(str(significativity_symbols_list_not_empty))


if(length(significativity_symbols_list_not_empty)>1){# Opening bracket instructions if at this stage the length of significativity_symbols_list_not_empty is superior to 1.

print("Voici la structure de significativity_symbols_list_not_empty")
print(str(significativity_symbols_list_not_empty))

significativity_symbols_list_not_empty_only_symbols <- lapply(significativity_symbols_list_not_empty,function(z){#accolade ouvrante de la fonction du lapply
#permettant de ne recuperer que le premier element de l attribut significativity_symbols de chaque objet.

return(z[[1]])

#accolade fermante de la fonction du lapply permettant de ne recuperer que le premier element de l attribut significativity_symbols de chaque objet.
})

significativity_symbols_list_not_empty_only_numbers <- lapply(significativity_symbols_list_not_empty,function(z){#accolade ouvrante de la fonction du lapply
#permettant de ne recuperer que le second element de l attribut significativity_symbols de chaque objet.

return(z[[2]])

#accolade fermante de la fonction du lapply permettant de ne recuperer que le second element de l attribut significativity_symbols de chaque objet.
})

#list de booleens destinee a tester si tous les attributs significativity_symbols de chaque objet de class Datasettorep constituant l objet de class Bibliodata 
#que l on veut creer ont leurs premiers elements respectifs qui sont egaux (en omettant les objets eventuels pour lesquels l attribut significativity_symbols est une list vide)
equality_symbols_vec <- lapply(2:length(significativity_symbols_list_not_empty_only_symbols),function(z){ #accolade ouvrante de la fonction du lapply
return(identical(significativity_symbols_list_not_empty_only_symbols[[1]],significativity_symbols_list_not_empty_only_symbols[[z]]))
#accolade fermante de la fonction du lapply
})

# On transforme la list de booleens generee ci-dessus en vecteur de booleens.
equality_symbols_vec <- unlist(equality_symbols_vec)

#list de booleens destinee a tester si tous les attributs significativity_symbols de chaque objet de class Datasettorep constituant l objet de class Bibliodata 
#que l on veut creer ont leurs deuxiemes elements respectifs qui sont egaux (en omettant les objets eventuels pour lesquels l attribut significativity_symbols est une list vide)
equality_numbers_vec <- lapply(2:length(significativity_symbols_list_not_empty_only_numbers),function(z){ #accolade ouvrante de la fonction du lapply
return(identical(significativity_symbols_list_not_empty_only_numbers[[1]],significativity_symbols_list_not_empty_only_numbers[[z]]))
#accolade fermante de la fonction du lapply
})

# On transforme la list de booleens generee ci dessus en vecteur de booleens. 
equality_numbers_vec <- unlist(equality_numbers_vec)

if(all(equality_symbols_vec == TRUE) == FALSE || all(equality_numbers_vec == TRUE) == FALSE){#accolade ouvrante instructions si toutes les lists attributs significativity_symbols 
#des objets constituant l objet Bibliodata ne sont pas identiques

error[[3]] <- paste0("All objects of class Datasettorep inside an object of class Bibliodata should have the same attribute significativity_symbols")


#accolade fermante instructions si toutes les lists attributs significativity_symbols des objets constituant l objet Bibliodata ne sont pas identiques.
}


# Closing bracket instructions if at this stage the length of significativity_symbols_list_not_empty is superior to 1.
}





################################################ verification qu il n'y a pas de problemes incompatibles avec la generation de l objet Bibliodata desire ###################

if( all(unlist(error)== "" )==FALSE){
#accolade ouvrante instructions si tous les elements de la list error ne sont pas des chaines de caracteres de longueur nulle

messagetoprint <- do.call(c,error)

stop(paste0(messagetoprint))

#accolade fermante instructions si tous les elements de la list error ne sont pas des chaines de caracteres de longueur nulle.
}else{#accolade ouvrante instructions si tous les elements de la list error valent "". Gestion des warnings. On definit et on affiche si necessaire les warnings qui peuvent etre utile

if(is.null(names(object@biblio))==TRUE){#accolade ouvrante instructions si les elements de la list correspondant a l attribut biblio ne sont pas nommes

warning_message[[1]] <- paste0("Elements of the list provided as value of the attribute biblio are not named. By default if a shiny app is generated, datasets will be named dataset_1, dataset_2 etc...")

#accolade fermante instructions si les elements de la list correspondant a l attribut biblio ne sont pas nommes. 
}

#generation d un message de warning si des elements de la list correspondant a l attribut biblio sont nommes "". A faire demain.







if(all(unlist(warning_message)=="")==FALSE){#accolade ouvrante instructions si un ou plusieurs elements de l instance generee sont peut etre susceptibles de poser des problemes a l utilisateur.
#merite en tout cas d etre souligne

warningtoprint <- do.call(c,warning_message)
print("voici warningtoprint")
print(warningtoprint)
if(any(warningtoprint =="")==TRUE){#s il y a des warnings qui n ont pas a etre affiche
warningtoprint <- warningtoprint[-which(warningtoprint == "")]
#s il y a des warnings qui n ont pas a etre affiche. 
}
warningtoprint <- paste0(warningtoprint,collapse = " \n")
warningtoprint <- paste0(warningtoprint, "\n The instance of the class Datasettorep is still created but be careful.")
#on affiche les messages de warnings



warning(warningtoprint)

#accolade fermante Un ou plusieurs elements de l instance generee sont peut etre susceptibles de poser des problemes a l utilisateur.
#merite en tout cas d etre souligne
}

#on renvoie ce boolean pour que l instance de class Bibliodata quand toutes les conditions pour que celle ci puisse etre genere sont remplies.
return(TRUE)

#accolade fermante  instructions instructions si tous les elements de la list error valent "".  gestion des warnings. On definit et on affiche si necessaire les warnings qui peuvent etre utiles.
}

#accolade fermante du setValidity
}

#parenthese fermante du setValidity
)

