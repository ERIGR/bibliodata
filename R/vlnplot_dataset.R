
#' Method to generate violinplots based on a \code{Datasettorep} object.
#'
#' @rdname vlnplot_dataset
#'
#' @param data a \code{Datasettorep} object
#' @param genetorepresent vector of class character. Each element in the vector corresponds to the name of a feature the user wants to represent (violinplot representation)
#' @param ordercondition vector of class character. Each element corresponds to the name of a condition in the dataset that the user wants to represent. 
#' Violins associated to each condition will be printed in the same order as in the vector.
#' @param color_to_fill vector of class character. Each color corresponds to the color of the violin at the same position in the violinplot representation as the position in the vector.
#' @param ToLog (logical) if the counts matrix should be log2(x+1) transformed.
#' @param yposminbar (numeric) a numeric value that specify where should be the horizontal bar (associated to symbols of significance) the lower in the graph compared to the higher point 
#' (maximum value) for a given feature.
#' @param yposmaxbar (numeric) a numeric value that specify where (how higher) should be the horizontal bar (associated to symbols of significance) the higher in the graph compared to the higher point
#' (maximum value) for a given feature.
#' @param motif (character chain) this character chain corresponds to the condition that is compared to others in the case where the parameter show_significativity_stars is set to TRUE. 
#' @param size_text_axis (numeric) size of text associated to graduations 
#' @param size_title_axis (numeric) size of the axis title.
#' @param x_axis_title (character chain). Name of the x axis.
#' @param show_significativity_stars (logical) whether or not display symbols of significativity
#' @param dot_size (numeric) The diameter of the dots relative to binwidth
#' @param dot_color (character chain) color of points
#' @param bin_width (numeric) In a dot plot, the width of a dot corresponds to the bin width (or maximum width, depending on the binning algorithm)
#' and dots are stacked, with each dot representing one observation.
#' @param size_stat_summary (numeric) size of the mean symbol for all the violin in the representation.
#' @param color_stat_summary (character) the color of the mean symbol.
#' @param box_width (numeric) The width of the box of the violins
#' @param thickness_median_line (numeric) The thickness of the median line.
#'
#' @import rlang purrr ggplot2 ggsignif stats magrittr
#'
#' @importFrom rlang is_empty
#'
#' @importFrom purrr map pmap 
#'
#' @importFrom ggplot2 ggplot aes_string theme_classic geom_violin geom_boxplot ggtitle theme geom_dotplot stat_summary xlab ylab scale_fill_manual
#'
#' @importFrom ggsignif geom_signif
#'
#' @importFrom stats symnum
#'
#' @importFrom magrittr %>%
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
#' str(diff_results)
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
#' has been retrieved from the github of the package DecoupleR.",
#' conv_df = data.frame())
#'
#' vlnplot_dataset(data = FOXA2_RNAseq,
#' genetorepresent = c("FOXA2","PLEKHN1","RHBDL2","FHL1","CLU","RBP4"),
#' ordercondition = c("WT","FOXA2ko"),
#' color_to_fill = c("WT" = "blue","FOXA2ko" = "red"),
#' show_significativity_stars = TRUE,
#' ToLog = TRUE,
#' motif = "FOXA2ko"
#' )
#'
#' @export



vlnplot_dataset <- function(data,
genetorepresent,
ordercondition,
color_to_fill,
ToLog,
yposminbar=2,
yposmaxbar=8,
motif = NULL,
size_text_axis=19,
size_title_axis=19,
x_axis_title = 19,
show_significativity_stars = FALSE,
dot_size = 0.7,
dot_color = NA,
bin_width = 0.037,
size_stat_summary = 2,
color_stat_summary = NA,
box_width = 0.4,
thickness_median_line = 3){
#accolade ouvrante de la fonction






## test conditionnel et instructions en fct du resultat du test pour empecher l execution de la fonction si toutes les conditions specifiees ne figurent pas dans l attribut metadata de l'instance de # #class Datasettorep.
  if(all(ordercondition %in% as.character(data@metadata[[data@colcond]]))!=TRUE){# accolade ouvrante instructions si toutes les conditions specifiees ne sont pas dans l attribut metadata de l instance #de class Datasettorep.
    
    stop(paste0("Toutes les conditions indiquees ne correspondent pas a des conditions specifies dans la colonne ",data@colcond," du dataframe donne comme valeur de l argument metadata "))
    
  }# accolade fermante instructions si toutes les conditions specifiees ne sont pas dans l attribut metadata de l instance de class Datasettorep. 


## test conditionnel et instructions en fct du resultat du test pour empecher l execution de la fonction si la meme condition est specifiee plusieurs fois dans ordercondition.
  if(length(unique(ordercondition))!=length(ordercondition)){
    
    stop(paste0("Chaque niveau de facteur ne doit etre present qu une seule fois dans le vecteur ordercondition"))
    
  }


  #Instructions a executer si toutes les conditions n'ont pas ete selectionnees.
  if(length(unique(as.character(ordercondition))) < length(unique(as.character(data@metadata[[data@colcond]])))){#accolade ouvrante 
  #instructions a executer si toutes les conditions n ont pas ete selectionnees.
  
    
  #Message a afficher si toutes les conditions n ont pas ete selectionnees.  
    print("toutes les conditions n ont pas ete selectionnees")

   

  #suppression des conditions qui ne figurent pas dans ordercondition dans le dataframe de Metadata et dans la matrice d expression expr_matrix.
  #Generation d un nouveau dataframe de metadata qui va etre celui utilise par la suite. Celui ci ne contient que les conditions specifiees dans le vecteur correspondant 
  #a l argument ordercondition.

  newMetadata <- data@metadata[-which(data@metadata[[data@colcond]] %in% ordercondition == FALSE),,drop=FALSE] 
  print("toutes les conditions n ont pas ete selectionnees")
    
  print("Voici les positions des individus qui ont ete selectionnes dans le dataframe de metadata.")
  print(which(data@metadata[[data@colcond]] %in% data@colcond == TRUE))
    

  #Generation d une nouvelle matrice qui va etre celle utilise par la suite dans la fonction. Celle ci contient donc uniquement les valeurs de l ensemble des variables pour
  #les echantillons specifiees par le vecteur ordecondition.
  newExpr_Matrix <- t(data@counts)
  newExpr_Matrix <- newExpr_Matrix[-which(data@metadata[[data@colcond]] %in% ordercondition == FALSE),]


  #Accolade fermante. Instructions a executer si toutes les conditions n ont pas ete selectionnees.  
  }else{#accolade ouvrante instructions si toutes les conditions ont ete selectionnees   
    
  print("toutes les conditions ont ete selectionnees")  
  newMetadata <- data@metadata  
  newExpr_Matrix <- t(data@counts)

  #accolade fermante instructions si toutes les conditions ont ete selectionnees.
  }

#on supprime les niveaux de facteurs qui ne sont plus associes a aucun echantillon.
  if("factor" %in% apply(data@metadata,2,class) == TRUE){
    newMetadata <- droplevels(newMetadata)
  }
  
  print("Affichage de newMetadata")
  print(newMetadata)
  

  print(newMetadata)

  print("Affichage de la colonne de newMetadata qui specifie la condition associee a chaque echantillon.")
  print(newMetadata[[data@colcond]])

  #on genere un vecteur qui correspond a ordercondition
  labels_condition <- ordercondition
  #les noms de chaque element de ordercondition correspondent egalement aux elements de ordercondition.
  names(labels_condition) <- ordercondition
  
  #on teste si le dataframe de conversion est vide ou non.
  print("est ce que le tableau de conversion est NULL")
  print(rlang::is_empty(data@conv_df))



  # s il faut convertir les noms des genes dans les colonnes.
  if(rlang::is_empty(data@conv_df)==FALSE){#instruction s il faut convertir les noms des genes dans les colonnes
    
    print("conversion dans la nomenclature approprie")

    col_appropriate_nom <- conv_ID(colnames(newExpr_Matrix),convTab = data@conv_df)[,2]# vecteur qui donne le gene_symbol de chaque gene_id du vecteur colonne du dataframe d'expression
    print("Here are the first elements of the vector col_appropriate_nom")
    print(head(col_appropriate_nom))

  }else{#instruction s il n'y a pas de conversion a faire
    
    print("pas de conversion des noms des genes realises")
    col_appropriate_nom <- colnames(newExpr_Matrix)
    
    print("voici les noms des premieres colonnes de newExpr_Matrix")
    print(head(col_appropriate_nom))

    print("Voici ce que l on obtient pour le gene SOD2 genes duplicats")

    print(grep("SOD2",col_appropriate_nom))

   
   }






    purrr::map(.x = as.list(genetorepresent),.f =function(x){# debut de la fonction inclue dans map
    
    print(x)
    print(paste0("est ce que ",x,"est dans col_gene_symbol ?"))
    testl <- (x %in% col_appropriate_nom)
    print(testl)
    
    
    
    if(x %in% col_appropriate_nom == TRUE){# s il  a bien des valeurs pour ce gene dans la matrice d expression
      
      ######################## determination de la position des barres horizontales pour la significativite      
      #occurence : nombre de colonnes du dataframe associe au gene name donne en input
      occurence <- length(which(col_appropriate_nom == x))
      #poscol : position des colonnes de la matrice d expression qui correspondent a des valeurs associees aux gene name donnes en input de la fonction.
      poscol <- which(col_appropriate_nom == x)
      #namecol : nom des colonnes de la matrice d expression qui correspondent a des valeurs associees aux gene name donnes en input de la fonction.
      nomcol <- colnames(newExpr_Matrix)[which(col_appropriate_nom == x)]
      
      MaxValue <- numeric(occurence)
      ListPosition <- vector("list",occurence)
      
      print("le nombre de genes en query : ")
      print(occurence)
      
      
      for(i in 1:occurence){
        
        print("voici la valeur de la variable ToLog")
        print(ToLog)
        
        print("Here are the first lines of the attribute metadata")
        print(head(newMetadata))

        print("here is the name of the column of interest in newMetadata")
        print(data@colcond)
        
        if(ToLog==TRUE){
          ListPosition[[i]] <- pmap(.l = list(
            x1 = as.list(combn(unique(newMetadata[[data@colcond]]),2)[1,]),# premier element des paires de                         #conditions a comparer.
            x2 = as.list(combn(unique(newMetadata[[data@colcond]]),2)[2,])),# second element des paires de                         #conditions a comparer.
            .f = function(x1,x2){
              
              c(max(log2(as.matrix(newExpr_Matrix+1))[which(newMetadata[[data@colcond]] %in% c(x1,x2)),which(colnames(log2(as.matrix(newExpr_Matrix+1)))==nomcol[i])]),
                min(max(log2(as.matrix(newExpr_Matrix+1))[which(newMetadata[[data@colcond]] %in% x1),which(colnames(log2(as.matrix(newExpr_Matrix+1)))==nomcol[i])]),
                    max(log2(as.matrix(newExpr_Matrix+1))[which(newMetadata[[data@colcond]] %in% x2),which(colnames(log2(as.matrix(newExpr_Matrix+1)))==nomcol[i])]))
              )
              
              
              
            }) %>% lapply(FUN=function(z){return((z[1]+0.3*z[2])*5*(2:length(combn(unique(newMetadata[[data@colcond]]),2)[1,])))}) %>% unlist()
          
          
          MaxValue[i] <- max(log2(as.matrix(newExpr_Matrix+1))[,which(colnames(log2(as.matrix(newExpr_Matrix+1)))==nomcol[i])],na.rm = TRUE)         
          
        } else
        {
          
          ListPosition[[i]] <- purrr::pmap(.l = list(
            x1 = as.list(combn(unique(newMetadata[[data@colcond]]),2)[1,]),# premier element des paires de                         #conditions a comparer.
            x2 = as.list(combn(unique(newMetadata[[data@colcond]]),2)[2,])),# second element des paires de                         #conditions a comparer.
            .f = function(x1,x2){
              
              
              c(max(as.matrix(newExpr_Matrix)[which(newMetadata[[data@colcond]] %in% c(x1,x2)),which(colnames(as.matrix(newExpr_Matrix))==nomcol[i])]),
                min(max(as.matrix(newExpr_Matrix)[which(newMetadata[[data@colcond]] %in% x1),which(colnames(as.matrix(newExpr_Matrix))==nomcol[i])]),
                    max(as.matrix(newExpr_Matrix)[which(newMetadata[[data@colcond]] %in% x2),which(colnames(as.matrix(newExpr_Matrix))==nomcol[i])]))
              )
              
              
              
            }) %>% lapply(FUN=function(z){return((z[1]+1.5*z[2]))*40}) %>% unlist()
          
          
          MaxValue[i] <- max(as.matrix(newExpr_Matrix)[,which(colnames(as.matrix(newExpr_Matrix))==nomcol[i])],na.rm = TRUE)
          
        }
        
      }

    ################################## premiere version de ListPosition #########################

    print("affichage de la premiere structure de ListPosition")
    
    print(str(ListPosition))


    
      

      if(rlang::is_empty(data@stattest)==FALSE){##opening bracket : instructions if data@stattest is not empty. 

      ################################ vecteur d'annotation etoile de significativite ###################
      
      #Annotation significant star
      #creation d une liste vide de longueur egale a occurence.
      Annotation <- vector("list",occurence)
       
      print("Voici la structure de StatisticList") 
      print(str(data@stattest))
      
      print("voici la longueur de StatisticList")

      for(i in 1:occurence){##debut de la boucle pour determiner les annotations de significativite pour tout les gene id associes a chacun des gene symbol donne en entree
        
        Annotation[[i]] <- lapply(data@stattest,FUN=function(z){
          
          
          return(z[which(rownames(z) == nomcol[i]),][[data@colsignificativity]])
          
          
          
          
        }) %>% lapply(FUN=function(z){
          
          
          if(is.na(z)==TRUE){return("NS")}else{
            
            return(paste0(z," : ",stats::symnum(z,symbols = data@significativity_symbols[[1]],cutpoints = data@significativity_symbols[[2]],corr = FALSE)[[1]]))
            
          }
          
        }) %>% unlist()
        
      }##fin de la boucle pour determiner les annotations de significativite pour tout les gene id associes a chacun des gene symbol donne en entree
      ################################## vecteur de paires de comparaison #######################
      




       print("Voici la premiere structure d Annotation")
       print(str(Annotation))
       print("voici les noms des elements d Annotation")
       print(names(Annotation))
       print("Voici la valeur de l argument motif")
       print(motif)

      


      
      for(i in 1:occurence){##debut de la boucle destinee a modifier ListPosition et Annotation
        
        compa <- paste0(combn(unique(newMetadata[[data@colcond]]),2)[2,],"_vs_",combn(unique(newMetadata[[data@colcond]]),2)[1,])
        
        

        print("voici la premiere version de compa")
        print(compa)
        
        ListPosition[[i]] <- ListPosition[[i]][c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa))]
        print(length(ListPosition[[i]]))
        
        
        print("voici la chaine de caracteres permettant la selection")
        print(c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa)))
        
        print(paste0("voici Annotation",i))
        print(Annotation[[i]])
        
        print(paste0("Voici les noms de Annotation",i))
        print(names(Annotation[[i]]))

        print("voici l element selectionne dans Annotation")
        print(Annotation[[i]][which(names(Annotation[[i]]) %in% compa[c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa))])])
        
        print("compa selected ")
        
        
        
        print(str(compa[c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa))]))
        
        
        print("positions noms correspondants")
        print(which(names(Annotation[[i]]) %in% compa[c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa))]))

        if(length(Annotation[[i]]) == 1){Annotation[[i]] <- Annotation[[i]][grep(motif,names(Annotation[[i]]))]}
        else{

        Annotation[[i]] <- Annotation[[i]][which(names(Annotation[[i]]) %in% compa[c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa))])]
        print(length(Annotation[[i]]))
        }
        
        compa <- compa[c(grep(paste0("^",motif,"_vs_"),compa),grep(paste0("_vs_",motif,"$"),compa))] %>% strsplit("_vs_")
        print("voici la longueur de compa")
        print(length(compa))     

        print("voici la structure de compa")
        print(str(compa))
        
        print("voici compa")
        print(compa) 




      }##fin de la boucle destinee a modifier ListPosition et Annotation


      
      # Creation d un data frame contenant deux colonnes, la premiere donnant la condition associee a chaque echantillon, la deuxieme, les valeurs (transformees log fold 2)
      
      #print("voici compa")
      #compa
      #print(compa)

      #print("voici la structure de compa")
      #print(str(compa))
      
      
      #print("le nombre de genes en query : ")
      #print(occurence)
      
      #print("voici Annotation")
      #print(Annotation)

      #print("voici la structure de Annotation")

      #print(str(Annotation))
     
      #print("voici la structure de ListPosition desormais")

      #print(str(ListPosition))
    
#closing bracket : instructions if data@stattest is not empty. 
   }  

   restr_expr <- vector("list",occurence) 
      
      for(i in 1:occurence){
        
        if(ToLog==TRUE){#s il faut faire une transformation logarithme 2 des donnees   
          restr_expr[[i]] <- data.frame(condition = newMetadata[[data@colcond]],
                                        Gene = log2(newExpr_Matrix[,which(colnames(log2(as.matrix(newExpr_Matrix+1)))==nomcol[i])]+1))
          
          #on modifie l ordre des niveaux des facteurs de la colonne condition
          restr_expr[[i]]$condition <- factor(restr_expr[[i]]$condition,levels = ordercondition) 
          
          #on renomme la premiere colonne du dataframe qui est utilise pour faire la representation. 
          colnames(restr_expr[[i]])[1] <- data@colcond
          
          
          
        }
        else{# si ce n est pas la peine.
          
          restr_expr[[i]] <- data.frame(condition = newMetadata[[data@colcond]],
                                        Gene = newExpr_Matrix[,which(colnames(as.matrix(newExpr_Matrix))==nomcol[i])])
          
          #on modifie l ordre des niveaux des facteurs de la colonne condition
          restr_expr[[i]]$condition <- factor(restr_expr[[i]]$condition,levels = ordercondition)
          
          
          colnames(restr_expr[[i]])[1] <- data@colcond
          
          
        } 
      }
      
      
      
      
      
      # Representation ggplot
      #print(restr_expr)
      #print("voici la structure de compa")
      #print(str(compa))
      #print("yposminbar")
      #print(yposminbar)
      #print("yposmaxbar")
      #print(yposmaxbar)
      #print(MaxValue[[1]])
      
      print("le nombre de genes en query : ")
      print(occurence)
      
      if(occurence == 1){
        
        if(show_significativity_stars==TRUE){#accolade ouvrante instructions si show_signifis_emptyicativity_stars vaut TRUE
          
           
         if(rlang::is_empty(data@stattest) == FALSE){
          
          return(ggplot2::ggplot(restr_expr[[1]],aes_string(x = data@colcond, y = "Gene",fill = data@colcond))+
                  ggplot2::theme_classic()+
                  ggplot2::geom_violin(trim = FALSE)+
                  ggplot2::geom_boxplot(width = box_width,fatten = thickness_median_line)+
                  ggplot2::ggtitle(label = paste0(x))+
                  ggplot2::theme(axis.title = ggplot2::element_text(size = size_title_axis))+
                  ggplot2::theme(axis.text = ggplot2::element_text(size = size_text_axis))+
                  ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=dot_size,binwidth = bin_width,fill = dot_color)+
                  ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size= size_stat_summary,color = color_stat_summary)+
                  ggplot2::xlab(x_axis_title)+
                  ggplot2::ylab(paste0(x," expression"))+
                  ggsignif::geom_signif(comparisons = compa,
                                        y_position = seq(MaxValue[[1]]+yposminbar,MaxValue[[1]]+yposmaxbar,
                                                         length.out = length(compa)),
                                        annotations=Annotation[[1]],
                                        tip_length = 0, vjust=0.4)+
                  ggplot2::scale_fill_manual(values = color_to_fill,labels =labels_condition)
                
                
                
                
                
          )
          

         #ci dessous accolade fermante si l attribut stattest n est pas une list vide.
         }
         else{#accolade ouvrante si l attribut stattest est une list vide


              print("significativity stars cannot be printed as there is an empty list as value of the attribute stattest")

                    return(ggplot2::ggplot(restr_expr[[1]],aes_string(x = data@colcond, y = "Gene",fill = data@colcond))+
                  ggplot2::geom_violin(trim = FALSE)+
                  ggplot2::geom_boxplot(width = box_width,fatten = thickness_median_line)+
                  ggplot2::ggtitle(label = paste0(x))+
                  ggplot2::theme(axis.title = ggplot2::element_text(size = size_title_axis))+
                  ggplot2::theme(axis.text = ggplot2::element_text(size = size_text_axis))+
                  ggplot2::scale_fill_manual(values = color_to_fill,labels = labels_condition)+
                  ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size = size_stat_summary,color = color_stat_summary)+
                  ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=dot_size,binwidth = bin_width,fill = dot_color)+
                  ggplot2::xlab(x_axis_title)+
                  ggplot2::ylab(paste0(x," expression"))+
                  ggplot2::theme_classic()
                  )

         #accolade fermante si l attribut stattest est une list vide
         }

          #ci dessous accolade fermante si show_significativity_stars vaut TRUE
        }
        else{#accolade ouvrante instructions si show_significativity_stars vaut FALSE.
          
          
          return(ggplot2::ggplot(restr_expr[[1]],aes_string(x = data@colcond, y = "Gene",fill = data@colcond))+
                  ggplot2::geom_violin(trim = FALSE)+
                  ggplot2::geom_boxplot(width = box_width,fatten = thickness_median_line)+
                  ggplot2::ggtitle(label = paste0(x))+
                  ggplot2::theme(axis.title = ggplot2::element_text(size = size_title_axis))+
                  ggplot2::theme(axis.text = ggplot2::element_text(size = size_text_axis))+
                  ggplot2::scale_fill_manual(values = color_to_fill,labels = labels_condition)+
                  ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size = size_stat_summary,color = color_stat_summary)+
                  ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=dot_size,binwidth = bin_width,fill = dot_color)+
                  ggplot2::xlab(x_axis_title)+
                  ggplot2::ylab(paste0(x," expression"))+
                  ggplot2::theme_classic()
                  )
                
                
                
            
          
          
          #ci-dessous accolade fermante si show_significativity_stars vaut FALSE.   
        }
        
        
        
      }
      else{
        
        
        for(i in 1:occurence){##debut de boucle representation
          
          
          
          if(show_significativity_stars==TRUE){#accolade ouvrante instructions si show_significativity vaut TRUE
            
               if(rlang::is_empty(data@stattest) == FALSE){#opening bracket if the attribute stattest is not empty

            print("il y a plusieurs occurences pour le feature")
            print("voici la structure de compa")
            print(str(compa))

            print("voici les premieres lignes des donnees qui vont etre representees")
            print(head(restr_expr[[i]]))
            print("Voici la class de ce qui va etre represente.")
            print(class(restr_expr[[i]]))
            

            print(ggplot2::ggplot(restr_expr[[i]],aes_string(x = data@colcond, y = "Gene",fill = data@colcond))+
                    ggplot2::theme_classic()+
                    ggplot2::geom_violin(trim = FALSE)+
                    ggplot2::geom_boxplot(width= box_width,fatten = thickness_median_line)+
                    ggplot2::theme(axis.title = ggplot2::element_text(size=size_title_axis))+
                    ggplot2::theme(axis.text = ggplot2::element_text(size=size_text_axis))+
                    ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize= dot_size,binwidth = bin_width,fill = dot_color)+
                    ggplot2::ggtitle(label = paste0(x,"_",nomcol[i]))+
                    ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size = size_stat_summary,color = color_stat_summary)+
                    ggplot2::xlab(x_axis_title)+
                    ggplot2::ylab(paste0(x," expression"))+
                    ggsignif::geom_signif(comparisons = compa,
                                          y_position = seq(MaxValue[[i]]+yposminbar,MaxValue[[i]]+yposmaxbar,length.out = length(compa)),
                                          annotations=Annotation[[i]],
                                          tip_length = 0, vjust=0.4)+
              ggplot2::scale_fill_manual(values = color_to_fill,labels = labels_condition))
              
#closing bracket if the attribute stattest is not empty
                  }    
             else{#opening bracket if the attribute stattest is an empty list


    print("significativity stars cannot be printed as there is an empty list as value of the attribute stattest")

    print(ggplot2::ggplot(restr_expr[[i]],aes_string(x = data@colcond, y = "Gene",fill= data@colcond))+
                    ggplot2::theme_classic()+
                    ggplot2::geom_violin(trim = FALSE)+
                    ggplot2::geom_boxplot(width = box_width,fatten = thickness_median_line)+
                    ggplot2::ggtitle(label = paste0(x,"_",nomcol[i]))+
                    ggplot2::theme(axis.title = ggplot2::element_text(size=size_title_axis))+
                    ggplot2::theme(axis.text = ggplot2::element_text(size=size_text_axis))+
                    ggplot2::scale_fill_manual(values = color_to_fill,labels = labels_condition)+
                    ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size = size_stat_summary,color = color_stat_summary)+
                    ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=dot_size,binwidth = bin_width,fill = dot_color)+
                    ggplot2::xlab(x_axis_title)+
                    ggplot2::ylab(paste0(x," expression")))




#closing bracket if the attribute stattest is an empty list
                    }

        
            
            
            
            #ci-dessous, accolade fermante instructions si show_significativity vaut TRUE
}
          else{# accolade ouvrante instructions si show_significativity vaut FALSE


            #print("il y a plusieurs occurences pour le feature")
            #print("voici la structure de compa")
            #print(str(compa))

            #print("voici les premieres lignes des donnees qui vont etre representees")
            #print(head(restr_expr[[i]]))
            #print("Voici la class de ce qui va etre represente.")
            #print(class(restr_expr[[i]]))


            
            print(ggplot2::ggplot(restr_expr[[i]],aes_string(x = data@colcond, y = "Gene",fill= data@colcond))+
                    ggplot2::theme_classic()+
                    ggplot2::geom_violin(trim = FALSE)+
                    ggplot2::geom_boxplot(width = box_width,fatten = thickness_median_line)+
                    ggplot2::ggtitle(label = paste0(x,"_",nomcol[i]))+
                    ggplot2::theme(axis.title = ggplot2::element_text(size=size_title_axis))+
                    ggplot2::theme(axis.text = ggplot2::element_text(size=size_text_axis))+
                    ggplot2::scale_fill_manual(values = color_to_fill,labels = labels_condition)+
                    ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size = size_stat_summary,color = color_stat_summary)+
                    ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=dot_size,binwidth = bin_width,fill = dot_color)+
                    ggplot2::xlab(x_axis_title)+
                    ggplot2::ylab(paste0(x," expression")))
          
        
          # closing bracket instructions if the attribute stattest is empty
          
                  
            
            #ci-dessous, accolade fermante instructions si show_significativity vaut FALSE
          }
          
          
          # 
        }##fin de boucle representation
      }
      
    }# fin instruction s il y a bien des valeurs d expression pour le gene
    
    else{# instruction s'il n y a pas de valeurs d expression pour le gene.
      
      
      return("Ce gene ne figure pas dans la matrice")
      
      
      
    }# fin instructions s il n y a pas de valeurs d expression pour le gene.
    
  }#fin de la fonction inclue dans map
  
  )# fin de la fonction map

# accolade fermante de la fonction
}
