#' Function to generate pheatmaps
#'
#' @description Function to generate heatmaps (based on the package pheatmap representations) based on a \code{Datasettorep} object.
#'
#' @rdname pheatmap_dataset 
#'
#' @param data an object of class Datasettorep
#'
#' @param featurestorepresent a character vector. Each element of the vector is a feature that the user wants to show in the heatmap.
#'
#' @param OrderSample a character vector that gives the order in which the samples should be printed in the heatmap from the left to the right.
#'
#' @param ColGroupMetadata a named character vector that provides the color that should be associated to each condition (groups of interest in the dataframe of metadata).
#'
#' @param pathoutput a character chain that gives the path where the pdf containing the heatmap will be generated.
#'
#' @param Cluster_Method character chain that specify the clustering method that should be apply by the algorithm. Only the following character chains work : c("none","complete","ward.D","ward.D2","single","average","mcquitty","median","centroid"). "none" means that no clustering will be  applied. Samples will be kept in the order specified.
#'
#'
#' @param FontSizeCompHeatmap a numeric value that specify the size of letters in the heatmap.
#'
#' @param Title Character chain that gives the title of the heatmap.
#'
#' @param toLog logical (TRUE or FALSE) if TRUE, then the values of the count matrix will be log2(x+1) transformed before clustering and generation of the heatmap. Otherwise, original values of the count matrix will be used for the clustering and the generation of the heatmap.
#'
#' @param pdf_width Numeric value that specify the width of the output pdf that contains the heatmap. 
#'
#' @param pdf_height Numeric value that specify the height of the pdf that contains the heatmap.
#'
#' @param scalerow logical value (TRUE or FALSE). If TRUE, then a z-transformation is applied on the features values before clustering and generation of the pheatmap. If FALSE, z-transformation is not applied.
#'
#' @param show_sample_name logical value (TRUE or FALSE) If TRUE the name of each sample is printed in the output heatmap. If FALSE, names of samples are not printed in the output heatmap.
#'
#' @param clustering_distance_samples a character chain that specify which types of distances between samples will be computed and used before clustering. The following distances can be choose :
#' "euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"
#'
#' @param clustering_distance_features a character chain that specify which types of distances between features will be computed ands used before clustering. The following distances can be choose :
#' "euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"  
#'
#' @importFrom rlang is_empty
#'
#' @importFrom purrr pmap 
#'
#' @importFrom ggplot2 ggplot aes_string theme_classic geom_violin geom_boxplot ggtitle theme geom_dotplot stat_summary xlab ylab scale_fill_manual
#'
#' @importFrom ggsignif geom_signif
#'
#' @importFrom stats symnum
#'
#' @importFrom magrittr %>%
#'
#' @importFrom RColorBrewer brewer.pal
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
#' has been retrieved from the github of the package DecoupleR.")
#'
#' pheatmap_dataset(data = FOXA2_RNAseq, 
#' featurestorepresent = c("FOXA2","PLEKHN1","RHBDL2","FHL1","CLU","RBP4"),
#' OrderSample = c("WT","FOXA2ko"), 
#' ColGroupMetadata = c("WT" = "blue", "FOXA2ko" = "red"),
#' pathoutput = file.path(tempdir(), "test.pdf"),
#' Cluster_Method = "ward.D",
#' toLog = FALSE)
#'
#' @export
#'





pheatmap_dataset <- function(data,
featurestorepresent,
OrderSample,
ColGroupMetadata,
pathoutput,
Cluster_Method = "complete",
FontSizeCompHeatmap = 5,
Title = "",
toLog=TRUE,
pdf_width = 15,
pdf_height = 15, 
scalerow = FALSE,
show_sample_name = FALSE,
clustering_distance_samples = "euclidean",
clustering_distance_features = "euclidean"){
# accolade ouvrante de pheatmap_dataset







if(Cluster_Method %in% c("none","complete","ward.D","ward.D2","single","average","mcquitty","median","centroid") == FALSE){# opening bracket instruction if the value
#of argument Cluster_Method is not in the vector specified above.

stop("value of the argument Cluster_Method can only be either none (which means no clustering), complete, ward.D, ward.D2, single,average, mcquitty, median or centroid")
  
}


if(clustering_distance_samples %in% c("euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski") == FALSE || clustering_distance_features %in% c("euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski") == FALSE){#

stop("value of argument clustering_distance_samples and argument clustering_distances_features can only have the following values : euclidean, maximum, manhattan, canberra, binary and minkowski")


}



  if(rlang::is_empty(data@conv_df)==FALSE){#opening bracket if the attribute conv_df of the object data is not null.


     GeneralListe <- lapply(featurestorepresent,function(z){#accolade ouvrante du lapply

          if(z %in% data@conv_df[,2] == TRUE){
          # accolade ouvrante si z se trouve dans la premiere colonne de conv_df.
                
                #print(z,"se trouve dans le dataframe de conversion")

                return(data@conv_df[which(data@conv_df[[2]]==z),1])


          # accolade fermante si z se trouve dans la premiere colonne de conv_df.
          }else{

               return("NOT_FOUND")

          }



      #accolade fermante du lapply
    })

    names(GeneralListe) <- as.character(featurestorepresent)  

    
  
    if(any(unlist(lapply(GeneralListe,function(x){if(length(x)==1 && x == "NOT_FOUND"){return(TRUE)}else{return(FALSE)}})))==TRUE){#cas accolade ouvrante cas ou il y a des unknown dans la liste
    
       # on indique a l utilisateur les genes qui ne figurent pas dans le tableau de conversion parmi les genes de sa liste d input
       print(paste0("Les genes qui ne figurent pas dans le tableau de conversion sont :",names(keep(GeneralListe,function(x) {if(length(x)>1){return(FALSE)}else{if(x == "NOT_FOUND"){return(TRUE)}else{return(FALSE)}}}))))
    
       GeneralListe <- purrr::discard(GeneralListe,function(x) {if(length(x)>1){return(FALSE)}else{if(x == "NOT_FOUND"){return(TRUE)}else{return(FALSE)}}})
    
    
    #cas accolade fermante cas ou il y a des unknown dans la list  
    }


     GeneralListe <- lapply(GeneralListe,FUN = function(x){
    
         return(unique(x))
    
     })
    

    print("Voici la structure de GeneralListe une fois que l on a garde que les features uniques")

    print(str(GeneralListe))


     InputModName <-  purrr::pmap(.l = list(nom = as.list(names(GeneralListe)),NomenVersion = GeneralListe),.f = function(nom,NomenVersion){
    
        if(length(NomenVersion) == 1){
      
            return(nom)
        }else{
      
              return(paste0(nom,"_",NomenVersion))
        }
    
    })

    print("Voici la premiere structure de InputModName")
    print(str(InputModName))




    InputModName <- as.character(do.call(c,InputModName))

    print("voici comment est InputModName maintenant")
    print(InputModName)

      # Rangement des lignes dans l'ordre specifie
    MatrixForPheatmap <- as.character(data.frame(Input = InputModName,Output = as.character(do.call(c,GeneralListe)))[,2]) %>% lapply(function(x){
      
      if(x %in% base::rownames(data@counts)){
        
        return(data@counts[which(base::rownames(data@counts)==x),])
        
        
      }else{
        
        print("ligne non trouvee")
        return("not found")
        
      }
      
    }
    )# On range les lignes associes a chacun des genes dans le bon ordre (comme specifie dans la liste de genes fournie en entree)
    
    print("Voici MatrixForPheatmap a present")
    print(MatrixForPheatmap)

    names(MatrixForPheatmap) <- as.character(data.frame(Input = InputModName,Output = as.character(do.call(c,GeneralListe)))[,1])
    
    print("Voici MatrixForPheatmap a present qu on lui a donne des noms")
    print(MatrixForPheatmap)
 
      
    #suppression of the object GeneralListe which is from to now useless.
    rm(GeneralListe)

    # closing bracket if the attribute conv_df is not empty.
    }else{# opening bracket if the attribute conv_df of the object data is empty.

      MatrixForPheatmap <- lapply(featurestorepresent,function(x){
      
      if(x %in% base::rownames(data@counts)){
        
        return(data@counts[which(base::rownames(data@counts)==x),])
        
        
      }else{
        
        print("ligne non trouvee")
        return("not found")
        
      }
      
    }
    )# On range les lignes associes a chacun des genes dans le bon ordre (comme specifie dans la liste de genes fournie en entree)

    names(MatrixForPheatmap) <- featurestorepresent

    # closing bracket if the attribute conv_df of the object data is empty.  
    }


  # preparation du vecteur de couleurs.
  ColoursHeatmap <- list(ColGroupMetadata) %>% set_names("Condition")


  print("Voici ColoursHeatmap")

  print(ColoursHeatmap)


  print("Voici la structure de ColoursHeatmap")

  print(str(ColoursHeatmap))

  MatrixForPheatmap <- purrr::discard(MatrixForPheatmap,is.character)#elimine les elements de la liste qui correspondent a des elements noms trouves
  
   #we merge elements in order to make a matrix.
  MatrixForPheatmap <- do.call(rbind,MatrixForPheatmap)



  print("Verification que les noms de colonnes de MatrixForPheatmap correspondent bien exactement aux noms de lignes de metadata.") 
  all(colnames(MatrixForPheatmap)==rownames(data@metadata))


  MatrixForPheatmap <-  do.call(c,lapply(OrderSample,FUN = function(x){
    
    return(rownames(data@metadata)[which(data@metadata[,which(colnames(data@metadata)== data@colcond)]==x)])#on obtient en sortie un vecteur qui contient les noms des lignes # de metadatas associees aux conditions dans le bon ordre.
    
    
    
  })) %>% lapply(function(x){
      
      if(x %in% colnames(MatrixForPheatmap)){
        
        
        return(MatrixForPheatmap[,which(colnames(MatrixForPheatmap)==x)])# on stocke les colonnes de MatrixPheatmap dans une liste dans le bon ordre.
        
        
        
      }
      else{
        
        print("colonne non trouvee")
        return("not found")
        
      }
      
    })


  MatrixForPheatmap <- do.call(cbind,MatrixForPheatmap)
  
  colnames(MatrixForPheatmap) <- do.call(c,lapply(OrderSample,FUN = function(x){
    
  return(rownames(data@metadata)[which(data@metadata[,which(colnames(data@metadata)==data@colcond)]==x)])
    
  }))

    # creation de la table d annotation des colonnes
  ColumnAnnotation <- data.frame(Condition = rep(OrderSample,
                                                 do.call(c,lapply(OrderSample,FUN = function(x){
                                                   
                                                   return(length(rownames(data@metadata)[which(data@metadata[,which(colnames(data@metadata)==data@colcond)]==x)]))
                                                   
                                                 }))),
                                 row.names = do.call(c,lapply(OrderSample,FUN = function(x){
                                   
                                   return(rownames(data@metadata)[which(data@metadata[,which(colnames(data@metadata)== data@colcond)]==x)])
                                   
                                   
                                   
                                 }))
                                 
  )




    if(toLog==TRUE){#accolade ouvrante instructions si l argument toLog vaut TRUE
    print(head(log2(MatrixForPheatmap+1)))
    print(head(rownames(log2(MatrixForPheatmap+1))))
  }else{#accolade ouvrante instructions si l argument toLog vaut TRUE
    
    print(head(MatrixForPheatmap))
    print(head(rownames(MatrixForPheatmap)))
    
    
  }







  if(Cluster_Method == "none"){#si l on veut garder l'ordre des colonnes de maniere supervisee comme specifie dans la fonction
    
    
    #####pheatmap classique du package pheatmap ######
    
    if(toLog==TRUE){#accolade ouvrante instructions si toLog vaut TRUE

      MatrixForPheatmap <- log2(MatrixForPheatmap+1)

      if(scalerow==TRUE){#accolade ouvrante instructions si l on souhaite que les lignes soient centrees-reduites.
    
          #on effectue l'operation de centrage reduction pour toutes les lignes de la matrice.
    
           #le lapply permet de se debarrasser des problemes crees par la matrice assay vst de DESeq2 apres l operation de centrage.  
            newscalematrix <-lapply(1:ncol(MatrixForPheatmap),function(z){#accolade ouvrante de la fonction du lapply.
      
            return(MatrixForPheatmap[,z])
      
            #accolade fermante de la fonction du lapply.
            }) 
            
            newscalematrix <- do.call(cbind,newscalematrix) %>% apply(1,scale) %>% t()
    
            #on renomme la matrice qui a perdu le nom des colonnes
            colnames(newscalematrix) <- colnames(MatrixForPheatmap)
    
            #MatrixForPheatmap devient la matrice centree reduite.
            MatrixForPheatmap <- newscalematrix
            #On peut eliminer newscalematrix qui ne sert plus a rien.
            rm(newscalematrix)
    
      #accolade fermante instructions si l on souhaite que les lignes soient centrees reduites  
    }




      pdf(file = pathoutput,width = pdf_width,height=pdf_height)
      print(pheatmap::pheatmap(MatrixForPheatmap,
                               cluster_rows = FALSE,
                               cluster_cols = FALSE,
                               annotation_col = ColumnAnnotation,
                               show_colnames = show_sample_name,
                               color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 25, name = "RdYlBu")))(100),
                               cellheight = 9,
                               annotation_colors =ColoursHeatmap,
                               main=Title
                               ))
      
      dev.off()
      
    }#accolade fermante instructions si l argument toLog vaut TRUE
    else{#accolade ouvrante instructions si l argument toLog vaut FALSE
      
      
      
      ########### Heatmap du package complexHeatmap #####################
      
      # Annotation nom des colonnes de la pheatmap
      annota_group_column <- paste0(1:length(OrderSample),"_",OrderSample)
      
        if(scalerow==TRUE){#accolade ouvrante instructions si l on souhaite que les lignes soient centrees reduites.
    
           #on effectue l'operation de centrage reduction pour toutes les lignes de la matrice.
    
           #le lapply permet de se debarrasser des problemes crees par la matrice assay vst de DESeq2 apres l operation de centrage.  
           newscalematrix <-lapply(1:ncol(MatrixForPheatmap),function(z){#accolade ouvrante de la fonction du lapply.
      
           return(MatrixForPheatmap[,z])
           #accolade fermante de la fonction du lapply.
        }) 
         
        newscalematrix <- do.call(cbind,newscalematrix) %>% apply(1,scale) %>% t()
    
        #on renomme la matrice qui a perdu le nom des colonnes
        colnames(newscalematrix) <- colnames(MatrixForPheatmap)
    
        #MatrixForPheatmap devient la matrice centree reduite.
        MatrixForPheatmap <- newscalematrix
        #On peut eliminer newscalematrix qui ne sert plus a rien.
        rm(newscalematrix)
        #accolade fermante instructions si l on souhaite que les lignes soient centrees reduites  
      }
      
      
      pdf(file = pathoutput,width = pdf_width,height=pdf_height)
      
      
      print(pheatmap::pheatmap(MatrixForPheatmap,
                               cluster_rows = FALSE,
                               cluster_cols = FALSE,
                               annotation_col = ColumnAnnotation,
                               show_colnames = show_sample_name,
                               color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 25, name = "RdYlBu")))(100),
                               cellheight = 9,
                               annotation_colors =ColoursHeatmap,
                               main=Title))
      
      dev.off()
      
    }#accolade fermante instructions si l argument toLog vaut FALSE
    
  }
  
  
  else{#si l on veut une classification
    
    #####pheatmap classique du package pheatmap ######
    
    
    if(toLog==TRUE){#accolade ouvrante instructions si l argument toLog vaut TRUE
      
      MatrixForPheatmap <- log2(MatrixForPheatmap+1)


        if(scalerow==TRUE){#accolade ouvrante instructions si l on souhaite que les lignes soient centrees reduites.
    
            #on effectue l'operation de centrage reduction pour toutes les lignes de la matrice.
    
            #le lapply permet de se debarrasser des problemes crees par la matrice assay vst de DESeq2 apres l operation de centrage.  
            newscalematrix <-lapply(1:ncol(MatrixForPheatmap),function(z){#accolade ouvrante de la fonction du lapply.
      
            return(MatrixForPheatmap[,z])
            #accolade fermante de la fonction du lapply.
            }) 
            
            newscalematrix <- do.call(cbind,newscalematrix) %>% apply(1,scale) %>% t()
    
            #on renomme la matrice qui a perdu le nom des colonnes
            colnames(newscalematrix) <- colnames(MatrixForPheatmap)
    
            #MatrixForPheatmap devient la matrice centree reduite.
            MatrixForPheatmap <- newscalematrix
            #On peut eliminer newscalematrix qui ne sert plus a rien.
            rm(newscalematrix)
            #accolade fermante instructions si l on souhaite que les lignes soient centrees reduites  
       }


      
      pdf(file = pathoutput,width = pdf_width,height=pdf_height)
      
      pheatmap::pheatmap(log2(MatrixForPheatmap+1),
                         cluster_rows = TRUE,
                         cluster_cols = TRUE,
                         clustering_method = Cluster_Method,
                         annotation_col = ColumnAnnotation,
                         show_colnames = show_sample_name,
                         color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 25, name = "RdYlBu")))(100),
                         cellheight = 9,
                         main=Title,
                         annotation_colors =ColoursHeatmap,
                         clustering_distance_cols = clustering_distance_samples,
                         clustering_distance_rows = clustering_distance_features)
      
      
      dev.off()
      
      }
    else{#accolade ouvrante instructions si l'argument toLog vaut FALSE
      
        #l operation de centrage reduction est realisee apres la selection des bonnes lignes et des bonnes colonnes.
        if(scalerow==TRUE){#accolade ouvrante instructions si l on souhaite que les lignes soient centrees reduites.
    
                           #on effectue l operation de centrage reduction pour toutes les lignes de la matrice.
    
                           #le lapply permet de se debarrasser des problemes crees par la matrice assay vst de DESeq2 apres l operation de centrage.  
                           newscalematrix <-lapply(1:ncol(MatrixForPheatmap),function(z){#accolade ouvrante de la fonction du lapply.
      
                           return(MatrixForPheatmap[,z])
           
           #accolade fermante de la fonction du lapply.
          }) 
          
          newscalematrix <- do.call(cbind,newscalematrix) %>% apply(1,scale) %>% t()
    
          #on renomme la matrice qui a perdu le nom des colonnes
          colnames(newscalematrix) <- colnames(MatrixForPheatmap)
    
          #MatrixForPheatmap devient la matrice centree reduite.
          MatrixForPheatmap <- newscalematrix
          #On peut eliminer newscalematrix qui ne sert plus a rien.
          rm(newscalematrix)
          #accolade fermante instructions si l on souhaite que les lignes soient centrees reduites  
          }
      
          pdf(file = pathoutput,width = pdf_width,height=pdf_height)
      
            pheatmap::pheatmap(MatrixForPheatmap,
                            cluster_rows = TRUE,
                            cluster_cols = TRUE,
                            clustering_method = Cluster_Method,
                            annotation_col = ColumnAnnotation,
                            show_colnames = show_sample_name,
                            color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 25, name = "RdYlBu")))(100),
                            cellheight = 9,
                            main=Title,
                            annotation_colors =ColoursHeatmap,
                            clustering_distance_cols = clustering_distance_samples,
                            clustering_distance_rows = clustering_distance_features)
      
      dev.off()
      
      
      
      
      
      
      
      
      
      
    }#accolade fermante instructions si l argument toLog vaut FALSE
    
  }

}
