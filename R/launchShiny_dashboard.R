#' Shiny app to generate graphical representations of datasets   
#'
#' @description This function launch a shiny app that allows to the user to generate either violinplots representations (based on the package ggplot2) or heatmap representations of each of the 
#' dataset stored in the bibliodata object provided by the user as value of the argument bibliodata.
#'
#' @rdname launchShiny_dashboard
#'
#' @param object An object of class bibliodata that contains a set of datasets of interest for the user.
#'
#' @param colors Character vector A set of colors (for example some of those provided as output of the function grDevices::colors). The user should take enough colors to be sure that he will be able 
#' pick a color for each conditions associated to the datasets he wants to explore with this application.
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem
#'
#' @importFrom shiny titlePanel sidebarLayout helpText selectInput textInput uiOutput conditionalPanel numericInput actionButton textOutput fluidPage uiOutput mainPanel observeEvent updateSelectInput eventReactive renderText renderUI shinyApp
#'
#'
#' @examples
#'
#'
#' require(readr)
#' require(R.utils)
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
#' description = "RNAseq of HEL cells (cell line derived from a patient 
#' suffering of an erythroleukemia). 
#' There is four conditions is this dataset : control HEL cells, HEL cells carrying the ETO2-GLIS2 
#' fusion (introduced by CRISPR-Cas9 editing), 
#' HEL cells carrying the ETO2-GLIS2 fusion 
#' but with a truncated C265G domain (zinc finger of the GLIS2 part of the protein), 
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
#'res <- withTimeout({
#'  launchShiny_dashboard(example_bibliodata,colors = c("red","blue","green",
#' "purple","orange","yellow","black","grey"))
#' }, timeout = 0.75, onTimeout = "silent")
#' 
#' @export
#'


launchShiny_dashboard <- function(object,
colors = c("white","grey","aquamarine","bisque","black","blue","blue4","brown","cyan","gold","gold4","green","khaki","magenta","navyblue","orange","pink","pink4","purple","red","sienna","yellow")
){
# opening bracket of the function of setMethod


############################## chargement des packages ######################
#require(shiny)
#require(shinydashboard)
#require(pheatmap)
#require(rlang)
#require(purrr)
#require(ggsignif)
#require(ggplot2)
#require(magrittr)
#require(readr)



####################################### code qui doit tourner avant de definir l appli Shiny en elle meme. ########################

#Generation of the vector conditions_colors based on the value of the argument colors of the method launchShiny_vlnplot.

if((length(colors) == length(unique(colors)))==FALSE){#opening bracket instruction if one or several colors appear
#several times in the vector colors.


stop("every element of the vector given as value of the argument colors should be unique")


#closing bracket instruction if one or several colors appear several times in the vector colors.
}


### conditions_colors va correspondre avant la valeur de l argument colors.
conditions_colors <- colors



#a chaque element du vecteur conditions_colors  on ajoute un _ suivi du nombre correspondant a la position de l element dans le vecteur.
conditions_colors <- paste0(conditions_colors,"_",1:length(conditions_colors))

#definition de la variable maxIDX qui correspond a la longueur du vecteur conditions_colors.
maxIDX <- length(conditions_colors)

#les noms des elements correspondent aux prefixe de chacuns des element (le prefixe correspondant toujours a une couleur)
names(conditions_colors) <- colors

print("voici conditions_colors :")
print(conditions_colors)


#if no names are given to elements of the list attribute biblio, each element of the list attribute biblio will be named dataset_1,dataset_2 etc... according to his position in the list.

#the variable biblio_list will be associated to the list attribute biblio of the value of the argument bibliodata.
biblio_list <- object@biblio

if(is.null(names(biblio_list))==TRUE){#opening bracket instruction if there is no names associated to elements of the list biblio_list

names(biblio_list) <- paste0("Dataset_",1:length(biblio_list))

#closing bracket instruction if there is no names associated to elements of the list biblio_list
}

########################### definition de l appli Shiny ###########################

##### definition de l ui

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "representation violinplot et heatmap"),
  shinydashboard::dashboardSidebar(    
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Violinplot", tabName = "Violinplot", icon = NULL),
      shinydashboard::menuItem("Pheatmap", tabName = "Pheatmap", icon = NULL)
    )),
  shinydashboard::dashboardBody(
### opening parenthesis of the dashboardBody.

shinydashboard::tabItems(
##opening parenthesis of the tabItems
#### partie qui concerne l application violinplot

shinydashboard::tabItem(
##opening parenthesis of tabItem regarding the generation of violinplots
tabName = "Violinplot",
shiny::fluidPage(#opening parenthesis of fluidPage

  shiny::tags$head(#opening parenthesis of tags$head

       shiny::tags$style(HTML("
       input[type=\"number\"]{

        height: 20px;

       }


      input[type=\"text\"]{

        height: 20px;

       }

      .selectize-input {
        height: 20px;

      }
cond_order
  
       
       ")) 


      


  #closing parenthesis of tags$head
  ),    

  # Application title
  shiny::titlePanel("Violinplot representation"),

# Sidebar with a slider input for number of bins 
shiny::sidebarLayout(#opening parenthesis of the function sidebarLayout
       shiny::sidebarPanel(#opening parenthesis of sidebarPanel
                shiny::helpText("Violinplot representation of a feature of the selected dataset."),
                shiny::selectInput("data_Vln",
                label = "dataset",
                choices = names(biblio_list),
                selected = names(biblio_list)[1]
                ),
                
                shiny::selectInput("log2","log2(x+1)?",choices = c("TRUE","FALSE"),selected = "TRUE"),

                shiny::selectInput("SigniStars","show significativity stars",choices = c("TRUE","FALSE"),selected = "TRUE"),
                
                shiny::textInput("listfeatures_Vln","path to a list of one or several features",value= "/path/to/a/listfeatures.txt"),
                
                shiny::textInput("outputpdf_Vln","path to output pdf",value = "/path/to/the/file_with_features_violinplot_representations.pdf"),
                
                
                #### probleme avec conditionalPanel il faudran comprendre comment y remedier (peut etre avec addUI removeUI mais pas sur)
                #conditionalPanel(condition = paste0("input.data=='",names(biblio_list),"'")"input.data=='Bulk RNAseq EG'",
                #selectInput("condEGRNAseqBulk_motif",label="Condition reference EG RNAseq Bulk",choices = c("C265","dNHR2","EG","ERGKO","ERGKO_EG","zCTRL"),multiple = FALSE)
                #),
                
                #uiOutput that will allow to select the reference condition according to the dataset previously selected (selectInput data)
                shiny::uiOutput("reference_condition"),

                #uiOutput that will allow to select conditions to represent in the violinplot and in which order.
                shiny::uiOutput("order_conditions_Vln"),


                shiny::selectInput("color_Vln",
                label = "couleurs violinplot (nbr col = nbr cond)",
                choices = sort(conditions_colors),multiple = TRUE),
                selectInput("DotColor",
                label = "color of points (NA means transparency)",
                choices = c("NA","red","blue","green","orange","purple","brown","black","grey","gold","pink","khaki","cyan","white","wheat3","navajowhite","violet"),
                selected = "NA",
                multiple = FALSE),
                  
                shiny::textInput("hauteurpdf_Vln","height of the pdf",value="15"),
                  
                shiny::textInput("largeurpdf_Vln","width of the pdf",value="15"),

                shiny::numericInput("BoxWidth","width of the box",value = 0.4),  
                shiny::numericInput("BinWidth","bin width",value = 0.037),
                shiny::numericInput("sizepoint","size of point",value = 0.7 ),
                shiny::conditionalPanel(condition = "input.SigniStars == 'TRUE'",numericInput("yposminb","y min signi lines",value = 1)),
                shiny::conditionalPanel(condition = "input.SigniStars == 'TRUE'",numericInput("yposmaxb","y max signi lines",value = 3)),
                shiny::numericInput("SizeTextAxis", "size of axis text",value = 19),
                shiny::numericInput("SizeTitleAxis","size of axis title",value = 19),
                shiny::numericInput("ThicknessMedianLine","thickness of median line",value = 3),

                shiny::selectInput("ColorStatSummary",
                label = "color of mean symbol (NA means transparency)",
                choices = c("red","blue","green","orange","purple","brown","black","grey","gold","pink","khaki","cyan","white","wheat3","navajowhite","violet"),
                selected = "black"),
                  
                shiny::numericInput("SizeStatSummary","Taille symbole de moyenne",value = 2),
                  
                shiny::actionButton("go_Vln","Go")
                
                
  ##closing parenthesis of sidebarPanel
),
shiny::mainPanel(
shiny::textOutput("ViolinPlot")

)

# closing parenthesis of the function sidebarLayout
)
#closing parenthesis of FluidPage
)
#closing parenthesis of TabItem
),

### partie qui concerne l application pheatmap
shinydashboard::tabItem(
##opening parenthesis of tabItem regarding the generation of heatmaps.
tabName = "Pheatmap",
shiny::fluidPage(#opening parenthesis of fluidPage

# Application title
shiny::titlePanel("Pheatmap representation"),
shiny::sidebarLayout(#opening parenthesis of the function sidebarLayout

   shiny::sidebarPanel(#opening parenthesis of sidebarPanel
       shiny::helpText("pheatmap representation of specified features for the selected dataset"),
       shiny::selectInput("data_Ph",
       label = "dataset",
       choices = names(biblio_list),
       selected = names(biblio_list)[1]
       ),
       shiny::uiOutput("order_conditions_Ph"),
       shiny::selectInput("color_Ph",
       label = "couleurs pheatmap (nbr col = nbr cond)",
       choices = sort(conditions_colors),multiple = TRUE),
       shiny::textInput("listfeatures_Ph","Path to a list of one or several features.",value = "/path/to/a/listfeatures.txt"),
       shiny::textInput("outputpdf_Ph","path to output pdf",value = "/path/to/the/file_with_features_pheatmap_representation.pdf"),
       shiny::numericInput("largeurpdf_Ph","width of the pdf",value = "15"),
       shiny::numericInput("hauteurpdf_Ph","height of the pdf", value = "15"),
       shiny::selectInput("titlepheatmaplogi","title for the pheatmap ?",choices = c("TRUE","FALSE"),selected = FALSE),
       shiny::selectInput("ShowSampleName","print names of each samples",choices = c("TRUE","FALSE"),selected = FALSE),
       shiny::conditionalPanel(condition = "input.titlepheatmaplogi == 'TRUE'",
       shiny::textInput("titlepheatmap","title of the pheatmap",value = "pheatmap")),
       shiny::selectInput("ClusteringMethod","method of clustering",choices = c("none","complete","ward.D","ward.D2","single","average","mcquitty","median","centroid"),selected = "complete"),
       shiny::conditionalPanel(condition = "input.ClusteringMethod == 'complete' || input.ClusteringMethod == 'ward.D' || input.ClusteringMethod == 'ward.D2' || input.ClusteringMethod == 'single' || input.ClusteringMethod == 'average' || input.ClusteringMethod == 'mcquitty' || input.ClusteringMethod == 'median' || input.ClusteringMethod == 'centroid'",
       shiny::selectInput("clustering_distance_samples","clustering distance for samples",choices = c("euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"),selected = "euclidean")),
       shiny::conditionalPanel(condition = "input.ClusteringMethod == 'complete' || input.ClusteringMethod == 'ward.D' || input.ClusteringMethod == 'ward.D2' || input.ClusteringMethod == 'single' || input.ClusteringMethod == 'average' || input.ClusteringMethod == 'mcquitty' || input.ClusteringMethod == 'median' || input.ClusteringMethod == 'centroid'",
       shiny::selectInput("clustering_distance_features","clustering distances for features",choices = c("euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"),selected = "euclidean")),
       shiny::selectInput("ToLog","log data (log2) ?",choices = c("TRUE","FALSE"),selected = TRUE),
       shiny::conditionalPanel(condition ="input.ToLog == 'FALSE'",
       shiny::selectInput("scalerow","z-score transformation features ?",choices = c("TRUE" , "FALSE"),selected = TRUE)),
       shiny::numericInput("pheatmap_fontsize","fontsize row and column name",value = 5,min = 0.01),
       shiny::actionButton("go_Ph","Go")
   #closing parenthesis of sidebarPanel
   ),
   shiny::mainPanel(
    
    shiny::textOutput("Pheatmap")
   
   )
# closing parenthesis of the function sidebarLayout
)
#closing parenthesis of fluidPage
)
##closing parenthesis of tabItem regarding the generation of heatmaps.
)
### closing parenthesis of the tabItems
)

### closing parenthesis of the dashboardBody.
)

#closing parenthesis of dashboardPage
)



######################### definition de la partie serveur de l application


server <- function(input,output,session){ 
#opening bracket of the server function

### partie qui concerne l application violinplot


########## fonction pour pouvoir selectionner plusieurs fois la meme couleur.

  # need <<- when written in observers
  
  idx_Vln <- maxIDX 

  previous_input_color_Vln = c()
  initial_choices_Vln = sort(conditions_colors)  # must match ui
   
  

 


  maxIDX_Vln <- maxIDX

  observeEvent(input$color_Vln, {
    
    cat("\nstep 0> trigger", "\n")
    #explication ligne ci dessous : si previous_input_color est bien different de la valeur actuelle de input$color alors les instructions des lignes qui suivent sont executes.
    req(!identical(previous_input_color_Vln, input$color_Vln))
    cat("step 1> change detected", "\n")
    
    # addition stocke les couleurs differentes entre le vecteur actuel input$color et le vecteur ancien previous_input_color.
    addition <- base::setdiff(input$color_Vln, previous_input_color_Vln)
    if (length(addition) > 0) {#accolade ouvrante si le vecteur addition est de longueur superieure a 0.
      cat("step 2> addition:", addition, "\n")
      #on incremente idx de 1 de maniere globale. L incrementation est valable en dehors de la fonction.
      idx_Vln <<- idx_Vln + 1
      #on stocke les noms des elements qui sont differents entre le vecteur imput$color et le vecteur previous_input_color.
      new_nm <- names(initial_choices_Vln[initial_choices_Vln == addition])
      #new_val : on accole au nom de l element en plus idx incremente de 1.
      new_val <- paste0(new_nm,"_", idx_Vln)
      #on rajoute a initial_choices l element new_val
      choices <- c(initial_choices_Vln, new_val)
      #on redonne des noms a choices. On rajoute le nom associe a l element new_val.
      names(choices) <- c(names(initial_choices_Vln), new_nm)
    }
    
    #on definit missing. correspond surement a la situation ou on enleve un element par rapport au vecteur de depart. 
    #On identifie l element manquant.
    
    missing <- base::setdiff(previous_input_color_Vln, input$color_Vln)
    if (length(missing) > 0) {
      # s il y a au moins un element manquant.
      cat("step 2> missing:", missing, "\n")
      #missing_idx quel est la position de la valeur de initial_choices qui correspond a missing.
      missing_idx <- which(initial_choices_Vln == missing)
      #on supprime dans initial_choices l element se trouvant a la position de missing_idx.
      choices <- initial_choices_Vln[-missing_idx]
    }
    
    # sort choices so that No is always first
    choices <- sort(choices)
    cat("step 3> updated choices:", choices, "\n")
    
    updateSelectInput(session, "color_Vln",
                      choices = choices,
                      selected = input$color_Vln
    )
    
    # save current values
    previous_input_color_Vln <<- input$color_Vln
    initial_choices_Vln <<- choices
  }, ignoreNULL = FALSE)




  
  previous_input_color_Ph = c()
  initial_choices_Ph = sort(conditions_colors)  # must match ui
   
  

  idx_Ph <- maxIDX


  maxIDX_Ph <- maxIDX

  shiny::observeEvent(input$color_Ph, {
    
    cat("\nstep 0> trigger", "\n")
    #explication ligne ci-dessous : si previous_input_color est bien different de la valeur actuelle de input$color alors les instructions des lignes qui suivent sont executes.
    req(!identical(previous_input_color_Ph, input$color_Ph))
    cat("step 1> change detected", "\n")
    
    # addition stocke les couleurs differentes entre le vecteur actuel input$color et le vecteur ancien previous_input_color.
    addition <- base::setdiff(input$color_Ph, previous_input_color_Ph)
    if (length(addition) > 0) {#accolade ouvrante si le vecteur addition est de longueur superieure a 0.
      cat("step 2> addition:", addition, "\n")
      #on incremente idx de 1 de maniere globale. L incrementation est valable en dehors de la fonction.
      idx_Ph <<- idx_Ph + 1
      #on stocke les noms des elements qui sont differents entre le vecteur imput$color et le vecteur previous_input_color.
      new_nm <- names(initial_choices_Ph[initial_choices_Ph == addition])
      #new_val : on accole au nom de l element en plus idx incremente de 1.
      new_val <- paste0(new_nm,"_", idx_Ph)
      #on rajoute a initial_choices l element new_val
      choices <- c(initial_choices_Ph, new_val)
      #on redonne des noms a choices. On rajoute le nom associe a l element new_val.
      names(choices) <- c(names(initial_choices_Ph), new_nm)
    }
    
    #on definit missing. correspond surement a la situation ou on enleve un element par rapport au vecteur de depart. 
    #On identifie l'element manquant.
    
    missing <- base::setdiff(previous_input_color_Ph, input$color_Ph)
    if (length(missing) > 0) {
      # s il y a au moins un element manquant.
      cat("step 2> missing:", missing, "\n")
      #missing_idx quel est la position de la valeur de initial_choices qui correspond a missing.
      missing_idx <- which(initial_choices_Ph == missing)
      #on supprime dans initial_choices l element se trouvant a la position de missing_idx.
      choices <- initial_choices_Ph[-missing_idx]
    }
    
    # sort choices so that No is always first
    choices <- sort(choices)
    cat("step 3> updated choices:", choices, "\n")
    
    shiny::updateSelectInput(session, "color_Ph",
                      choices = choices,
                      selected = input$color_Ph
    )
    
    # save current values
    previous_input_color_Ph <<- input$color_Ph
    initial_choices_Ph <<- choices
  }, ignoreNULL = FALSE)
  



output$reference_condition <- shiny::renderUI({#opening bracket of the renderUI that will allow to define the reference condition according 
#to the dataset specified previously

#vector that contains conditions that the user will select according to the dataset he want to study.
cond <- unique(biblio_list[[input$data_Vln]]@metadata[[biblio_list[[input$data_Vln]]@colcond]])

shiny::selectInput("refcond",label = "choose the reference condition",choices = cond)


#closing bracket of the renderUI that will allow to define the reference condition according to the dataset specified previously.
})


output$order_conditions_Vln <- shiny::renderUI({# opening bracket of the renderUI that will allow to select conditions that 
#will be represented in the violonplot and also the order of these conditions in the violinplot

condfororder_Vln <- unique(biblio_list[[input$data_Vln]]@metadata[[biblio_list[[input$data_Vln]]@colcond]])

shiny::selectInput("cond_order_Vln",label = "conditions to represent",choices = condfororder_Vln,multiple = TRUE)


# closing bracket of the renderUI that will allow to select conditions that will be represented in the violinplot
#and also the order of these conditions in the violinplot
})


#### Generation de la partie 

testreac1 <- shiny::eventReactive(input$go_Vln,
                            {#opening bracket of testreac

                             
                             #import the list of genes that will be represented.
                             feature_l_Vln <- read.table(input$listfeatures_Vln,header = FALSE)[[1]]
                              
                              # generation of violinplots
                              
                              # specifying the output path for the pdf that will contain representations (and also the height and the width of the pdf)
                              pdf(input$outputpdf_Vln,height = as.numeric(input$hauteurpdf_Vln),width=as.numeric(input$largeurpdf_Vln))
                              
                              if(input$yposminb <= 0){
                                
                                stop("the value of y min signi lines should be superior to 0")
                              }
                              if(input$yposmaxb <= input$yposminb){
                                
                                stop("the value of y min signi lines should be inferior to the value of y max signi lines")
                                
                              }
                              
                              
                              if(length(input$DotColor)>1){
                                
                                stop("only on color should be choose for dot color")
                              }
                              if(input$DotColor=="NA"){
                                DotCol <- NA
                                
                              }else{
                                
                                DotCol <- input$DotColor
                                
                              }
                              if(input$ColorStatSummary == "NA"){
                                
                                ColStatSummary <- NA
                                
                              }else{
                                
                                ColStatSummary <- input$ColorStatSummary
                                
                              }


                              #### part where we call the method vlnplot_dataset of the class Datasettorep to generate the represnetation.
                                #print("voici order_conditions avec input")
                                #print(input$order_conditions)
                                 
                                 print("Voici input color.")
                                 print(input$color_Vln)
                                  
                                 # preparation du vecteur de couleur pour remplir les violinplots
                                 vec_color_Vln <- unlist(lapply(strsplit(input$color_Vln,"_"),function(z){
                                 #accolade ouvrante de la fonction.  

                                 return(z[[1]])   

                                # accolade fermante de la fonction.
                                 }))

                                print("Voici cond_order_Vln")
                                print(input$cond_order_Vln)
                                #print("voici order_conditions avec output") 
                                #print(output$order_conditions)

                               print(vlnplot_dataset(data = biblio_list[[input$data_Vln]],
                               genetorepresent = feature_l_Vln,
                               ordercondition = input$cond_order_Vln,
                               color_to_fill = vec_color_Vln,
                               ToLog = input$log2,
                               yposminbar = input$yposminb,
                               yposmaxbar = input$yposmaxb,
                               motif = input$refcond,
                               size_text_axis = input$SizeTextAxis,
                               size_title_axis= input$SizeTitleAxis,
                               x_axis_title = "condition",
                               show_significativity_stars = input$SigniStars,
                               dot_size = input$sizepoint,
                               dot_color = input$DotColor,
                               bin_width = input$BinWidth,
                               size_stat_summary = input$SizeStatSummary,
                               color_stat_summary = input$ColorStatSummary,
                               box_width = input$BoxWidth,
                               thickness_median_line = input$ThicknessMedianLine
                               ))

                              dev.off()
                              # exemple d utilisation de la fonction a effacer apres utilisation.
                              print(paste0("Les genes de la liste figurant dans le fichier ",input$feature_l," ont ete representes en violinplot dans le fichier ", input$outputpdf))
                              #                              vlnplot_dataset(data = test_datasettorep1,
                              #genetorepresent = c("GATA1","ERG","CTNNB1","SOD2"),
#ordercondition = c("Glis1","Glis2"),
#color_to_fill = c("red","blue"),
#ToLog = FALSE,
#yposminbar = 2,
#yposmaxbar = 8,
#motif= "Glis2",
#size_text_axis=19,
#size_title_axis=19,
#x_axis_title = "condition",
#show_significativity_stars = TRUE,
#dot_size = 0.2,
#dot_color = NA,
#bin_width = 0.037,
#size_stat_summary = 2,
#color_stat_summary = NA)
#






#closing bracket of testreac
}
#closing parenthesis of the part eventReactive                            
)



output$ViolinPlot <- shiny::renderText({
    
    
    print(testreac1())
    
    
})
  






### partie qui concerne l application pheatmap


output$order_conditions_Ph <- shiny::renderUI({## opening bracket of the renderUI that will allow to select conditions
#that will be represented in the pheatmap as well as the order of these conditions in the pheatmap.

condfororder_Ph <- unique(biblio_list[[input$data_Ph]]@metadata[[biblio_list[[input$data_Ph]]@colcond]])

shiny::selectInput("cond_order_Ph",label = "conditions to represent",choices = condfororder_Ph, multiple = TRUE)


## closing bracket of the renderUI that will allow to select conditions that will be represented in the pheatmap as well as the order of these conditions in the pheatmap.
})


testreac2 <- shiny::eventReactive(input$go_Ph,
{#opening bracket of eventReactive

print("Voici les conditions qui vont etre representes.")

print(input$cond_order_Ph)

print("Voici les couleurs associes a chaque condition.")
print(input$color_Ph)

#import the list of genes that will be represented
feature_l_Ph <- read.table(input$listfeatures_Ph,header=FALSE)[[1]]

#Generation of the vector that specifies the color associated to each condition.
vec_color_conditions_Ph <- as.character(input$color_Ph)
names(vec_color_conditions_Ph) <- as.character(input$cond_order_Ph)

pheatmap_dataset(data = biblio_list[[input$data_Ph]],
featurestorepresent = feature_l_Ph,
OrderSample = input$cond_order_Ph,
ColGroupMetadata = unlist(lapply(strsplit(vec_color_conditions_Ph,"_"),function(t){return(t[[1]])})),
pathoutput = input$outputpdf_Ph,
Cluster_Method = input$ClusteringMethod,
FontSizeCompHeatmap = input$pheatmap_fontsize,
Title = input$titlepheatmap,
toLog = as.logical(input$ToLog),
pdf_width = as.numeric(input$largeurpdf_Ph),
pdf_height = as.numeric(input$hauteurpdf_Ph),
scalerow = as.logical(input$scalerow),
show_sample_name = as.logical(input$ShowSampleName),
clustering_distance_samples = input$clustering_distance_samples,
clustering_distance_features = input$clustering_distance_features)


print(paste0("Les genes de la liste figurant dans le fichier ",input$feature_l," ont ete representes en pheatmap dans le fichier ", input$outputpdf))

#closing bracket of eventReactive
}
#closing parenthesis of the function eventReactive
)

output$Pheatmap <- shiny::renderText({
    
    
 print(testreac2())
    
    
})








#closing bracket of the server function
}

shiny::shinyApp(ui, server)

#closing bracket of the function.
}
