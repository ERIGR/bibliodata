% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-Datasettorep.R
\docType{class}
\name{Datasettorep-class}
\alias{Datasettorep-class}
\title{The Datasettorep S4 R class}
\description{
An object of class Datasettorep allows to store a dataset and to generate basic representations based on the stored dataset, that is violinplot and heatmap representations.
}
\section{Slots}{

\describe{
\item{\code{counts}}{The matrix (class matrix) that stored values of each variables (rows) for each sample (column)}

\item{\code{metadata}}{a data.frame (class data.frame) that gives the condition associated to each sample (rows). This data.frame should contain at least a column (whose name is given by the slot colcondition) #' that gives the condition associated to each sample. Rownames of the metadata data.frame should correspond to colnames of the matrix counts. (samples should be ranged in the same order as in the matrix #' counts)}

\item{\code{stattest}}{a list (class list). Each element of this list is a data.frame (class data.frame) with at least one column (whose name is given the slot colsignificativity) that gives the level of  #'significativity (for instance, p-value or fdr) of the difference between two conditions for each feature. That is why each element of the list should correspond to results of statistical analysis #'comparing features between two conditions (conditions specified thanks to the data.frame metadata)}

\item{\code{conv_df}}{a data.frame that gives the corresponding name of each feature in another nomenclature when needed (for example ensembl gene ID and usual symbol gene name). The first column should #'correspond to names in the the nomenclature used in the matrix given as value of the slot counts. Each character chain in this column should be unique. The second column contains corresponding names in #'the other  nomenclature. The same name can be found several times in this column.}

\item{\code{colcond}}{a character chain (class character) that gives the name of the column of metadata that should be use to specify the condition associated to each sample.}

\item{\code{colsignificativity}}{a character chain (class character) that gives the name of the column in each data.frame that provides the level of significativity of the difference of 
a feature between two conditions.}

\item{\code{significativity_symbols}}{a list (class list) that gives the symbol associated to each level of significativity when printing violinplot.}

\item{\code{description}}{a character chain (character chain) that allows to describe for instance how the dataset has been generated. Can be useful when the dataset has been generated a long time ago to store important informations about the generation of the data (computational and statistical methods used for example as well as the version of these tools)}
}}

\concept{objects}
