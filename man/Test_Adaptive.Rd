% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Test_Adaptive.R
\name{Test_Adaptive}
\alias{Test_Adaptive}
\title{Test_Adaptive: Adaptive Immune System prediction function}
\usage{
Test_Adaptive(main_data, get_whole_info = TRUE)
}
\arguments{
\item{main_data}{GEO2R data output format in the format of data frame}

\item{get_whole_info}{Defaults to TRUE.Provides a option if to provide the whole information as provided by the database or just the list of genes.}
}
\value{
Returns a data frame of the gene with information filtered from the database
}
\description{
Tests if the genes present in the input file is present in geneset belonging to Th1,Th2,Th17,Tfh,CD4native,Th1-17,NK,Monocytes,Native B,Active CD4 and Active CD8 cells. This allows us to see if they are related to the adaptive immune system and which particular cell.
The database used for this was ...
}
