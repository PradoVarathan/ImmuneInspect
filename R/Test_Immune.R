#' Test_Immune: Tests the gene under immune gene database InnateDB
#'
#' This funciton allows user to filter if the genes belong in the human immune system, as presented in the InnateDB database. It also has an option of output either being just a list of genes or the whole list for further analysis
#' @param screened_data The data frame, preferably from the screening function of p value, to serve as an input file along in the same format as output of selected columns in GEO2R analysis
#' @param get_whole_info Defaults to TRUE for further analysis, when FALSE, provides only the list of genes
#'
#' @return Returns a dataframe of the immune genes present in the input data
#'
#' @export
Test_Immune = function(screened_data, get_whole_info=TRUE){



  if(get_whole_info){  #gives whole data from the main data

    Immune_db_names = c(as.character(Immune_db$name))
    Test_names = c(as.character(screened_data$Gene.symbol))
    Logical_Vector = is.element(Test_names,Immune_db_names)
    immune_screen_list = data.frame(ID = screened_data$ID[Logical_Vector], adj.P.Val = screened_data$adj.P.Val[Logical_Vector],P.Value = screened_data$P.Value[Logical_Vector],t=screened_data$t[Logical_Vector],B=screened_data$B[Logical_Vector],logFC = screened_data$logFC[Logical_Vector],Gene.symbol = screened_data$Gene.symbol[Logical_Vector],Gene.title = screened_data$Gene.title[Logical_Vector])
  } else {   #just gives a list of names of genes for immediate look-up
    immune_screen_list = c()
    Immune_db_names = c(as.character(Immune_db$name))
    Test_names = c(as.character(screened_data$Gene.symbol))
    Logical_Vector = is.element(Test_names,Immune_db_names)
    immune_screen_list = as.character(screened_data$Gene.symbol[Logical_Vector])
  }
  return(immune_screen_list)
}
