#' Screen_Pval: Screening Based on P-value
#'
#' This function removes the genes with p value greater that 0.05. This function also can be used to remove the rows with no gene symbol present.
#'
#'
#' @param main_data The data file as loaded from GEO2R results using the import function
#' @param remove_empty Defaults to TRUE. It removes the rows with no gene symbol present.
#'
#'
#' @return Returns a data frame with the screened gene symbols with other rows intact.
#'
#' @export

Screen_pval = function(main_data, remove_empty = TRUE){

 Screned_Data.df <- data.frame(ID=character(),adj.P.Val = numeric(),P.Value=factor(),t = factor(),B=factor(),logFC=factor(),Gene.symbol=character(),Gene.title=character(),stringsAsFactors = FALSE)
  i = 1
  if (remove_empty){
    while (i < length(main_data$ID)+1){

      if ((main_data$adj.P.Val[i]<0.05) && (main_data$Gene.symbol[i] != "")){

        Screned_Data.df = rbind(Screned_Data.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i]))

      }

      i =1+i
    }


  }

  else{
    while (i < length(main_data$ID)+1){

      if ((main_data$adj.P.Val[i]<0.05)){

        Screned_Data.df = rbind(Screned_Data.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i]))

      }

      i =1+i
    }
  }
  return(Screned_Data.df)
}
