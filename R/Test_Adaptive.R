#' Test_Adaptive: Adaptive Immune System prediction function
#'
#' Tests if the genes present in the input file is present in geneset belonging to Th1,Th2,Th17,Tfh,CD4native,Th1-17,NK,Monocytes,Native B,Active CD4 and Active CD8 cells. This allows us to see if they are related to the adaptive immune system and which particular cell.
#' The database used for this was ...
#'
#' @param main_data GEO2R data output format in the format of data frame
#' @param get_whole_info Defaults to TRUE.Provides a option if to provide the whole information as provided by the database or just the list of genes.
#'
#' @return Returns a data frame of the gene with information filtered from the database and provides a pie chart on the same.
#' @export
Test_Adaptive = function(main_data, get_whole_info=TRUE){

  Adaptive_Screened.df <- data.frame(ID=character(),adj.P.Val = factor(),P.Value=factor(),t = factor(),B=factor(),logFC=factor(),Gene.symbol=character(),Gene.title=character(),Category=character(),stringsAsFactors = FALSE)
  i = 1
  if (get_whole_info){
    while (i < length(main_data$ID)+1){
      if (is.element(main_data$Gene.symbol[i],Th1.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="Th1"))
      }
      else if (is.element(main_data$Gene.symbol[i],Th2.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="Th2"))
      }
      else if (is.element(main_data$Gene.symbol[i],Th17.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="Th17"))
      }
      else if (is.element(main_data$Gene.symbol[i],Tfh.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="Tfh"))
      }
      else if (is.element(main_data$Gene.symbol[i],CD4naive.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="naiveCD4"))
      }
      else if (is.element(main_data$Gene.symbol[i],Th1_17.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="Th1/17"))
      }
      else if (is.element(main_data$Gene.symbol[i],NK.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="NK"))
      }
      else if (is.element(main_data$Gene.symbol[i],Monocyte.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="Monocyte"))
      }
      else if (is.element(main_data$Gene.symbol[i],NaiveB.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="NaiveB"))
      }
      else if (is.element(main_data$Gene.symbol[i],ActiveCD4.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="ActivatedCD4"))
      }
      else if (is.element(main_data$Gene.symbol[i],ActiveCD8.df$Gene.name)){
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category="ActivatedCD8"))
      }

      else{
        Adaptive_Screened.df = rbind(Adaptive_Screened.df,data.frame(ID=main_data$ID[i],adj.P.Val=main_data$adj.P.Val[i],P.Value=main_data$P.Value[i],t=main_data$t[i],B=main_data$B[i],logFC=main_data$logFC[i],Gene.symbol=main_data$Gene.symbol[i],Gene.title=main_data$Gene.title[i],Category=NA))
      }
      i =1+i
    }
  }
  Adaptive_Screened.df=na.omit(Adaptive_Screened.df)
  pie = count(Adaptive_Screened.df,vars="Category")
  k = ggplot(pie, aes(x=factor(1),y=freq, fill=factor(Category))) +geom_bar(stat="identity",width=1) + coord_polar(theta = "y",start = 0) + xlab("")+ylab("")+guides(fill=guide_legend(title = "Cell Lines"))
  print(k)
  return(Adaptive_Screened.df)
}
