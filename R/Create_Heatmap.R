#' Create_HeatMap : nteractive heatmap
#'
#' This function creates a heatmap based on the input cutoff value provided for the logFC value and provides
#' an interactive map for the same.
#'
#' @param file1 First input file in same GEO2R format
#' @param file2 Second input file for comparasion in the same format
#' @param names_exp List of the names of the experiment or comparission written as a list
#' @param cut_num Cutoff value for the logFC value
#'
#' @return Retruns a heatmap using the d3heatmap function that provides an interactive heatmap with the dendrogram
#' @export

Create_HeatMap = function(file1,file2,names_exp =c("AD_vs_Asym","AD_vs_Cont"),cut_num=0.8){

  Imm11 = file1[order(file1$Gene.symbol),]
  Imm22 = file2[order(file2$Gene.symbol),]

  i=1
  new_matrix = data.frame(Gene.symbol=character(),logFC_1=factor(),logFC_2=factor())
  while(i<nrow(Imm11)){
    j=1
    while(j<nrow(Imm22)){



      if(as.character(Imm11$Gene.symbol[i])==as.character(Imm22$Gene.symbol[j])){

        new_matrix = rbind(new_matrix,data.frame(Gene.symbol= Imm11$Gene.symbol[i],logFC_1=Imm11$logFC[i],logFC_2=Imm22$logFC[j]))

      }

      j=j+1

    }
    i=i+1
  }

  by_ddply_mat = ddply(new_matrix,.(Gene.symbol),summarize,logFC_1=mean(logFC_1),
                       logFC_2=mean(logFC_2))
  by_ddply_mat_order = by_ddply_mat[order(by_ddply_mat$logFC_1,by_ddply_mat$logFC_2),]
  by_ddply_mat_order_1 = by_ddply_mat[order(by_ddply_mat$logFC_1),]

  by_ddply_mat$logFC_1 = ifelse(abs(by_ddply_mat$logFC_1)>cut_num,by_ddply_mat$logFC_1, NA)
  by_ddply_mat$logFC_2 = ifelse(abs(by_ddply_mat$logFC_2)>cut_num,by_ddply_mat$logFC_2, NA)

  by_ddply_mat=na.omit(by_ddply_mat)

  new_heat_matrix = by_ddply_mat[,-1]
  rownames(new_heat_matrix) = by_ddply_mat[,1]
  colnames(new_heat_matrix)= names_exp
  d3heatmap(data.matrix(new_heat_matrix),scale="column",colors = "Blues")

}


