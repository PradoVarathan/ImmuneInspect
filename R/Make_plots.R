#' Make_plots: Makes histograms and wordclouds for basic analysis
#'
#' @param Retrived_Innatedb Input information after getting info from the InnateDB suing the Retrive_from_db function
#' @seealso Retrive_from_db
#' @export
Make_plots = function(Retrived_Innatedb){
  t=TRUE
  while(t){

    #Localization plot

    k=ggplot(ret1, aes(x=cerebralLocalization)) +geom_histogram(stat="count") + xlab("Location") + ylab("Number of genes")
    print(k)
    #Wordcloudd plot
    wordcloud(Retrived_Innatedb$function.,colors = c("chartreuse", "cornflowerblue", "darkorange"),max.words = 150)
    wordcloud(Retrived_Innatedb$goFunctions.,colors = c("chartreuse", "cornflowerblue", "darkorange"),max.words = 150)



    t = FALSE
  }

}
