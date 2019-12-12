#' Retrive_from_db: Retriving complete information from InnateDB
#'
#' This function retrives all information about the gene from the InnateDB.
#'
#' @param immune_screen_data Should consist a column of Gene.symbol or name, as seen in GEO2R output file, and is best when Test_Immune function is used previously to reduce the trial number
#' @param Database  Defaults to Innate, but the RLR database is under development and thus can be used along with Innate
#'
#' @return Retruns a data frame consisting all the important data present in Innate DB
#'
#' @export
Retrive_from_db = function(immune_screen_data, Database = "Innate"){

  #All databases to pre-load

  if(Database == "Innate"){


    Logical_element = is.element(Immune_db$name,immune_screen_data$Gene.symbol)

    Innate_Info_Scr = data.frame(id=Immune_db$id[Logical_element], species=Immune_db$species[Logical_element],
                                 taxonID=Immune_db$taxonId[Logical_element], ensembl=Immune_db$ensembl[Logical_element],
                                 entrez=Immune_db$entrez[Logical_element],name= Immune_db$name[Logical_element],
                                 fullname=Immune_db$fullname[Logical_element],synonym = Immune_db$synonym[Logical_element],
                                 signature=Immune_db$signature[Logical_element],chromStart=Immune_db$chromStart[Logical_element],
                                 chromEnd=Immune_db$chromEnd[Logical_element],chromStrand = Immune_db$chromStrand[Logical_element],
                                 chromName=Immune_db$chromName[Logical_element],band= Immune_db$band[Logical_element],
                                 goTerms=Immune_db$goTerms[Logical_element],function.=Immune_db$function.[Logical_element],
                                 goFunctions=Immune_db$goFunctions[Logical_element],goLocalizations=Immune_db$goLocalizations[Logical_element],
                                 cerebralLocalization=Immune_db$cerebralLocalization[Logical_element],nrIntxValidated=Immune_db$nrIntxsValidated[Logical_element],
                                 nrIntxsPredicted=Immune_db$nrIntxsPredicted[Logical_element], transcripts=Immune_db$transcripts[Logical_element],
                                 humanOrthologs=Immune_db$humanOrthologs[Logical_element],mouseOrthologs = Immune_db$mouseOrthologs[Logical_element],
                                 bovineOrthologs=Immune_db$bovineOrthologs[Logical_element],lastModified=Immune_db$lastModified[Logical_element])



  }
  else if (Database == "RLR"){

    Logical_element = is.element(RLR_db$`Uniprot gene names`,immune_screen_data$Gene.symbol)
    #problem in spliting the uniport gene names..

  }

  return(Innate_Info_Scr)

}
