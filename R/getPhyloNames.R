#' getPhyloNames
#'
#' Find matching scientific or common names for the common or scientific names provided and cache the results for efficient retrieval. This is a convenience wrapper for the \code{\link[taxize]{sci2comm}} and \code{\link[taxize]{comm2sci}} functions.
#'
#' Depending on what nameType you supply, getPhyloNames will use the sci2comm or comm2sci function to look for the matching taxonomic name. It first searches the NCBI database, and if that doesn't return a result, it will try the Encyclopedia of Life (EOL) database. This function relies on an internal helper function \code{\link{getPhyloNames_noCache}}, though you will generally not need to use it. The advantage of getPhyloNames is that it caches results, since the database APIs can be a little slow, so you will not need to keep looking up the same names over and over again.
#'
#' @param speciesNames a name or vector of common or scientific names of organisms of interest (in quotes)
#' @param nameType what type of name are you supplying? Either "sci" or "common"
#' @param clearCache delete the cached phylonamescache.rds file saved in tempdir()? default=F
#' @param quiet suppress verbose feedback from the taxize package? Default=T
#'
#' @export
#'
getPhyloNames<-function(speciesNames,nameType,clearCache=F,quiet=T){
  #allow for abbreviated nameType specification
      if(substr(nameType,1,1)=="s"){nameType <- "sci"}else{nameType <- "common"}

#check for cached species names, cuz taxize is slooooow
  tmpfile_names<-fs::path(tempdir(),"phylonamescache",ext="rds")

  #Delete cache file if requested
  if(clearCache){unlink(tmpfile_names,recursive=T);message("\n@cache cleared\n")}

    #If there's no cache, look things up
    if(!file.exists(tmpfile_names)){
      taxa_final<-getPhyloNames_noCache(speciesNames,nameType,quiet=quiet)
      test1=T #We'll consider saving this to cache

    #if there is a cache, see if it needs to be updated with new rows
    }else{
      taxa_cached<-readRDS(tmpfile_names)
      message("\nChecking cached species records\n")
      species_missing<-speciesNames[which(is.na(match(speciesNames,taxa_cached[,switch(nameType,
                                                                                sci="scientific_name",
                                                                                common="common_name")])))]
      #subset cached by the requested species records
      if(length(species_missing)==0){
        taxa<-taxa_cached
        test1=F
      }else{
        taxa_new<-getPhyloNames_noCache(species_missing,nameType,quiet=quiet)
        taxa<-rbind(taxa_cached,taxa_new)
        #I'll wait till later to write RDS, b/c I want to see if these are valid entries
        test1=T
      }
      goodRows<-match(speciesNames,taxa[,switch(nameType,sci="scientific_name",common="common_name")])
      taxa_final<-taxa[goodRows,]
    }

    #Everything below is regardless of whether cache existed or not

      #Do some error checking
      #if all of common or scientific names contain "found" as in were not found, suggest changing nameType,
      #or if some of the records have the same scientific and common name

      #Output results to user
      message("\n",rep("-",35),"\n Taxonomic Name Results\n",rep("-",35))
      print(taxa_final)
      message(rep("-",35))

      #Error checking
      if(nrow(taxa_final)==sum(grepl("found",taxa_final[,1]))|
         nrow(taxa_final)==sum(grepl("found",taxa_final[,2]))){stop("\nSomething's weird here. Did you set the right nameType?\n")}

      #warn if sci and common names match
      if(sum(taxa_final[,1]==taxa_final[,2],na.rm=T)>0){warning("Double check output. You've got some matching scientific and common names. Did you supply the correct nameType?")
        test2=F
        }else{test2=T}

    #Warn about individual no matches
    if(sum(grepl("found",taxa_final))>0){
      noMatch.indx<-which(apply(taxa_final,c(1,2),function(x) grepl("found",x)),arr.ind=T)
      noMatch<-taxa_final[noMatch.indx[,"row"],ifelse(noMatch.indx[,"col"]==1,2,1)[1]]
      warning("No match for: \n   -",paste0( noMatch,collapse="\n   -" ),"\n")
      test3=F
    }else{test3=T}

      #save to cache if all 3 tests pass
      if(test1&test2&test3){
        saveRDS(taxa_final,tmpfile_names)
        message("\n@cache updated")
        }else{
          if(test1==F){message("\n@Records already in cache")
            }else{message("\n@not saved to cache (because of potential errors)")}
        }

      invisible(taxa_final)

}
