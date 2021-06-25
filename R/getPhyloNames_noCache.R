#' getPhyloNames_noCache
#'
#' Internal helper function for getPhyloNames; looks for either a scientific or common name matching whichever you supply, based on nameType. Does the work of interfacing with the \code{\link[taxize]{sci2comm}} and \code{\link[taxize]{comm2sci}} functions
#'
#' You generally won't want or need to use this function, since \code{\link{getPhyloNames}} invokes it and will cache the results to run more efficiently. But this is basically a convenience wrapper that can suppress some of the messaging from taxize and also search both NCBI and EOL databases for taxonomic matches.
#'
#' @param speciesNames a name or vector of common or scientific names of organisms of interest (in quotes)
#' @param nameType what type of name are you supplying? Either "sci" or "common"
#' @param quiet suppress verbose feedback from the taxize package? Default=T
#'
#' @export
#'
getPhyloNames_noCache<-function(speciesNames,nameType,quiet=T){
      taxize::taxize_options(taxon_state_messages=quiet)
    #allow for abbreviated nameType specification
      if(substr(nameType,1,1)=="s"){nameType <- "sci"}else{nameType <- "common"}
      message("Looking for ",switch(nameType,sci="common",common="scientific")," names in NCBI:\n")
      outNames<-switch(nameType,
                        common={pbapply::pbsapply(speciesNames,function(x){
                          tmp<-if(quiet){suppressMessages(taxize::comm2sci(x)) }else{taxize::comm2sci(x)}
                          #if common name not found in NCBI, try in EOL
                            if(length(tmp[[1]])==0){
                              tmpList<-if(quiet){suppressMessages(taxize::comm2sci(x,simplify=FALSE,db="eol")[[1]])
                                      }else{taxize::comm2sci(x,simplify=FALSE,db="eol")[[1]]}
                                tmp<-tmpList$name[1]

                              if(is.null(tmp)){tmp<-"no common name found"
                                }else{message("\n ! ",nameType," name for ",x," not found in NCBI; using top hit in EOL: ",tmp,"\n")}
                            }

                          if(length(tmp[[1]])==0){tmp<-"no sci. name found"}
                          message("\n  -", x,"  =  ",tmp,"\n")
                          unlist(tmp)
                          })
                        },
                        sci={pbapply::pbsapply(speciesNames,function(x){
                            tmp<-if(quiet){suppressMessages(taxize::sci2comm(x))}else{
                              taxize::sci2comm(x)}#taxize is a noisy package...suppressing all feedback
                            #if common name not found in NCBI, try in EOL
                            if(length(tmp[[1]])==0){
                              tmpList<-if(quiet){suppressMessages(taxize::sci2comm(x,simplify=FALSE,db="eol")[[1]])
                                      }else{taxize::sci2comm(x,simplify=FALSE,db="eol")[[1]]}
                                tmp<-subset(tmpList,tmpList$language=="en")$vernacularname[1]

                              if(is.null(tmp)){tmp<-"no common name found"}
                            }
                            message("\n  -", x,"  =  ",tmp,"\n")
                            unlist(tmp)
                          })
                        })
    noms<-data.frame(common_name=switch(nameType,sci=outNames,common=speciesNames),scientific_name=switch(nameType,sci=speciesNames,common=outNames),row.names = NULL)

invisible(noms)
}
