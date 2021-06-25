#' getWikiPic
#'
#' Get the thumbnail image from any Wikipedia page
#'
#' Downloads the top right image in the infobox of a wikipage determined by your search term. Also caches this image so if you ask for the same image again, it doesn't download it.
#'
#' @param x search term for finding a Wikipedia page
#' @param width the width of the downloaded image in pixels; default=220
#' @param picSaveDir the location to download images to; default=tempdir(); use paste0(getwd(),"/wiki") to save to a subfolder in your working directory or file.choose() to pic a location on the fly
#' @param quiet suppress verbose feedback? default=T
#' @param openDir open the picSaveDir location to browse images in finder/windows explorer? default=F
#' @param clearCache WARNING delete the picSaveDir? This will delete the whole directory, so never set picSaveDir to getwd(); default=F
#'
getWikiPic<-function(x,width=220,picSaveDir=tempdir(),quiet=T,openDir=F,clearCache=F){
  message("\n",rep("-",50),"\n  Downloading Wikipedia Pics\n",rep("-",50))
  if(clearCache){unlink(picSaveDir,recursive=T)}#delete cache if requested
  dir.create(picSaveDir,showWarnings=!quiet)
  imgs<-pbapply::pblapply(x, function (ttl,...){
    savefilename<-fs::path(picSaveDir,paste0(gsub(" ","_",ttl),"_",width,"px"),ext="jpeg")
          #Check if exists. Don't download if it does
          if(!file.exists(savefilename)){
              d<-WikipediR::page_info("en","wikipedia",page=ttl,clean_response=T,ext="jpeg")
              url<-d[[1]]$fullurl
              wikipage<-if(quiet){suppressWarnings(rvest::session(url))}else{rvest::session(url)}
              #check if there's a wiki page for this
              if(wikipage$response$status_code!=404){
                imginfo<-rvest::html_elements(wikipage,".infobox img")
                img.url0<- rvest::html_attr(imginfo[1] ,"src")
                img.url<-paste0("https:",img.url0)
                  if(width!=220){
                    img.url<-gsub("/220px-",paste0("/",width,"px-"),img.url)
                  }

                dlTest <- try(utils::download.file(img.url,savefilename,quiet=quiet),silent=quiet)
                if(inherits(dlTest,"try-error")){
                  message("\n Img download failed for '",ttl,"'.\n")
                  savefilename <- NA
                  }else{ message("\n Img saved for ",ttl,": ",basename(img.url),"\n")}#tell user original filename (or error)
              }else{
                #if there is a 404 code
                savefilename <- NA
                message("\n!! No Wikipedia page found for ",ttl,"!!\n")
              }
          }else{message("\n  Skipping ",ttl,"; already downloaded.**\n")}
          savefilename
      },width,picSaveDir)#End lapply

  #If requested,open the containing folder
  if(openDir){system(paste0("open ",picSaveDir))}
  out<-data.frame(search_term=x,img_loc=unlist(imgs),row.names=NULL)
  invisible(out)
}#End function
