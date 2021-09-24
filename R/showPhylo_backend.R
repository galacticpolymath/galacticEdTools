#' showPhylo_backend
#'
#' helper function for showPhylo that does all the work
#'
#' @inheritParams showPhylo


showPhylo_backend<-function(speciesNames,nameType,dateTree=TRUE,labelType="b",labelOffset=.45,aspectRatio=1,pic="wiki",dotsConnectText=FALSE,picSize=1,picSaveDir,optPicWidth=200,picBorderWidth=10,picBorderCol="#363636",openDir=FALSE,xAxisPad=.2,xTitlePad=20,numXlabs=8,textScalar=1,xTitleScalar=1,phyloThickness=1.2,phyloCol="#363636",textCol="#363636",plotMar=c(t=.02,r=.4,b=.02,l=.02),clearCache=FALSE,quiet=TRUE,silent=FALSE,...){

  # for testing
  # list2env(list(speciesNames=c("bandicoot","numbat","tasmanian devil","koala"),nameType="c",dateTree=TRUE,labelType="b",labelOffset=.45,aspectRatio=1,pic="wiki",dotsConnectText=FALSE,picSize=1,picSaveDir=tempdir(),optPicWidth=200,picBorderWidth=10,picBorderCol="#363636",openDir=FALSE,xAxisPad=.2,xTitlePad=20,numXlabs=8,textScalar=1,xTitleScalar=1,phyloThickness=1.2,phyloCol="#363636",textCol="#363636",plotMar=c(t=.02,r=.4,b=.02,l=.02),clearCache=FALSE,quiet=TRUE,silent=FALSE),envir=globalenv())


  # Check for extra missing dependencies
  missingpkgs<-unlist(sapply(c("magick"),function(pkg){
   if (!requireNamespace(pkg, quietly = TRUE)) {pkg}else{NULL}
    }))
  if(length(missingpkgs>0)){
  message("Extra dependencies missing:\n\t-",paste0(missingpkgs,collapse="\n\t-"))
  message("Consider running: install.packages(pkgs=c(",paste0("'",missingpkgs,"'",collapse=", "),"))")
  }
    if(missing(nameType)){stop("\nPlease supply the type of names you're providing; i.e. nameType= either 'sci' or 'common'")}
    if(missing(picSaveDir)){picSaveDir<-fs::path(tempdir(),"showPhylo")}

    # allow for abbreviated nameType specification
    nameType_l<-substr(nameType,1,1)
    if(!nameType_l%in%c("s","c")){stop("nameType must be one of 's' or 'c' for scientific or common names, respectively")}
    nameType<-switch(nameType_l,s="sci",c="common")
    # 1. Lookup, error check, & compile a df of sci and common names --------------
    spp<-getPhyloNames(speciesNames,nameType,clearCache = clearCache,quiet=quiet)

    # allow for abbreviated pic specification (and test it)
    pic_l<-substr(pic,1,1)
    if(!pic_l%in%c("w","p","c","n")){stop("pic must be one of 'w' 'p' 'c' or 'n' for Wikipedia, Phylopic, custom or none, respectively.")}
    pic<-switch(pic_l,w="wiki",p="phylopic",c="cust",n="none")

    # Now search for matches to scientific names in Open Tree of Life
    # # Provide error catching framework b/c sometimes the OTL server is down
    message("\n Trying to match scientific names with Open Tree of Life")
    message("\n *You may be asked to choose a number if there are multiple matches.\n")
    prob_rotl<-tryCatch({
    tol_taxa<-rotl::tnrs_match_names(spp$scientific_name,do_approximate_matching = F)
    },error=function(e){message("\n! Open Tree of Life lookup failed.");e}
    )
    if("error"%in%class(prob_rotl)){stop(prob_rotl)}

    message(rep("-",35),"\nOTL matching results\n",rep("-",35))
    print(tol_taxa[,])
    message(rep("-",35))
    # if there are no matches, throw error
    if(sum(is.na(tol_taxa$unique_name)>0)){stop("\n *Some species records not matched. Try changing your search terms.")}

    # tidying/flagging extinct ------------------------------------------------
    # pull out "extinct" flag to add qualifier for extinct taxa
    tol_taxa$extinct<-ifelse(grepl("extinct",tol_taxa$flags,fixed=T)," \"*Extinct*\"","")
    # make consistent common name capitalization and add extinction flag if appropriate
    tol_taxa$common_name<-paste0(tools::toTitleCase(spp$common_name),tol_taxa$extinct)
    tol_taxa$searchNames.user<-speciesNames


    # Make tree from scientific names in tol_taxa -----------------------------
    prob<-tryCatch(
      tree<-if(quiet){suppressWarnings(rotl::tol_induced_subtree(rotl::ott_id(tol_taxa),label="name"))
            }else{rotl::tol_induced_subtree(rotl::ott_id(tol_taxa),label="name")},
      error=function(e) {
        message("\n! Tree Build FAILED\n* The Tree of Life Open Taxonomy system doesn't work super well with extinct organisms sometimes. Try removing them from your set.")
        e})
    if("error"%in%class(prob)){
      problematic<-gsub(".*'ott(\\d*)'.*","\\1",prob$message)
      message("Possible problem: ",tol_taxa$searchNames.user[which(tol_taxa$ott_id%in%problematic)])
      }

    # Dating the tree ---------------------------------------------------------
    if(dateTree){
      opentree_chronograms=NULL #initialize for R check issue
      tree_taxa<-paste0(tree$tip.label,collapse=",")
      #look for cached datelife entries
      tmpfile_datelife<-fs::path(tempdir(),"datelifecache",ext="rds")
      # delete cache if requested
      if(clearCache){unlink(tmpfile_datelife,recursive=T)}
      #if there's a cache, read it in and see if it has a tree with the desired species
      if(file.exists(tmpfile_datelife)){
        datelife_cache_list<-readRDS(tmpfile_datelife)
        # Does it have the right species? Test it.
        species_cached <- tree_taxa%in%names(datelife_cache_list)

      }else{
        datelife_cache_list=list()
        species_cached=F
        }

      # Now we either have an imported cache list or an empty list
      # If we have the species in cache, make them available
      if(species_cached){
        tree_final<-datelife_cache_list[[match(tree_taxa,names(datelife_cache_list))]]
      }else{
        tryCatch({
          message(rep("-",55),"\n Attempting to scale the tree to divergence times...\n",rep("-",55))
          message(" Tip: If it takes more than a few seconds, it's probably going to fail. (You may want to hit stop).\n")
          tree_final<-datelife::datelife_search(tree,summary_format="phylo_median")
          #Add tree_final to datelife_cache_list, with the names of taxa as the list element name
          datelife_cache_list[[length(datelife_cache_list)+1]] <- tree_final
          names(datelife_cache_list)[length(datelife_cache_list)]<-tree_taxa
          saveRDS(datelife_cache_list,tmpfile_datelife)
          rm(datelife_cache_list)#Remove cache to save space!
          rm(opentree_chronograms)#remove object created by datelife
        },error=function(e) {
          stop("\n\n! Tree dating FAILED !\nTry setting quiet=F to get more warnings, removing taxa, or setting dateTree=F.\n\n")})
      }
    #if tree doesn't need to be dated
    }else{tree_final<-tree}


    # make an index to go between tree tips and tol_taxa object
    tree_final$tip.label.backup<-tree_final$tip.label


    # Clean names -------------------------------------------------------------
    # in case of weird names with parentheticals; e.g. Phyllopteryx (genus in Deuterostomia)
    tol_names_cleaned<-gsub("(.*) ?[(].*[)](.*)","\\1\\2",tol_taxa$unique_name)
    tipIndx<-match(tree_final$tip.label.backup,gsub(" ","_",tol_names_cleaned))
    # sci_tmp<-gsub(" ","~",tol_names_cleaned[tipIndx])
    # com_tmp<-paste0("(",gsub(" ","~",tol_taxa$common_name[tipIndx]),")")
    sci_tmp<-tol_names_cleaned[tipIndx]
    com_tmp<-tol_taxa$common_name[tipIndx]
    sc_tmp<-paste0("***",sci_tmp,"***<br>(",com_tmp,")")
    tree_final$tip.label<-switch(labelType,b= sc_tmp,
                                           c= com_tmp,
                                           s= paste0("***",sci_tmp,"***"))
    # tree_final$tip.label<-switch(labelType,b= paste0("atop(bolditalic(",sci_tmp,"),'",
    #                                           gsub("([^~()*]*'[^~()*]*)","\"\\1\"",fixed=F,com_tmp)    ,"')"),
    #                                        c= gsub("[()]","",com_tmp),
    #                                        s= sci_tmp)


    # Look up and cache phylopic image UIDs in an efficient manner ------------
      if(pic=="phylopic"){
        # check for cached phylopic UIDs, cuz this is slooooow
        tmpfile_uid<-fs::path(tempdir(),"phyloUIDcache",ext="rds")
        # delete cache if requested
        if(clearCache){unlink(tmpfile_uid,recursive=T)}

        if(!file.exists(tmpfile_uid)){
        message(rep("-",45),"\n  Looking for PhyloPics for your species...(slow)\n",rep("-",45))
        phylopic_error<-tryCatch({
          pic_uid<-do.call(rbind,  pbapply::pblapply(tree_final$tip.label.backup,function(x) ggimage::phylopic_uid(x)) )
        },error=function(e){message("PhyloPic did not work for some reason: ",e)}
        )
        saveRDS(pic_uid,tmpfile_uid)
        pic_uid_final <- pic_uid
        pic_uid_cached<-NA
        }else{
        # if we've already cached phylo info, compare new names and see if we can just tack on a few more
        pic_uid_cached<-readRDS(tmpfile_uid)
        noncached_taxa<-tree_final$tip.label.backup[which(is.na(match(tree_final$tip.label.backup,pic_uid_cached$name)))]
        if(length(noncached_taxa)==0){
          pic_uid_final<-pic_uid_cached[match(tree_final$tip.label.backup,pic_uid_cached$name),]
          }else{
          # lookup and append the missing taxa to cache
          message(rep("-",45),"\n  Looking up Phylopic IDs for taxa not already cached:\n",rep("-",45))
          message("\n\n  -",paste0(noncached_taxa,collapse="\n  -"))
          pic_uid_new<-do.call(rbind,  pbapply::pblapply(noncached_taxa,function(x) ggimage::phylopic_uid(x)) )
          pic_uid<-rbind(pic_uid_cached,pic_uid_new)
          saveRDS(pic_uid,tmpfile_uid)
          # now filter out to just the relevant ones
          pic_uid_final<-pic_uid[match(tree_final$tip.label.backup,pic_uid$name),]
          }
        }
        rm(pic_uid_cached)#remove phylopic cache to save memory
      }

# Get Wikipedia main pic --------------------------------------------------
    # initialize addIMg
    addImg=F
    if(pic=="wiki"){
      wikiPics<-getWikiPic(tree_final$tip.label.backup,picSaveDir = picSaveDir,clearCache=clearCache,openDir=openDir)
      wikiPics$name<-tree_final$tip.label.backup
      # If scientific name didn't come up with anything, try common name
      if(sum(is.na(wikiPics$img_loc))>0){
          missingImgs<-which(is.na(wikiPics$img_loc))
          common_names_in_order_of_tips<-gsub("\\(|\\)","",
                                              tol_taxa$common[match(tree_final$tip.label.backup,
                                                                    gsub(" ","_",tol_names_cleaned))])
          # replace search_term with common name
          wikiPics$search_term[missingImgs]<-common_names_in_order_of_tips[missingImgs]
          # search again
          message("Trying common name for missing species ")
          wikiPics[missingImgs,1:2]<-getWikiPic(wikiPics$search_term[missingImgs],picSaveDir = picSaveDir)
      }

      # Use image Magick to add border if requested
      if(picBorderWidth>0){
        wikiPics$img_loc_border<-sapply(wikiPics$img_loc,function(x){
          if(is.na(x)){
            NA
          }else{
            oldfile<-x
            newfile_border<-fs::path(picSaveDir,paste0(gsub("^(.*)\\..*$","\\1",basename(x)), "_border.jpg"))
            # check if this file already exists
            if(file.exists(newfile_border)){
              message(" -",paste0(basename(x),"_border.jpg  : Already Exists"))
              newfile_border
            }else{
            img<-magick::image_read(x)
            img<-magick::image_border(img,picBorderCol,paste0(picBorderWidth,"%x",picBorderWidth,"%"))
            # # Rescale to desired pixel width
            # img<-magick::image_scale(img,optPicWidth)

            # Rescale to desired pixel width (This constrains height to the width value to prevent overlap!!)
            img<-magick::image_scale(img,paste0(optPicWidth,"x",optPicWidth))
            # write bordered file
            magick::image_write(img,newfile_border)
            newfile_border
            }
          }
        })
        # output paths to bordered images
        imgLoc<-wikiPics$img_loc_border
        # If no border width requested, just plot originals
      }else{imgLoc<-wikiPics$img_loc}
      addImg <- T
    }




# Import custom images if supplied ----------------------------------------
    if(pic=="cust"){
      # check if images exist
      message(rep("-",45),"\n Checking for custom species images in picSaveDir=\n  > ",picSaveDir,"\n",rep("-",50))
      # Reorder tol_taxa to match tree as our Rosetta for matching file names
      tol_taxa.orderedByTree<-tol_taxa[match(tree_final$tip.label.backup,gsub(" ","_",tol_names_cleaned)),]
      # search_string
      img_files<-list.files(fs::path(picSaveDir),pattern="\\.png|\\.jpeg|\\.jpg")
      img_files.stndzd <- gsub(" |-","_",tolower(img_files))
      speciesNames.stndzd<-gsub(" |-","_",tolower(tol_taxa.orderedByTree$searchNames.user))
      IMGs <- sapply(speciesNames.stndzd,function(x){
        img_indx<-grep(x,img_files.stndzd)
        img_filename<-ifelse(length(img_indx)==0,NA,fs::path(picSaveDir,img_files[img_indx[1]]))# indx[1] is in case of multiple hits; take first
      })
      if(sum(is.na(IMGs)>0)){
        warning("Missing images for:\n -",paste0(names(IMGs)[which(is.na(IMGs))],collapse=" -"))
        }

      # Manipulate images to make them display faster and add a border if requested
      optImg_loc<-fs::path(picSaveDir,"opt_img_for_showPhylo")
      # Delete cached optimized photos if clearCache==T
      if(clearCache){unlink(optImg_loc,recursive=T)}

      # make optimized pic folder if it doesn't exist
      dir.create(optImg_loc,showWarnings = F)
      message(rep("-",45),"\n Optimizing custom images\n  Params:",
              "\n  |_ optPicWidth= ",optPicWidth,"px\n  |_ picBorderWidth= ",picBorderWidth,
              "\n  |_ picBorderCol= ",picBorderCol,"\n",rep("-",50),"\n")
      optimizedIMGs<-sapply(1:length(IMGs),function(i){
        x<-IMGs[i]
        if(is.na(x)){
          warning(" - ","!! ",names(IMGs[i])," IMAGE MISSING")
          NA
        }else{
          # preserves spaces, removes ext; Not sure if I should keep spaces, but respecting user's prefs
          baseName<-gsub("^(.*)\\..*$","\\1",basename(x))
          newfile<-fs::path(optImg_loc,paste0(baseName,"_",optPicWidth,"px"),ext="jpg")
          if(file.exists(newfile)){
            message(" - ",basename(newfile)," : Already exists")
          }else{
            # Work the image Magick
            img<-magick::image_read(x)

            # Rescale to desired pixel width
            img<-magick::image_scale(img,paste0(optPicWidth,"x",optPicWidth))

            # add border
            if(picBorderWidth>0){
              img<-magick::image_border(img,picBorderCol,paste0(picBorderWidth,"%x",picBorderWidth,"%"))
            }

            # write optimized file
            magick::image_write(img,newfile)
            message(" - ",basename(newfile)," : SAVED")
          }
          newfile
        }
      })
      addImg <- T
      imgLoc<-optimizedIMGs
       # If requested,open the containing folder
    if(openDir){system(paste0("open ",optImg_loc))}
    }# end custom image code

    # ! IMPORTANT to keep this up to date with defaults set on function call
    # interpret user plotMar specifications, accepting partial entries
    plotMar_defaults<-c(t=.02,r=.4,b=.02,l=.02)
    #***************

    if(length(plotMar)!=4){
      # for partial entries, check for names, then fill in with plotMar defaults
      if(is.null(names(plotMar))){
        message("! You must supply margin names with custom plotMar; e.g. plotMar=c(r=.25) or  a full set of dimension: plotMar=c(0,.25,0,0)")
        warning("Ignoring incorrectly specified plotMar")
        plotMar_final<-plotMar_defaults
      }else{
        # if correctly specified...
        plotMar_final<-plotMar_defaults
        plotMar_final[which(names(plotMar_defaults)%in%names(plotMar))]<-plotMar
      }
      # else, if 4 coordinates specified, simply store them
    }else{plotMar_final<-plotMar}

    # Plot that beautiful tree :) ---------------------------------------------

    # Define custom theme to override a lot of ggtree's styling (if we want to plot)
    theme_phylo<-ggplot2::theme(plot.margin=ggplot2::margin(plotMar_final,unit="npc"),
                                panel.border=ggplot2::element_blank())

    # Define basic tree plot before modifying in steps
    g00<-ggtree::ggtree(tree_final,size=phyloThickness,color=phyloCol)+theme_phylo

    # Extract some info from base plot
    timescale<-ggplot2::layer_scales(g00)$x$get_limits()[2]
    timescale_rounded <- ceiling(timescale/10)*10
    yscale<-ggplot2::layer_scales(g00)$y$get_limits()
    textOffset=labelOffset*timescale
    picSized=0.15*picSize
    picOffset=textOffset/2
    backgroundRec<-data.frame(xmin=timescale+picOffset-(picSized*timescale*.7),xmax=timescale+picOffset+(picSized*timescale*.7),
                              ymin=yscale[1]-.5,ymax=yscale[2]+.5)


    # Rescale to have a 50% buffer on the right to add text
    g0 <- g00+ggplot2::scale_x_continuous(breaks=seq(timescale,0,-timescale/(numXlabs-1)),
                                      labels=round(seq(0,timescale,timescale/(numXlabs-1))))  +
      ggplot2::coord_cartesian(ylim=c(yscale[1]-xAxisPad,yscale[2]),clip='off')

    # Extract info for text labels
    label.df<-g0$data[which(g0$data$isTip==TRUE),]
    label.df$xend<-label.df$x
    label.df$x<-label.df$x+textOffset

    # Add text labels
    x=y=label=xend=yend=NULL #So R check will shut up
    g <- g0+ ggtext::geom_richtext(data=label.df,ggplot2::aes(x=x,y=y,label=label),inherit.aes=FALSE,
                               label.size=0,hjust=0,color=textCol,size=5.2*textScalar)+
           # ggplot2::coord_fixed(aspectRatio,clip="off",ylim=c(yscale[1]-xAxisPad,yscale[2]))+
      {
        if(dotsConnectText){
          ggplot2::geom_segment(data=label.df,ggplot2::aes(x=x,xend=xend,y=y,yend=y),linetype="dotted")
        }
      }+
      # add semitransparent rectangle between dotted line and phylopic
      # geom_rect(inherit.aes=F,data=backgroundRec,aes(xmin=xmin,ymin=ymin, xmax=xmax,ymax=ymax),fill="white",alpha=.7)+
      {
        if(pic=="phylopic"){
          ggtree::geom_tiplab(image=pic_uid_final$uid,geom="phylopic",color=textCol,hjust=0.5,
                      size=picSized,offset=picOffset,alpha=1)}else{}
      }+{
        if(addImg){
          ggtree::geom_tiplab(image=imgLoc,geom="image",size=picSized,offset=picOffset,alpha=1,hjust=0.5,asp=1)
        }else{}
      }+{
        if(dateTree){
         ggplot2::xlab("Millions of Years Ago (Ma)")
        }else{}
      }


# ggtext::geom_richtext(aes(x=x,y=y,label=label),data=g$data,inherit.aes=F,hjust=0,label.size=0)
    # dateTree formatting has to be in 2 steps cuz aPPARENTLY you can add 2 layers in 1 if/then :(
    if(dateTree){
    g+ggplot2::theme(axis.ticks.x=ggplot2::element_line(color=phyloCol),
                  axis.ticks.length.x=ggplot2::unit(3,"pt"),
                  axis.title.x=ggplot2::element_text(margin=ggplot2::margin(xTitlePad,0,3,0),face="bold",size=20*textScalar,hjust=0),
                  axis.text.x=ggplot2::element_text(color=textCol,size=21*textScalar),
                  axis.line.x=ggplot2::element_line(color=phyloCol))
    }else{g}

}#end showPhylo_backend
