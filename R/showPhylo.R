#' showPhylo
#'
#' A *relatively* simple function for generating presentation-ready phylogenies, given a set of common or scientific names. It works best for a few species, since the goal is to make a phylogeny for pedagogical purposes, so the focus is on details, not quantity.
#'
#' You have the ability to look up matching common or scientific names for taxa, plot them in an evolutionary tree based on the Open Tree of Life Project, date taxa using \code{\link[datelife]{datelife_search}}, if data are available, plot a \href{http://phylopic.org/}{phylopic} (i.e. a silhouette of the organism) if available, pull down the profile image from each organism's Wikipedia page, or provide a custom image, and label the pictures with the scientific and common names. The result is a customizable ggplot2 object. This is a convenience wrapper that merges functionality from \pkg{ggplot2}, \pkg{ggtree}, \pkg{datelife}, \pkg{rotl},and \pkg{taxize}, among other packages.
#'
#' The impetus for this package was the need to make a figure showing that the many animals we call "panthers" are actually really distantly related cat species. Our genetic rescue lesson, sponsored by \href{http://swfitz.com/}{Dr. Sarah Fitzpatrick's lab}, uses the example of the Florida panther as a charismatic application of her research (which mostly uses guppies as a model for understanding how gene flow can save small, highly inbred populations from extinction).
#'
#' @param speciesNames a name or vector of common or scientific names of organisms of interest (in quotes)
#' @param nameType what type of name are you supplying? Either "sci" or "common"
#' @param dateTree try to scale the tree to estimated divergence times? Uses \code{\link[datelife]{datelife_search}} with summary_format="phylo_median." default= T
#' @param labelType which names to label tree "leaves"? Options= "s" for scientific, "c" for common, and "b" for both (default="b")
#' @param labelOffset how far from the tree tips do you want to put labels? default=0.3 (in proportion of x-axis units)
#' @param aspectRatio doesn't actually work yet; the output phylogeny is always square for the moment
#' @param pic what type of species image do you want to plot? options="wiki" (Wikipedia page profile image), "phylopic" (species' silhouette from the PhyloPic repository), "cust" (custom images: must be named .jpg or .png with names matching speciesNames in the picSaveDir folder), or "none"
#' @param dotsConnectText do you want a dotted line to go from the text to the labels? default=FALSE
#' @param picSize how big to scale images, where 1=100%; .5=50%; default=1
#' @param picSaveDir location for saving downloaded images; default=fs::path(tempdir(),"showPhylo")
#' @param optPicWidth picture width in pixels for optimized versions of images saved if using pic="cust"; default= 200
#' @param picBorderWidth for pic="wiki" or "cust," what size border would you like around your image (as a %); default=10
#' @param picBorderCol color of image border; default="#363636" (a dark, near-black color)
#' @param openDir for pic="wiki" or "cust," do you want to open the picSaveDir after processing files? default=FALSE
#' @param xAxisPad spacing between the phylogeny and the x-axis (scale is 1 is the distance between two sister taxa); default=.2
#' @param xTitlePad spacing between x-axis title and x numbers; default=20
#' @param numXlabs the number of year markers on the x-axis; default=8
#' @param textScalar multiplier of text size for labels and axis numbers; default=1
#' @param xTitleScalar multiplier of x-axis title size; default=1
#' @param phyloThickness how thick to make the phylogeny lines; default=1.2
#' @param phyloCol color of the phylogeny lines; default= "#363636"
#' @param textCol color of the axis and tip labels; default= "#363636"
#' @param plotMar margins around the plot area in proportional screen width units; note the right margin is much wider to make room for tip labels; default=c(t=.02,r=.1,b=.02,l=.02) for top, right, bottom, left
#' @param clearCache delete cached images and taxonomic names? Passed to getPhyloNames, getWikiPics, and also applies to optimized custom images; default=FALSE
#' @param quiet suppress verbose feedback from the taxize package? Passed to getPhyloNames and get WikiPic helper functions. Default=TRUE
#' @param silent suppress all console output? (Mainly for R documentation); Default=FALSE
#' @param ... pass other parameters to \code{\link[ggtree]{ggtree}}
#' @md
#' @import ggtree datelife
#' @export
#'
#' @examples
#' require(ggplot2);require(ggtree)
#' #declare some species common names
#' speciesNames <- c("puma","leopard","jaguar","domestic cat")
#'
#' #Make a dated phylogeny with them (by default, median divergence times are estimated
#' #using the datelife package and images are added from Wikipedia)
#' showPhylo(speciesNames,"c")
#'
#' #If a particular combination of organisms lacks divergence time data (particularly extinct species),
#' #causing an error
#' \dontrun{
#' showPhylo(c("potbellied seahorse","leafy seadragon",
#' "oarfish","Tyrannosaurus"),"common")
#' }
#' #you can possibly still produce an undated phylogeny (cladogram) using the open tree of life data
#' showPhylo(c("potbellied seahorse","leafy seadragon",
#' "oarfish","Tyrannosaurus"),"c",dateTree=FALSE)
#'
#' #Instead of Wikipedia images for each species, we can try to find a phylopic (silhouette)
#' showPhylo(c("potbellied seahorse","leafy seadragon","oarfish",
#' "Tyrannosaurus"),"c",dateTree=FALSE,pic="phylopic")
#'
#' #In the case above, the butterfly was the wrong image, so we could try searching using the genus,
#' # (note: change nameType to "scientific");
#' showPhylo(c("Hippocampus","phycodurus","Regalecus","Tyrannosaurus"),
#' "scientific",dateTree=FALSE,pic="p")
#'
#' #Not great, but \href{http://phylopic.org/}{phylopic} is open source,
#' #so you can upload your own silhouette
#'
#' #If you have a ton of species, you can just say no pics
#' noms<-c("rock hyrax","Hippopotamus","Eastern gray squirrel","Asian elephant",
#' "African elephant","groundhog","meerkat","wolverine")
#' showPhylo(noms,"c",pic="n",dateTree=FALSE)
#'
#' #Doesn't look great, so we can connect the tree tips with the text
#' #and make the tree blue (why not?); Also, let's just show common names
#' showPhylo(noms,"c",pic="n",dateTree=FALSE,phyloCol="royalblue1",dotsConnectText=TRUE,nameType="c")
#'
#' #Now let's shorten the gap between the tree and the text, and increase text size
#' (g<-showPhylo(noms,"c",pic="n",dateTree=FALSE,phyloCol="royalblue1",dotsConnectText=TRUE,
#' nameType="c",labelOffset=.2,textScalar=1.2))
#'
#' #once we're satisfied with the tree, we can edit it like any other ggplot
#' (g2<-g+labs(title="Phylogeny showing how weird rock hyraxes are:",
#' subtitle="They look like groundhogs, but are much closer related to elephants")+
#' theme(plot.title=element_text(size=18)) )
#'
#' #we can also use \href{https://yulab-smu.top/treedata-book/chapter5.html}{ggtree functions}
#' #(like \code{\link[ggtree]{geom_highlight}} to do all sorts of
#' #stuff, like highlight our key species (you can figure out which node that is here with g2$data)
#' g2+geom_highlight(mapping=aes(subset=node==6),fill="salmon")
#'
#'
#' #OK, let's simplify it, add divergence time and get some pics back
#' (g3<-showPhylo(noms[c(1,3,5,6,7)], "c" ))
#' g3+geom_highlight(mapping=aes(subset=node==4),fill="salmon")+
#' labs(title="Phylogeny showing how weird rock hyraxes are:",
#' subtitle="They look like groundhogs, but are much closer related to elephants")+
#' theme(plot.title=element_text(size=18))
#'
#' #to get the right output dimensions, you may need to play around with setting height & width
#' # e.g. ggsave("hyrax.jpeg",height=3,width=3)
#'
showPhylo_backend<-function(speciesNames,nameType,dateTree=TRUE,labelType="b",labelOffset=.45,aspectRatio=1,pic="wiki",dotsConnectText=FALSE,picSize=1,picSaveDir,optPicWidth=200,picBorderWidth=10,picBorderCol="#363636",openDir=FALSE,xAxisPad=.2,xTitlePad=20,numXlabs=8,textScalar=1,xTitleScalar=1,phyloThickness=1.2,phyloCol="#363636",textCol="#363636",plotMar=c(t=.02,r=.36,b=.02,l=.02),clearCache=FALSE,quiet=TRUE,silent=FALSE,...){

  #for testing
  #list2env(list(speciesNames=c("bandicoot","numbat","tasmanian devil","koala"),nameType="c",dateTree=TRUE,labelType="b",labelOffset=.45,aspectRatio=1,pic="wiki",dotsConnectText=FALSE,picSize=1,picSaveDir=tempdir(),optPicWidth=200,picBorderWidth=10,picBorderCol="#363636",openDir=FALSE,xAxisPad=.2,xTitlePad=20,numXlabs=8,textScalar=1,xTitleScalar=1,phyloThickness=1.2,phyloCol="#363636",textCol="#363636",plotMar=c(t=.02,r=.36,b=.02,l=.02),clearCache=FALSE,quiet=TRUE,silent=FALSE),envir=globalenv())


  #Check for extra missing dependencies
  missingpkgs<-unlist(sapply(c("magick"),function(pkg){
   if (!requireNamespace(pkg, quietly = TRUE)) {pkg}else{NULL}
    }))
  if(length(missingpkgs>0)){
  message("Extra dependencies missing:\n\t-",paste0(missingpkgs,collapse="\n\t-"))
  message("Consider running: install.packages(pkgs=c(",paste0("'",missingpkgs,"'",collapse=", "),"))")
  }
    if(missing(nameType)){stop("\nPlease supply the type of names you're providing; i.e. nameType= either 'sci' or 'common'")}
    if(missing(picSaveDir)){picSaveDir<-fs::path(tempdir(),"showPhylo")}

    #allow for abbreviated nameType specification
    nameType_l<-substr(nameType,1,1)
    if(!nameType_l%in%c("s","c")){stop("nameType must be one of 's' or 'c' for scientific or common names, respectively")}
    nameType<-switch(nameType_l,s="sci",c="common")
    # 1. Lookup, error check, & compile a df of sci and common names --------------
    spp<-getPhyloNames(speciesNames,nameType,clearCache = clearCache,quiet=quiet)

    #allow for abbreviated pic specification (and test it)
    pic_l<-substr(pic,1,1)
    if(!pic_l%in%c("w","p","c","n")){stop("pic must be one of 'w' 'p' 'c' or 'n' for Wikipedia, Phylopic, custom or none, respectively.")}
    pic<-switch(pic_l,w="wiki",p="phylopic",c="cust",n="none")

    #Now search for matches to scientific names in Open Tree of Life
    ## Provide error catching framework b/c sometimes the OTL server is down
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
    #if there are no matches, throw error
    if(sum(is.na(tol_taxa$unique_name)>0)){stop("\n *Some species records not matched. Try changing your search terms.")}

    # tidying/flagging extinct ------------------------------------------------
    #pull out "extinct" flag to add qualifier for extinct taxa
    tol_taxa$extinct<-ifelse(grepl("extinct",tol_taxa$flags,fixed=T)," \"*Extinct*\"","")
    #make consistent common name capitalization and add extinction flag if appropriate
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
      tryCatch({
        message(rep("-",55),"\n Attempting to scale the tree to divergence times...\n",rep("-",55))
        message(" Tip: If it takes more than a few seconds, it's probably going to fail. (You may want to hit stop).\n")
        tree_final<-datelife::datelife_search(tree,summary_format="phylo_median")

       },error=function(e) {
        stop("\n! Tree dating FAILED !\n Try setting quiet=F to get more warnings, removing taxa, or setting dateTree=F.\n")})
    }else{tree_final<-tree}

    # This modifies tip.labels destructively ----------------------------------
    #make an index to go between tree tips and tol_taxa object
    tree_final$tip.label.backup<-tree_final$tip.label


    # Clean names -------------------------------------------------------------
    #in case of weird names with parentheticals; e.g. Phyllopteryx (genus in Deuterostomia)
    tol_names_cleaned<-gsub("(.*) ?[(].*[)](.*)","\\1\\2",tol_taxa$unique_name)
    tipIndx<-match(tree_final$tip.label.backup,gsub(" ","_",tol_names_cleaned))
    # sci_tmp<-gsub(" ","~",tol_names_cleaned[tipIndx])
    # com_tmp<-paste0("(",gsub(" ","~",tol_taxa$common_name[tipIndx]),")")
    sci_tmp<-tol_names_cleaned[tipIndx]
    com_tmp<-tol_taxa$common_name[tipIndx]
    sc_tmp<-lapply(paste0("'bolditalic(",sci_tmp,")' \n(",com_tmp,")''"),function(x) str2expression(x))
    tree_final$tip.label<-switch(labelType,b= sc_tmp,
                                           c= paste0("(",com_tmp,")"),
                                           s= expression(bolditalic(sci_tmp)))
    # tree_final$tip.label<-switch(labelType,b= paste0("atop(bolditalic(",sci_tmp,"),'",
    #                                           gsub("([^~()*]*'[^~()*]*)","\"\\1\"",fixed=F,com_tmp)    ,"')"),
    #                                        c= gsub("[()]","",com_tmp),
    #                                        s= sci_tmp)


    # Look up and cache phylopic image UIDs in an efficient manner ------------
      if(pic=="phylopic"){
        #check for cached phylopic UIDs, cuz this is slooooow
        tmpfile_uid<-fs::path(tempdir(),"phyloUIDcache",ext="rds")
        #delete cache if requested
        if(clearCache){unlink(tmpfile_uid,recursive=T)}

        if(!file.exists(tmpfile_uid)){
        message(rep("-",45),"\n  Looking for PhyloPics for your species...(slow)\n",rep("-",45))
        phylopic_error<-tryCatch({
          pic_uid<-do.call(rbind,  pbapply::pblapply(tree_final$tip.label.backup,function(x) ggimage::phylopic_uid(x)) )
        },error=function(e){message("PhyloPic did not work for some reason: ",e)}
        )
        saveRDS(pic_uid,tmpfile_uid)
        pic_uid_final <- pic_uid
        }else{
        #if we've already cached phylo info, compare new names and see if we can just tack on a few more
        pic_uid_cached<-readRDS(tmpfile_uid)
        noncached_taxa<-tree_final$tip.label.backup[which(is.na(match(tree_final$tip.label.backup,pic_uid_cached$name)))]
        if(length(noncached_taxa)==0){
          pic_uid_final<-pic_uid_cached[match(tree_final$tip.label.backup,pic_uid_cached$name),]
          }else{
          #lookup and append the missing taxa to cache
          message(rep("-",45),"\n  Looking up Phylopic IDs for taxa not already cached:\n",rep("-",45))
          message("\n\n  -",paste0(noncached_taxa,collapse="\n  -"))
          pic_uid_new<-do.call(rbind,  pbapply::pblapply(noncached_taxa,function(x) ggimage::phylopic_uid(x)) )
          pic_uid<-rbind(pic_uid_cached,pic_uid_new)
          saveRDS(pic_uid,tmpfile_uid)
          #now filter out to just the relevant ones
          pic_uid_final<-pic_uid[match(tree_final$tip.label.backup,pic_uid$name),]
          }
        }
      }

# Get Wikipedia main pic --------------------------------------------------
    #initialize addIMg
    addImg=F
    if(pic=="wiki"){
      wikiPics<-getWikiPic(tree_final$tip.label.backup,picSaveDir = picSaveDir,clearCache=clearCache,openDir=openDir)
      wikiPics$name<-tree_final$tip.label.backup
      #If scientific name didn't come up with anything, try common name
      if(sum(is.na(wikiPics$img_loc))>0){
          missingImgs<-which(is.na(wikiPics$img_loc))
          common_names_in_order_of_tips<-gsub("\\(|\\)","",
                                              tol_taxa$common[match(tree_final$tip.label.backup,
                                                                    gsub(" ","_",tol_names_cleaned))])
          #replace search_term with common name
          wikiPics$search_term[missingImgs]<-common_names_in_order_of_tips[missingImgs]
          #search again
          message("Trying common name for missing species ")
          wikiPics[missingImgs,1:2]<-getWikiPic(wikiPics$search_term[missingImgs],picSaveDir = picSaveDir)
      }

      #Use image Magick to add border if requested
      if(picBorderWidth>0){
        wikiPics$img_loc_border<-sapply(wikiPics$img_loc,function(x){
          if(is.na(x)){
            NA
          }else{
            oldfile<-x
            newfile_border<-fs::path(picSaveDir,paste0(gsub("^(.*)\\..*$","\\1",basename(x)), "_border.jpg"))
            #check if this file already exists
            if(file.exists(newfile_border)){
              message(" -",paste0(basename(x),"_border.jpg  : Already Exists"))
              newfile_border
            }else{
            img<-magick::image_read(x)
            img<-magick::image_border(img,picBorderCol,paste0(picBorderWidth,"%x",picBorderWidth,"%"))
            # #Rescale to desired pixel width
            # img<-magick::image_scale(img,optPicWidth)

            #Rescale to desired pixel width (This constrains height to the width value to prevent overlap!!)
            img<-magick::image_scale(img,paste0(optPicWidth,"x",optPicWidth))
            #write bordered file
            magick::image_write(img,newfile_border)
            newfile_border
            }
          }
        })
        #output paths to bordered images
        imgLoc<-wikiPics$img_loc_border
        #If no border width requested, just plot originals
      }else{imgLoc<-wikiPics$img_loc}
      addImg <- T
    }




# Import custom images if supplied ----------------------------------------
    if(pic=="cust"){
      #check if images exist
      message(rep("-",45),"\n Checking for custom species images in picSaveDir=\n  > ",picSaveDir,"\n",rep("-",50))
      #Reorder tol_taxa to match tree as our Rosetta for matching file names
      tol_taxa.orderedByTree<-tol_taxa[match(tree_final$tip.label.backup,gsub(" ","_",tol_names_cleaned)),]
      #search_string
      img_files<-list.files(fs::path(picSaveDir),pattern="\\.png|\\.jpeg|\\.jpg")
      img_files.stndzd <- gsub(" |-","_",tolower(img_files))
      speciesNames.stndzd<-gsub(" |-","_",tolower(tol_taxa.orderedByTree$searchNames.user))
      IMGs <- sapply(speciesNames.stndzd,function(x){
        img_indx<-grep(x,img_files.stndzd)
        img_filename<-ifelse(length(img_indx)==0,NA,fs::path(picSaveDir,img_files[img_indx[1]]))#indx[1] is in case of multiple hits; take first
      })
      if(sum(is.na(IMGs)>0)){
        warning("Missing images for:\n -",paste0(names(IMGs)[which(is.na(IMGs))],collapse=" -"))
        }

      # Manipulate images to make them display faster and add a border if requested
      optImg_loc<-fs::path(picSaveDir,"opt_img_for_showPhylo")
      #Delete cached optimized photos if clearCache==T
      if(clearCache){unlink(optImg_loc,recursive=T)}

      #make optimized pic folder if it doesn't exist
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
          #preserves spaces, removes ext; Not sure if I should keep spaces, but respecting user's prefs
          baseName<-gsub("^(.*)\\..*$","\\1",basename(x))
          newfile<-fs::path(optImg_loc,paste0(baseName,"_",optPicWidth,"px"),ext="jpg")
          if(file.exists(newfile)){
            message(" - ",basename(newfile)," : Already exists")
          }else{
            #Work the image Magick
            img<-magick::image_read(x)

            #Rescale to desired pixel width
            img<-magick::image_scale(img,paste0(optPicWidth,"x",optPicWidth))

            #add border
            if(picBorderWidth>0){
              img<-magick::image_border(img,picBorderCol,paste0(picBorderWidth,"%x",picBorderWidth,"%"))
            }

            #write optimized file
            magick::image_write(img,newfile)
            message(" - ",basename(newfile)," : SAVED")
          }
          newfile
        }
      })
      addImg <- T
      imgLoc<-optimizedIMGs
       #If requested,open the containing folder
    if(openDir){system(paste0("open ",optImg_loc))}
    }#end custom image code


    #interpret user plotMar specifications, accepting partial entries
    plotMar_defaults<-c(t=.02,r=.2,b=.02,l=.02)
    if(length(plotMar)!=4){
      #for partial entries, check for names, then fill in with plotMar defaults
      if(is.null(names(plotMar))){
        message("! You must supply margin names with custom plotMar; e.g. plotMar=c(r=.25) or  a full set of dimension: plotMar=c(0,.25,0,0)")
        warning("Ignoring incorrectly specified plotMar")
        plotMar_final<-plotMar_defaults
      }else{
        #if correctly specified...
        plotMar_final<-plotMar_defaults
        plotMar_final[which(names(plotMar_defaults)%in%names(plotMar))]<-plotMar
      }
      #else, if 4 coordinates specified, simply store them
    }else{plotMar_final<-plotMar}

    # Plot that beautiful tree :) ---------------------------------------------

    #Define custom theme to override a lot of ggtree's styling (if we want to plot)
    theme_phylo<-ggplot2::theme(plot.margin=ggplot2::margin(plotMar_final,unit="npc"),
                                panel.border=ggplot2::element_blank())

    #Define basic tree plot before modifying in steps
    g0<-ggtree::ggtree(tree_final,size=phyloThickness,color=phyloCol,...)+theme_phylo

    #Extract some info from base plot
    timescale<-ggplot2::layer_scales(g0)$x$get_limits()[2]
    timescale_rounded <- ceiling(timescale/10)*10
    yscale<-ggplot2::layer_scales(g0)$y$get_limits()
    textOffset=labelOffset*timescale
    picSized=0.15*picSize
    picOffset=textOffset/2
    backgroundRec<-data.frame(xmin=timescale+picOffset-(picSized*timescale*.7),xmax=timescale+picOffset+(picSized*timescale*.7),
                              ymin=yscale[1]-.5,ymax=yscale[2]+.5)



    #Rescale to have a 50% buffer on the right to add text
    g <- g0+ggplot2::scale_x_continuous(breaks=seq(timescale,0,-timescale/(numXlabs-1)),
                                      labels=round(seq(0,timescale,timescale/(numXlabs-1))))  +
      ggplot2::coord_cartesian(ylim=c(yscale[1]-xAxisPad,yscale[2]),clip='off')+
      #Add text labels
      ggtree::geom_tiplab(geom='text',vjust=0.5,hjust=0,parse=F,offset=textOffset,align=dotsConnectText,
                  color=textCol,size=5*textScalar)  + #,label.padding=ggplot2::unit(1,"lines")
      # ggplot2::coord_fixed(aspectRatio,clip="off",ylim=c(yscale[1]-xAxisPad,yscale[2]))+

      #add semitransparent rectangle between dotted line and phylopic
      #geom_rect(inherit.aes=F,data=backgroundRec,aes(xmin=xmin,ymin=ymin, xmax=xmax,ymax=ymax),fill="white",alpha=.7)+
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

    #dateTree formatting has to be in 2 steps cuz aPPARENTLY you can add 2 layers in 1 if/then :(
    if(dateTree){
    g+ggplot2::theme(axis.ticks.x=ggplot2::element_line(color=phyloCol),
                  axis.ticks.length.x=ggplot2::unit(3,"pt"),
                  axis.title.x=ggplot2::element_text(margin=ggplot2::margin(xTitlePad,0,3,0),face="bold",size=26*textScalar),
                  axis.text.x=ggplot2::element_text(color=textCol,size=21*textScalar),
                  axis.line.x=ggplot2::element_line(color=phyloCol))
    }else{g}

}

#Run the code through helper function to handle suppression of all output (silent mode)
showPhylo<-function(speciesNames,nameType,dateTree=TRUE,labelType="b",labelOffset=.45,aspectRatio=1,pic="wiki",dotsConnectText=FALSE,picSize=1,picSaveDir,optPicWidth=200,picBorderWidth=10,picBorderCol="#363636",openDir=FALSE,xAxisPad=.2,xTitlePad=20,numXlabs=8,textScalar=1,xTitleScalar=1,phyloThickness=1.2,phyloCol="#363636",textCol="#363636",plotMar=c(t=.02,r=.36,b=.02,l=.02),clearCache=FALSE,quiet=TRUE,silent=FALSE,...){
  #Don't fully understand why this works, but we're passing all arguments into backend function
  mycall<-match.call()
  mycall[[1]]<-as.symbol("showPhylo_backend")

  if(silent){
  invisible(capture.output(suppressMessages(g<-eval(mycall))))
    plot(g)
  }else{eval(mycall)}
}

#
# showPhylo(c("giraffe","monarch butterfly","house fly","electric eel","blue bottle fly","mantisfly"),nameType="c",plotMar=c(r=.32),picSize=1,labelOffset = .45,silent=T,dateTree=F)
#ggsave("test.jpg",width=7,height=10)
#
# showPhylo(speciesNames=c("lion","ocelot","puma","leopard","jaguar","domestic cat"),nameType="c")
