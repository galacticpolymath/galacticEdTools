#' theme_galactic
#'
#' A ggplot2 theme for Galactic Polymath styling. Sensible defaults for plots intended for presentations and worksheets. (Large text, thick grid lines, etc.)
#'
#' @param base.theme ggplot2 base theme to be modified; default="gray"; other options are  "bw","light","linedraw", "dark","minimal","classic" and "void" as listed \href{https://ggplot2.tidyverse.org/reference/ggtheme.html}{in this gallery}
#' @param grid.wt.maj How heavy do you want grid lines to be? (in case printer makes things lighter); default=.8
#' @param grid.wt.min How heavy do you want grid lines to be? (in case printer makes things lighter); default=.6
#' @param grid.col What color do you want the grid to be? Default: NA (maintain base theme); options are "gp_gray" or any custom color
#' @param bg.col Background color for plot area; default=NA (base theme default)
#' @param border.wt How heavy do you want the plot border to be?
#' @param border.col  Color of plot border. Default: same as font (#363636)
#' @param font Google font to use, "Montserrat" by default; see options with sysfonts::font_families_google() or the \href{https://fonts.google.com/}{Google font gallery}
#' @param regular.wt font weight for regular font style
#' @param bold.wt font weight for bold text
#' @param font.face style of axis label and title fonts; 1=plain, 2= bold, 3=italic, 4=bold+italic; Provide 1 value for all or 3 values for title, x-axis label, y-axis label (in that order); default= 1 (plain)
#' @param text.cex Controls size of text; if a single value, a simple multiplier for scaling all text; if 4 values, a multiplier for title, axis labels, axis values, and legend title; default=1 (don't rescale font sizes)
#' @param title.col color of title and subtitle (if present)
#' @param axis.lab.col color of axis labels
#' @param axis.text.col color of axis text (numbers, dates, etc)
#' @param axis.tick.length length of axis ticks (in pt units)
#' @param pad.title padding between title and plot; default=5 in "pt" units
#' @param pad.xlab padding between x-axis values and x label; default=5 in "pt" units
#' @param pad.ylab padding between x-axis values and x label; default=12 in "pt" units
#' @param pad.legend padding between legend title and key; default=0 in "pt" units
#' @param pad.outer set outer plot margins; default= c(20,10,5,5) for top, right, bottom, left in "pt" units
#' @examples
#' require(ggplot2)
#' #default plotting
#' (g<-ggplot(mtcars,aes(wt,mpg,col=as.factor(gear)))+geom_point())
#' #add ggplot themeing (intended to look good and be readable by
#' #data novices in printed & projected formats)
#' g+theme_galactic()
#'
#' #change the base theme
#' g+theme_galactic(base.theme="dark")
#'
#' #doesn't look great, let's change the palette to a color-blind-friendly
#' #Viridis theme (and the font while we're at it)
#' g+theme_galactic(font="Architects Daughter" )+
#' scale_colour_viridis_d(option="inferno")+geom_point(size=3)
#'
#' #Still not happy with the contrast...somewhere between the grays we've tried
#' (g2 <- g+theme_galactic(font="Architects Daughter",bg.col="gray70")+
#' scale_colour_viridis_d(option="inferno")+geom_point(size=3))
#'
#' #let's add a title and change the legend title
#' (g3 <- g2+
#' labs(title="What a good lookin' plot", col=expression(atop("Number","of gears")),parse=TRUE))
#'
#' #Make all the text bigger with one multiplier (useful for quickly scaling
#' #for a different output size)
#' g3+theme_galactic(text.cex=2,grid.col="gp_gray")+ggtitle("That's too big")
#' # Note we lost all our customizations because we overwrote our theme.
#'
#' #Add more space to the right side of the margin
#' g3+theme_galactic(text.cex=0.8,pad.outer=c(50,40,50,30))+ggtitle("Increased Outer Plot Margins")
#'
#' # Change the size of each type of text
#' g3+theme_galactic(text.cex=c(0.6,1.1,0.5,1.2))+labs(title="60% Title Text Size",
#' y="Axes are 110% Text Size", x="↑ Axis Values 50%↑ ",col="120% Legend\nText Size")
#'
#' # Change padding between text labels and graph elements
#' g3+theme_galactic(text.cex=c(.8,1,1,1),pad.title=30,
#' pad.xlab=0,pad.ylab=35,pad.legend=0)+ggtitle("Custom padding for elements")
#' @export

theme_galactic<-function(base.theme = "gray",
                         grid.wt.maj = .7,
                         grid.wt.min = .4,
                         grid.col = NA,
                         bg.col = NA,
                         border.wt = 1,
                         border.col = "#6D6D6D",
                         font = "Montserrat",
                         regular.wt = 400,
                         bold.wt = 700,
                         text.cex = 1,
                         font.face = 1,
                         title.col = "#363636",
                         axis.lab.col = "#363636",
                         axis.text.col = "#6D6D6D",
                         axis.tick.length = 6,
                         pad.title = 5,
                         pad.xlab = 5,
                         pad.ylab = 12,
                         pad.legend = 0,
                         pad.outer = c(20, 10, 5, 5)
){

  if(!is.na(grid.col)&grid.col=="gp_gray"){grid.col= "#C3C3C3"}
  showtext::showtext_auto()
    #Only try to download font if online and not already available
  if(is.na(match(font,sysfonts::font_families()))){
    isOnline=RCurl::url.exists("https://www.google.com")
    if(isOnline){
        tryCatch({
        sysfonts::font_add_google(name=font,family=font,regular.wt=regular.wt,bold.wt=bold.wt)},
        error=function(e) cat("\nFont: '",font,"' unavailable."))
    }else{
        cat("\nYou don't seem to be online. Can't download your requested font.")
    }
  }

  n_tcex_pars<-length(text.cex)
  if(n_tcex_pars!=1&n_tcex_pars!=4){stop("text.cex must be of length 1 or 4")}


# Main ggplot layer -------------------------------------------------------

#base theme to modify
eval(parse(text=paste0("ggplot2::theme_",base.theme,"()")))+
    #set background color if requested
    {if(is.na(bg.col)){}else{ggplot2::theme(panel.background=ggplot2::element_rect(fill=bg.col))}}+

  #Add other theme mods
  ggplot2::theme(
    text=ggplot2::element_text(family=font),
    #panel.border=ggplot2::element_rect(size=border.wt,colour=border.col),
    plot.margin=eval(parse(text=paste("ggplot2::margin(",paste(pad.outer,collapse=","),")",collapse="",sep=""))),
    plot.title=ggplot2::element_text(family=font,size=30*ifelse(n_tcex_pars==1,text.cex,text.cex[1]),
                                     face=if(length(font.face)==1){font.face}else{font.face[1]},
                                     color=title.col, margin = ggplot2::margin(t = 0, r = 0, b = pad.title, l = 0)),
    plot.subtitle=ggplot2::element_text(family=font,size=22*ifelse(n_tcex_pars==1,text.cex,text.cex[1]),color=title.col),
    axis.title=ggplot2::element_text(family=font,size=22*ifelse(n_tcex_pars==1,text.cex,text.cex[2]),color=axis.lab.col),
    axis.text=ggplot2::element_text(family=font,size=18*ifelse(n_tcex_pars==1,text.cex,text.cex[3]),color=axis.text.col),
    plot.caption=ggplot2::element_text(family=font,size=ifelse(n_tcex_pars==1,14*text.cex,14),color=axis.text.col),
    axis.ticks=ggplot2::element_line(color=grid.col,size=grid.wt.maj),
    axis.ticks.length=ggplot2::unit(axis.tick.length,"pt"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = pad.xlab, r = 0, b = 0, l = 0),
                                         face=if(length(font.face)==1){font.face}else{font.face[2]}),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = pad.ylab, b = 0, l = 0),
                                         face=if(length(font.face)==1){font.face}else{font.face[3]}),
    #only change grid.col if requested (due to incompatibility across themes)
    panel.grid.major=ggplot2::element_line(size=grid.wt.maj,if(!is.na(grid.col)){colour = grid.col}else{}),
    panel.grid.minor=ggplot2::element_line(size=grid.wt.min,if(!is.na(grid.col)){colour = grid.col}else{}),
    legend.text=ggplot2::element_text(family=font,color=axis.text.col,size=18*ifelse(n_tcex_pars==1,text.cex,text.cex[4])),
    legend.title=ggplot2::element_text(family=font,color=axis.lab.col,face="bold",size=18*ifelse(n_tcex_pars==1,text.cex,text.cex[4]),
                                       margin = ggplot2::margin(t = 0, r = 0, b = pad.legend, l = 0)),
    legend.position = "right", legend.text.align = 0, legend.background =ggplot2::element_blank() )


}

gpLogo<-function(ggObj,xNPC=.9,yNPC=.9,which="horiz_logoWords_GradWhite",size=.1,cloudinaryString=NULL){
  logoFile=switch(which,
    grad_logo_gradTrans="https://res.cloudinary.com/galactic-polymath/image/upload/v1593304396/logos/GP_logo_grad_transBG_300_tbn4ei.png",
    grad_logo_gradWhite="https://res.cloudinary.com/galactic-polymath/image/upload/b_white/v1593304396/logos/GP_logo_grad_transBG_300_tbn4ei.png",
    horiz_logoWords_gradTrans="https://res.cloudinary.com/galactic-polymath/image/upload/v1593304395/logos/GP_logo_wordmark_horiz_grad_transBG_300_lqdj7q.png",
    horiz_logoWords_gradWhite="https://res.cloudinary.com/galactic-polymath/image/upload/b_white/v1593304395/logos/GP_logo_wordmark_horiz_grad_transBG_300_lqdj7q.png",
    horiz_logoWords_whiteAblue="https://res.cloudinary.com/galactic-polymath/image/upload/v1593316226/logos/GP_logo_wordmark_horiz_white_aBlueBG_300_qmuas0.png",
    horiz_logoWords_whiteBlack="https://res.cloudinary.com/galactic-polymath/image/upload/v1594949366/logos/GP_logo_wordmark_horiz_white_blackBG_600_buwnlf.png",
    "Error"
  )

  if(logoFile=="Error"){stop("That's not one of the logo file options")}

  #Handle additional cloudinary parameters
  if(!is.null(cloudinaryString)){
    #test if already Cloudinary string in URL
    noCloudString=stringr::str_detect(logoFile,"upload\\/v")

    if(noCloudString){
    #Add strings
    splitStr<-stringr::str_split(logoFile,"upload\\/v",simplify=T)
    newURL<-paste0(splitStr[1],"upload/",cloudinaryString,"/v",splitStr[2])
    }else{
    #Add to existing strings
    extractStr0<-stringr::str_extract(logoFile,"upload\\/.*\\/v")
    extractStr<-gsub("/v","",extractStr0)
    splitStr<-stringr::str_split(logoFile,"upload\\/.*\\/v",simplify=T)
    newURL<-paste0(splitStr[1],extractStr,",",cloudinaryString,"/v",splitStr[2])
    }
  }else{newURL<-logoFile}

  #read in logo
  "https://res.cloudinary.com/galactic-polymath/image/upload/v1593317568/GP_logo_wordmark_horiz_white_blackBG_600_fjj1ii.png"
  logoImg<-png::readPNG(RCurl::getURLContent(newURL))


   ggObj+ggplot2::annotation_custom(grid::rasterGrob(logoImg,x=ggplot2::unit(xNPC,"npc"),y=ggplot2::unit(yNPC,"npc"),height=ggplot2::unit(size,"npc")))+if(xNPC>1|yNPC>1|xNPC<0|yNPC<0){ggplot2::coord_cartesian(clip = "off")}else{}

}



