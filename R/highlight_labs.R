#' highlight_labs
#'
#' The counterpoint to [blank_labs()], this function makes selected labels bold, italic, and colorful.
#'
#' This is used to make teacher graphs where it's clear what students will fill in on the blanked graph.
#'
#' @param which_lab Which label to highlight? Accepts a vector of 1 or more numbers or strings:
#' - 1 = "title"
#' - 2 = "x-lab"
#' - 3 = "y-lab
#' - 4 = "x-vals"
#' - 5 = "y-vals"
#' - 6 = "legend title"
#' - 7 = "legend text"
#' - 8 = "all"
#' @param txt_col Color of label text; default= "#005792" (Dark GP Hydrogen Blue)
#' @param font_face style of highlighted labels and title fonts; 1=plain, 2= bold, 3=italic, 4=bold+italic; default= 4
#' @examples
#' require(ggplot2)
#' ggplot(mtcars,aes(x=mpg,y=hp))+geom_point()+
#' theme_galactic()+ggtitle("Normal graph")
#'
#' ggplot(mtcars,aes(x=mpg,y=hp))+geom_point()+
#' theme_galactic()+highlight_labs(c(2,3))
#' @family functions for enigmatizing graphs
#' @export

highlight_labs <- function( which_lab,
                        txt_col="#005792",
                        font_face=4,
                        ...
                        )
{
    if(missing(which_lab)){
      stop("Must supply 'which_lab'. See `?highlight_labs` for details.")
    }

    #Interpret fonts & colors supplied
    d0 <- dplyr::tibble(n=1:7,item=c("plot.title","axis.title.x","axis.title.y","axis.text.x","axis.text.y","legend.title", "legend.text"),which_lab=c("title","x-lab","y-lab","x-vals","y-vals","legend title","legend text"),font_face=font_face,col=txt_col)

suppressWarnings({
    if(which_lab==8|which_lab=="all"){
      d<-d0
    }else{
    #filter out for only supplied labels (allows numeric or character inputs)
      keep<-sapply(which_lab,function(x){
          if(suppressWarnings(!is.na(as.numeric(x)))){
            x
          }else{
            d0$n[which(d0$which_lab==x)]
          }
    }) %>% unlist() %>% as.numeric()

    d<-d0[keep,]
    }
})

    #COLOR & FONT
    # Define ggplot theme colour arguments for all of our non-NA parameters
    font_args0<-sapply(1:nrow(d),function(i){
                        paste0(d$item[i],"=",
                               "ggplot2::element_text(colour='",
                               d$col[i],
                               "', face=",
                               d$font_face[i],
                               ")")})
    # Put all arguments together in a theme() call
    font_args<-paste0("ggplot2::theme(",paste0(font_args0,collapse=","),")")


    #Make a list of all ggplot function calls
    eval(parse(text=font_args))
}






