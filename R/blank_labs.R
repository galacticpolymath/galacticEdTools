#' blank_labs
#'
#' The counterpoint to [highlight_labs()]. A function to replace axis labels and/or graph title with light gray prompts for each field.
#'
#' This is used to make student graphs with parts missing to be filled in.
#'
#' @param x_txt X-Axis Label prompt text (watermarked on blank); default= "ADD X-AXIS LABEL"; "" will keep spacing, with no text prompt; NA will pass-through label and not overwrite.
#' @param y_txt Y-Axis Label prompt text (watermarked on blank); default= "ADD Y-AXIS LABEL"; "" will keep spacing, with no text prompt; NA will pass-through label and not overwrite
#' @param xval_txt For geom_boxplot or similar, replace factor labels (x-values) with this text; default=NA; Suggested text="Add LABEL". Needs to be repeated for as many labels as the original graph has
#' @param yval_txt Replace y-axis values with custom text; default=NA; Suggested text="Add LABEL". Needs to be repeated for as many labels as the original graph has
#' @param title_txt Graph Title prompt text (watermarked on blank); default= "ADD FIGURE TITLE"; "" will keep spacing, with no text prompt; NA will pass-through label and not overwrite
#' @param txt_col Color of prompt text; default= "gray92" (a very faint gray--you can use gray1 to gray99 to have the equivalent of changing transparency)
#' @param font_face style of axis label and title fonts; 1=plain, 2= bold, 3=italic, 4=bold+italic; Provide 1 value for all or 3 values for title, x- and y-axis labels, and x- and y- values (in that order); default= 4
#' @examples
#' require(ggplot2)
#' (g0 <- ggplot(mtcars,aes(x=mpg,y=hp))+geom_point()+
#' theme_galactic()+ggtitle("Normal graph") )
#'
#' # Make student version with x- and y- axis labels blanked, but not title
#' g0+blank_labs(title_txt=NA)+
#' labs(title="Blanked Student Graph",subtitle="(Add the missing axis labels)")
#'
#' # Note this is will not look great until you save the file
#' ggsave("blank graph.png",width=4,height=3)
#'
#' # Now make a teacher version of the graph with correct labels highlighted blue
#' g0+highlight_labs(c(2,3))
#'
#'
#'
#' @family functions for enigmatizing graphs
#' @export

blank_labs <- function( x_txt="ADD X-AXIS LABEL",
                        y_txt="ADD Y-AXIS LABEL",
                        xval_txt=NA,
                        yval_txt=NA,
                        title_txt="ADD FIGURE TITLE",
                        txt_col="gray92",
                        font_face=4,
                        ...
                        )
{

    #Interpret fonts & colors supplied
    font_face<-if(length(font_face==1)){rep(font_face,5)
      }else{
      c(font_face[1],font_face[2],font_face[2],font_face[3],font_face[3])
    }
    d0 <- dplyr::tibble(item=c("plot.title","axis.title.x","axis.title.y","axis.text.x","axis.text.y"),var=c("title_txt","x_txt","y_txt","xval_txt","yval_txt"),font_face=font_face,col=txt_col,lab_arg=c("title","x","y",NA,NA))

    #filter out rows that have NA variables (i.e. that we don't want to alter)
    #All items in d will be changed to custom "blank" text
    d<-d0[which(sapply(d0$var,function(x) !is.na(eval(parse(text=x))))),]

    #LABELS
    lab_args0 <-sapply(1:length(d$lab_arg), function(i) {
                  di<-d[i,]
                  if (!is.na(di$lab_arg)) {
                    paste0(di$lab_arg,"='",eval(parse(text=di$var)),"'")
                    }
                  }) %>% unlist()
      #construct ggplot command
    lab_args <- paste("ggplot2::labs(", paste0(lab_args0,collapse=","), ")")

    # strng<-"ggplot2::element_text(colour=txt_col)"

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

    #Add manual axis value labels if supplied
    # lbl<-function(n){rep(xval_txt,n)}
    x_vals<-if(!is.na(xval_txt)){
      paste0("ggplot2::scale_x_discrete(labels=function(names.arg){rep(xval_txt,length(names.arg))})")
    }else{NULL}
    y_vals<-if(!is.na(yval_txt)){
      paste0("ggplot2::scale_y_continuous(labels=function(names.arg){rep(yval_txt,length(names.arg))})")
    }else{NULL}

    #Make a list of all ggplot function calls
    lapply(c(lab_args,font_args,x_vals,y_vals),function(x) eval(parse(text=x)))
}






