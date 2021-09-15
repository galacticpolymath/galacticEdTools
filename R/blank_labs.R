#' blank_labs
#'
#' A function to replace axis labels and/or graph title with light gray prompts for each field.
#'
#' @param x_txt X-Axis Label prompt text (watermarked on blank); default= "ADD X-AXIS LABEL"; "" will keep spacing, with no text prompt; NA will pass-through label and not overwrite
#' @param y_txt Y-Axis Label prompt text (watermarked on blank); default= "ADD Y-AXIS LABEL"; "" will keep spacing, with no text prompt; NA will pass-through label and not overwrite
#' @param title_txt Graph Title prompt text (watermarked on blank); default= "ADD FIGURE TITLE"; "" will keep spacing, with no text prompt; NA will pass-through label and not overwrite
#' @param txt_col Color of prompt text; default= "gray92" (a very faint gray--you can use gray1 to gray99 to have the equivalent of changing transparency)
#' @param font.face style of axis label and title fonts; 1=plain, 2= bold, 3=italic, 4=bold+italic; Provide 1 value for all or 3 values for title, x-axis label, y-axis label (in that order); default= 4
#' @examples
#' require(ggplot2)
#' ggplot(mtcars,aes(x=mpg,y=hp))+geom_point()+theme_galactic()
#'
#' ggplot(mtcars,aes(x=mpg,y=hp))+geom_point()+theme_galactic()+blank_labs()
#' @export

blank_labs <- function(x_txt="ADD X-AXIS LABEL",
                        y_txt="ADD Y-AXIS LABEL",
                        title_txt="ADD FIGURE TITLE",
                        txt_col="gray92",
                        font.face=4
                        )
{
    lab_args <- unlist(sapply(c("x","y","title"),function(i) if(!is.na(eval(parse(text=paste0(i,"_txt"))))){paste0(i,"=",i,"_txt")}) )
    lab_args2 <- paste("labs(", paste0(lab_args,collapse=","), ")")


    strng<-"ggplot2::element_text(colour=txt_col)"
    theme_args<-unlist(sapply(c("x","y","title"),function(i) if(!is.na(eval(parse(text=paste0(i,"_txt"))))){
                paste0(switch(i,x="axis.title.x",
                                y="axis.title.y",
                                title="plot.title"),"=",strng)}) )
    theme_args2<-paste0("ggplot2::theme(",paste0(theme_args,collapse=","),")")

    font_theme_args<-paste0("ggplot2::theme(",
                            "plot.title=ggplot2::element_text(face=",if(length(font.face)==1){font.face}else{font.face[1]},"),",
                            "axis.title.x=ggplot2::element_text(face=",if(length(font.face)==1){font.face}else{font.face[2]},"),",
                            "axis.title.y=ggplot2::element_text(face=",if(length(font.face)==1){font.face}else{font.face[3]},"))")

    lapply(c(lab_args2,theme_args2,font_theme_args),function(x) eval(parse(text=x)))
}






