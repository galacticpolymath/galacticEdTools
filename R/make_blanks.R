#' make_blanks
#'
#' A function to replace axis labels and/or graph title with blanks, along with a light gray prompt for each field.
#'
#' @param x_txt X-Axis Label prompt text (watermarked on blank); default= "ADD X-AXIS LABEL"
#' @param y_txt Y-Axis Label prompt text (watermarked on blank); default= "ADD Y-AXIS LABEL"
#' @param title_txt Graph Title prompt text (watermarked on blank); default= "ADD FIGURE TITLE"
#' @param txt_col Color of prompt text; default= "gray92" (a very faint gray--you can use gray1 to gray99 to have the equivalent of changing transparency)
#' @param blank_col Color of blank (line segment under prompt text); default= "gray70"
#' @param pad_top Padding between the top of the graph and the Title blank line segment as a percent
#' @param pad_left Padding between the left of the graph and the Y-Axis blank line segment as a percent
#' @param pad_bottom Padding between the bottom of the graph and the X-Axis blank line segment as a percent

make_blanks <- function(x_txt="ADD X-AXIS LABEL",
                        y_txt="ADD Y-AXIS LABEL",
                        title_txt="ADD FIGURE TITLE",
                        txt_col="gray92",
                        blank_col="gray70",
                        pad_top=0.02,
                        pad_left=0.1,
                        pad_bottom=0.2,...
                        ){

    graf<- invisible(ggplot2::last_plot())
    x_grob_lims<- ggplot2::ggplot_build(graf)$layout$panel_params[[1]]$x.range
    y_grob_lims<- ggplot2::ggplot_build(graf)$layout$panel_params[[1]]$y.range
    blanks_coords<-data.frame(lab_type=c("title","x","y"),
                          x_basis=c(x_grob_lims[1],mean(x_grob_lims),x_grob_lims[1]),
                          y_basis=c(y_grob_lims[2],y_grob_lims[1],mean(y_grob_lims)))
    x_scale=diff(x_grob_lims)/100
    y_scale=diff(y_grob_lims)/100
    pad=0.1
    # X coordinates
    blanks_coords$x=c(x_grob_lims[1],#title start line
                      x_grob_lims[1]+.1*x_scale,#x start blank line
                      x_grob_lims[1]-pad_left*x_scale) #y start blank line
    blanks_coords$xend=c(x_grob_lims[2]-.1*x_scale,#title end line
                      x_grob_lims[2]-.1*x_scale,#x end blank line
                      x_grob_lims[1]-pad_left*x_scale) #y end blank line
    # Y coordinates
    blanks_coords$y=c(y_grob_lims[2]+ pad_top*y_scale,#title start line
                      y_grob_lims[1]-pad_bottom*y_scale,#x start blank line
                      y_grob_lims[1]+.05*y_scale) #y start blank line
    blanks_coords$yend=c(y_grob_lims[2]+ pad_top*y_scale,#title end line
                      y_grob_lims[1]-pad_bottom*y_scale,#x end blank line
                      y_grob_lims[2]-pad_left*y_scale) #y end blank line
    blanks_coords$label=c("ADD TITLE","ADD X-AXIS LABEL","ADD Y-AXIS LABEL")

    #Modify original plot
    graf2<-graf+ ggplot2::coord_cartesian(clip="off")
    #If modify x-axis
    if(!is.na(x_txt)){
      graf3<-graf2+ggplot2::xlab(x_txt)+{
      ggplot2::geom_segment(x=blanks_coords$x[2],
                   xend=blanks_coords$xend[2],
                   y=blanks_coords$y[2],yend=blanks_coords$yend[2],inherit.aes=F,col=blank_col)}+
      ggplot2::theme(axis.title.x=element_text(colour=txt_col))
       }else{graf2<-graf3}
      #if modify y-axis
       if(!is.na(y_txt)){
        graf4<-graf3+ggplot2::ylab(y_txt)+
        ggplot2::geom_segment(x=blanks_coords$x[3],
                   xend=blanks_coords$xend[3],
                   y=blanks_coords$y[3],
                   yend=blanks_coords$yend[3],inherit.aes=F,col=blank_col)+
      ggplot2::theme(axis.title.y=element_text(colour=txt_col))
       }else{graf4 <- graf3}
      #if modify title
       if(!is.na(title_txt)){
        graf5 <- graf4+ggplot2::ggtitle(title_txt)+
        ggplot2::geom_segment(x=blanks_coords$x[1],xend=blanks_coords$xend[1],
                             y=blanks_coords$y[1],yend=blanks_coords$yend[1],
                             inherit.aes=F,col=blank_col)+
        ggplot2::theme(plot.title=element_text(colour=txt_col))
     }else{graf5 <- graf4}
    plot(graf5 )
    }


ggplot(mtcars,aes(x=mpg,y=hp))+geom_point()+theme_galactic()+make_blanks(x_txt="jdjs")
make_blanks(x_txt=NA)


