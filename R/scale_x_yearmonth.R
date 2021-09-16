#' scale_x_yearmonth
#'
#' Makes y-axis have year as a bottom facet, with abbreviated months as minor axis-labels.
#'
#' Code inspired by \href{http://www.jasonfabris.com/blog/dteformat/}{This post by Jason Fabris}
#'
#' @param abbrev How long to make months? Default=1 means just first letter of month; NA is full month name.
#' @param breaks.mnth Frequency of month labels; default= 1 (every month)
#' @param mnth.size Font size of month labels; default=12 (pts)
#' @param yr.size Font size of year labels; default=16 (pts)
#' @param yr.var Variable name for year; default= "Year"
#' @examples
#' require(ggplot2)
#' #Set up time series dataset
#' df<-data.frame(date=as.Date(c(sapply(1949:1952,function(yr) {paste(yr,1:12,1,sep="-")}))),
#' passengers=as.vector(AirPassengers)[1:48])
#' df$Year<-sprintf("%.4s",df$date)
#' (g <- ggplot(df,aes(date,passengers))+geom_point())
#' #Now break show months and year on x-axis
#' (g2 <- g+scale_x_yearmonth())
#' #Now let's use GP styling for legibility from a distance. (Note themes have to come before the scaling layer).
#' (g3 <- g+theme_galactic()+scale_x_yearmonth())
#' #This is a bit packed; let's only show every 3 months
#' g3+scale_x_yearmonth(abbrev=3,breaks.mnth=3)
#' #Note that if you want to show a trend line, you can't currently do this across years, because we're really faceting for each year to trick ggplot into making 2 types of axis label
#' g3+scale_x_yearmonth(abbrev=3,breaks.mnth=4)+geom_smooth()
#' @export

scale_x_yearmonth=function(abbrev=1,breaks.mnth=1,expand.x=1.5,mnth.size=12,yr.var="Year",yr.size=16){
  dte_formatter <- function(x) {
    #formatter for axis labels:
    mth <- substr(format(x, "%b"),1,abbrev)
    mth
  }
list(ggplot2::scale_x_date(date_breaks=paste0(breaks.mnth," months"),labels=dte_formatter,expand=expansion(mult=.05,add=c(expand.x*30,0))),
    ggplot2::theme(strip.background=ggplot2::element_blank(),strip.placement="outside",panel.spacing.x=unit(0,"lines"),
                   panel.border=ggplot2::element_rect(colour="#363636",fill="transparent",size=.35),
                   strip.text=ggplot2::element_text(colour = "#363636",size=yr.size),
                   axis.text.x=ggplot2::element_text(size=mnth.size)),
    facet_wrap(eval(parse(text=paste0("~",yr.var,collapse=""))), nrow = 1, scales = "free_x", shrink = FALSE, strip.position = "bottom"),
    {}
     )

}

