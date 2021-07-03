#' ggciphR
#'
#' encipher the axis labels of a ggplot2 object to turn a graph into a data literacy puzzle
#'
#' @param ggGraph a ggplot2 object
#' @param alg algorithm for cipher (e.g. 1 to shift all letter forward 1 `[A->B]`; -2 to shift all letters back 2 places `[A->Y]`)
#' @export

ggciphR<-function(ggGraph,alg){
  g<-ggGraph
  pass2enciphR=function(x){enciphR(x,alg=alg)}
  #process all labels
  g$labels<-lapply(g$labels,pass2enciphR)

  #process custom legend if present (sometimes works, depending on ggplot object)
  try(if(length(g$scales$scales)>0){
    for(i in 1:length(g$scales$scales)){
      g$scales$scales[[i]]$name=enciphR(g$scales$scales[[i]],alg=alg)
    }
  })
  g
}

