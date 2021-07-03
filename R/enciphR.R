#' enciphR
#'
#' encipher any text string using an alphabetic shift
#'
#' @param x string to be enciphered
#' @param alg algorithm for cipher (e.g. 1 to shift all letter forward 1 `[A->B]`; -2 to shift all letters back 2 places `[A->Y]`)
#' @param key do you want a key to be included in output?; default=F; If key=T, output will be a list, rather than a vector.
#' @export

enciphR<-function(x,alg,key=F){
  X.vec<-unlist(strsplit(x,fixed=T,split=""))
  x.vec<-tolower(X.vec)#all lower case as vector

     #define new alphabet
      alphabet<-1:26+alg
      alphabet.shifted.idx<-sapply(alphabet,function(x) {if(x>26){x-26}else{ if(x<1){x+26}else{x}}})
      alphabet.shifted<-letters[alphabet.shifted.idx]

      keyMat=cbind(IN=letters,OUT=alphabet.shifted)

    #encipher
      x1.1<-as.vector(sapply(1:length(x.vec),function(s) {
         if(!x.vec[s]%in%letters){x.vec[s]}else{#If nonletter, leave it alone, else...
         l<-keyMat[match(x.vec[s],keyMat[,"IN"]),"OUT"]
         if(X.vec[s]%in%LETTERS){l<-toupper(l)}
         l
         }},USE.NAMES = F))
     x2<-paste0(x1.1,collapse="")
  if(key){
    out<-list(IN=x,OUT=x2,KEY=keyMat)}
      else{
      out<-x2
      }
      return(out)
}





