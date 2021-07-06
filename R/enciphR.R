#nonShiny ciphR code for enciphering a string
# x= string to be encoded
# alg= algorithm for cipher/code (could be +/1 N for alphabetic shift)
# default output is converted vector
# if key=T, output will be list w/ converted text & key as items

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

enciphR("Hello, my name is Simon3223",alg=+3,key=T)

#ggGraph is a ggplot2 object
#alg is an algorithm for enciphering strings for the enciphR function (e.g. +2)
ggCiphR<-function(ggGraph,alg){
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

