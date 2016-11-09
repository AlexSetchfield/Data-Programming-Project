#' protein MW preditor
#'
#' This function allows you to calculate the molecular weight of protein 
#' @param ps        amino acid sequence in a string form
#' @keywords protein weight 
#' @export
#' @examples 
#' mw("RNBC")  #"Molecular Weight roughly = 0.58 kDa"
mw<-function(ps){
  a<-NULL
  b<-NULL
  c<-NULL
  f<-0
  i<-1
  g<-NULL
  a<-unlist(strsplit(ps,""))
  b<-length(a)
  for (i in i:b){
    d<-NULL
    e<-NULL
    d<-a[i]
    if (d=="A"){
      e<-89
    } else if (d=="R"){
      e<-174
    } else if (d=="N"){
      e<-132
    } else if (d=="D"){
      e<-133
    } else if (d=="B"){
      e<-133
    } else if (d=="C"){
      e<-121
    } else if (d=="Q"){
      e<-146
    } else if (d=="E"){
      e<-147
    } else if (d=="Z"){
      e<-147
    } else if (d=="G"){
      e<-75
    } else if (d=="H"){
      e<-155
    } else if (d=="I"){
      e<-131
    } else if (d=="L"){
      e<-131
    } else if (d=="K"){
      e<-146
    } else if (d=="M"){
      e<-149
    } else if (d=="F"){
      e<-165
    } else if (d=="P"){
      e<-115
    } else if (d=="S"){
      e<-105
    } else if (d=="T"){
      e<-119
    } else if (d=="W"){
      e<-204
    } else if (d=="Y"){
      e<-204
    } else if (d=="V"){
      e<-117
    } else {
      e<-0
    }
    f<-f+e
  }
  g<-round((f+18.2)/1000,2)
  print(paste("Molecular Weight roughly =",g,"kDa"))
}
mw("RNBC")
