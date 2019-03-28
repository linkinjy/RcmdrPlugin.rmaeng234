#' qualitative data (one way)
#'
#' development of concepts which help us to understand social phenomena in natural settings, giving due emphasis to the meanings, experiences and views of the participants
#'
#' @param A,B,Y,r,i,j abcd
#'
#' @return anova
#'
#' @examples
#' A<-c(1,1,2,2,3,3,4,4)
#' B<-rep(c("plus","minus"),4)
#' Y<-c(190.00,10.00,178.00,22.00,194.00,6.00,170.00,30.00)
#' k<-data.frame(A,B,Y)
#' k
#' qual(A,B,Y,200,2,4)
#'
#' @export
#'
#' @importFrom
#' stats anova
#' stats lm
#' stats pf

qual<-function(A,B,Y,r,i,j){
  v<-matrix(k$Y,i,j)
  print(v)

  name.y <- paste(deparse(substitute(Y)))
  name.p <- paste(deparse(substitute(A)))
  name.sp <- paste(deparse(substitute(B)))
  A<-as.factor(A)
  B<-as.factor(B)
  cat("\nClass level information\n\n")
  np  <- length(unique(A))
  nsp <- length(unique(B))
  cat(name.p,  "\t: ",unique(as.character(A)),"\n")
  cat(name.sp, "\t: ",unique(as.character(B)),"\n")
  cat("\nNumber of observations: ", length(Y), "\n\n")

  ct<-(sum(v[2,])^2/((length(unique(A))*(r))))
  st<-sum(v[2,])-ct
  sa<-sum(v[2,]^2)/r-ct
  se<-st-sa
  n<-lm(Y ~ A+B)
  m<-anova(n)

  e<-NULL
  e<-m[c(1,2,3),]
  e[1,2]<-round(sa,2)
  e[1,3]<-e[1,2]/e[1,1]
  e[2,1]<-r*length(unique(A))-e[1,1]-1
  e[2,2]<-se
  e[2,3]<-e[2,2]/e[2,1]
  e[1,4]<-e[1,3]/e[2,3]
  e[2,4]<-""
  e[2,5]<-""
  e[3,1]<-r*length(unique(A))-1
  e[3,2]<-st
  e[3,3]<-""
  f<-as.numeric(e[1,4])
  g<-as.numeric(e[1,1])
  h<-as.numeric(e[2,1])
  e[1,5]<-1-pf(f,g,h)

  n<-NULL
  n[1]<- name.p
  n[2]<- "E"
  n[3]<- "T"
  rownames(e)<-n

  print(e)
}
