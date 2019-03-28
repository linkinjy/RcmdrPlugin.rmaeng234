#' can not split
#'
#' the factor can not split
#'
#' @param pplot,splot,y abcd
#'
#' @return anova
#'
#' @examples
#' A<-rep(c(1,1,2,2,3,3),3)
#' B<-c(rep(1,6),rep(2,6),rep(3,6))
#' y<-c(61.0,60.2,64.1,63.2,65.2,66.1,63.3,62.7,66.2,65.4,66.6,67.2,61.3,61.9,63.2,64.2,66.0,66.4)
#' sp.plot3(A,B,y)
#'
#' @export
#'
#' @importFrom
#' stats anova
#' stats lm
#'
sp.plot3<-function(pplot,splot,y){
  name.y <- paste(deparse(substitute(y)))
  name.p <- paste(deparse(substitute(pplot)))
  name.sp <- paste(deparse(substitute(splot)))

  pplot<-as.factor(pplot)
  splot<-as.factor(splot)
  cat("\nClass level information\n\n")
  nrep<- length(unique(y))
  np  <- length(unique(pplot))
  nsp <- length(unique(splot))
  cat(name.p,  "\t: ",unique(as.character(pplot)),"\n")
  cat(name.sp, "\t: ",unique(as.character(splot)),"\n")
  cat("\nNumber of observations: ", length(y), "\n\n")
  v<-anova(lm(y~pplot*splot))
  v[1,4]<-v[1,3]/v[3,3]
  v[2,4]<-v[2,3]/v[3,3]
  v[3,4]<-v[3,3]/v[4,3]
  N<-NULL
  N[1]<- name.p
  N[2]<- name.sp
  N[3]<- "E1"
  N[4]<- "E2"
  rownames(v)<-N
  v
}
