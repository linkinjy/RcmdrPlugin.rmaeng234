#' Fully Nested
#'
#' If all arguments are stake
#'
#' @param pplot,splot,aplot,rplot,y abcd
#'
#' @return analysis of variance
#'
#' @examples
#' A<-rep(c(1,1,2,2,3,3,4,4),4)
#' B<-c(rep(1,16),rep(2,16))
#' C<-c(rep(1,8),rep(2,8),rep(1,8),rep(2,8))
#' R<-c(rep(c(1,2),16))
#' con<-c(55.30,55.33,55.89,55.82,55.35,55.39,55.30,55.38,55.53,55.55,56.14,56.12,55.59,55.53,55.44,55.45,55.04,55.05,55.56,55.54,55.10,55.06,55.03,54.94,55.22,55.20,55.76,55.84,55.29,55.34,55.12,55.15)
#' sp.plot2(A,B,C,R,con)
#'
#' @export
#'
#' @importFrom
#' stats anova
#' stats lm

# fully nested
sp.plot2<-function(pplot,splot,aplot,rplot,y){
  mydata <- data.frame(pplot,splot,aplot,y)
  name.y <- paste(deparse(substitute(y)))
  name.p <- paste(deparse(substitute(pplot)))
  name.sp <- paste(deparse(substitute(splot)))
  name.ap <- paste(deparse(substitute(aplot)))
  name.rp <- paste(deparse(substitute(rplot)))

  pplot<-as.factor(pplot)
  splot<-as.factor(splot)
  aplot<-as.factor(aplot)
  rplot<-as.factor(rplot)
  cat("\nClass level information\n\n")
  nrep<- length(unique(y))
  np  <- length(unique(pplot))
  nsp <- length(unique(splot))
  nap <- length(unique(aplot))
  nrp <- length(unique(rplot))
  cat(name.p,  "\t: ",unique(as.character(pplot)),"\n")
  cat(name.sp, "\t: ",unique(as.character(splot)),"\n")
  cat(name.ap, "\t: ",unique(as.character(aplot)),"\n")
  cat(name.rp, "\t: ",unique(as.character(rplot)),"\n")
  cat("\nNumber of observations: ", length(y), "\n\n")
  mo<-anova(lm(y~pplot/splot/aplot ))
  N<-NULL
  N[1]<- name.p
  N[2]<- paste(name.sp,"(",name.p,")",sep="")
  N[3]<- paste(name.ap,"(",name.p,name.sp,")",sep="")
  N[4]<- "E"
  rownames(mo)<-N
  mo
}
