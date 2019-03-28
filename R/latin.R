#' Latin Squares
#'
#' In combinatorics and in experimental design, a Latin square is an n × n array filled with n different symbols, each occurring exactly once in each row and exactly once in each column.
#'
#' @param a,b,c,d,i,j i,j is natural number
#'
#' @return anova
#'
#' @examples
#' treat <- c(rep("treatA",5), rep("treatB",5), rep("treatC",5), rep("treatD",5), rep("treatE",5))
#' fertil <- c(rep("fertil1",1), rep("fertil2",1), rep("fertil3",1), rep("fertil4",1), rep("fertil5",1))
#' seed <- c("C1","C2","C3","C4","C5","C2","C3","C4","C5","C1","C3","C4","C5","C1","C2","C4","C5","C1","C2","C3", "C5","C1","C2","C3","C4")
#' freq <- c(68,64,71,71,72,74,70,80,74,80,63,65,70,69,68,64,58,69,66,65,70,72,76,70,78)
#' latin(treat,fertil,seed,freq,5,5)
#'
#' @export
#'
#' @importFrom
#' stats anova
#' stats lm

latin<-function(a,b,c,d,i,j){
  mydata <- data.frame(a,b,c,d)
  print(matrix(mydata$c, i,j))
  print(matrix(mydata$d, i,j))
  anova(lm(d~ a+b+c, mydata))
}
