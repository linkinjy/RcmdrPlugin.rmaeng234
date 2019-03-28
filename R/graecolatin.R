#' graeco latin squares
#'
#' In combinatorics, a Graeco-Latin square or Euler square or orthogonal Latin squares of order n over two sets S and T, each consisting of n symbols, is an n×n arrangement of cells, each cell containing an ordered pair (s,t), where s is in S and t is in T, such that every row and every column contains each element of S and each element of T exactly once, and that no two cells contain the same ordered pair.
#'
#' @param a,b,c,d,y,i,j i,j is natural number
#'
#' @return analysis of variance
#'
#' @examples
#' A <- c(rep("A1",4), rep("A2",4), rep("A3",4), rep("A4",4))
#' B <- c(rep("B1",1), rep("B2",1), rep("B3",1), rep("B4",1))
#' C <- c("C2","C4","C1","C3","C1","C3","C2","C4","C3","C1","C4","C2","C4","C2","C3","C1")
#' D <- c("D3","D1","D4","D2","D1","D3","D2","D4","D4","D2","D3","D1","D2","D4","D1","D3")
#' Y <- c(15,5,15,19,4,19,16,26,8,9,19,14,19,16,17,34)
#' greco.latin(A,B,C,D,Y,4,4)
#'
#' @export
#'
#' @importFrom
#' stats anova
#' stats lm

# graeco latin squares
greco.latin<-function(a,b,c,d,y,i,j){
  mydata <- data.frame(a,b,c,d,y)
  print(matrix(paste(mydata$c,mydata$d),i,j))
  print(matrix(mydata$y,i,j))
  anova(lm(y~ a+b+c+d, mydata))
}
