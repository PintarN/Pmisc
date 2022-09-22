#' Compute eigenvectors of matrix
#' 
#' Function is just here to test how the creation of functions work
#' @param mat A quadratic test matrix 
#'
#' @return a vector of eigenvectors
#' @export
#'
#' @examples mat <- matrix(sample(0:100,25,replace=T), ncol = 5)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4","I5")
#' eigenvalues <- test_eigenvector(mat)



test_eigenvector <- function (mat) 
{
  spectral <- eigen(mat)
  mat <- spectral$vectors[,2]
  mat <- 
  return(mat)
}





