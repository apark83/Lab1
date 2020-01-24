#' Sample Mean Function
#'
#' Takes a matrix and returns a vector of the column means
#' @param X an input matrix of size (n x p)
#'
#' @return a vector of means
#' @export
#'
#' @examples
#' X = matrix(c(15,40,37,31,19,47,45,74,16), nrow = 3, ncol = 3, byrow = TRUE): SampleMean(X)
SampleMean = function(X) {
  X = matrix(0:8,nrow = 3, ncol = 3, byrow = TRUE)       # default matrix "x" for function

  # print(x)

  n = nrow(X)      # define the number of rows in matrix x

  # Sums the columnd and rows
  col.sum = apply(X, 2, sum)
  # row.sum = apply(x, 1, sum)

  # Adds sums to original matrix (not necessary)
  # x <- cbind(x, Rsum = row.sum)
  # x <- rbind(x, Csum = c(col.sum, sum(col.sum)))
  # print(x)

  # Calculates the sample mean for each column
  xbar = 1/n * col.sum
  print(xbar)
}

