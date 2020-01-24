#' Sample Covariance Function
#'
#' Takes a matrix and returns a covariance matrix
#' @param X an input matrix of size (n x p)
#'
#' @return a matrix of covariance
#' @export
#'
#' @examples
#' X = matrix(c(15,40,37,31,19,47,45,74,16), nrow = 3, ncol = 3, byrow = TRUE): SampleCovariance(X)
SampleCovariance = function(X) {
  X = matrix(c(15,40,37,31,19,47,45,74,16), nrow = 3, ncol = 3, byrow = TRUE)      # create default matrix for function

    n=nrow(X)
    p=ncol(X)

    sums=apply(X, MARGIN = 2, FUN=sum) #sum of measurements for each variable
    xbar=(1/n)*sums

    s <- matrix(NA, nrow = p, ncol = p)  # create matrix with same dimensions as input matrix
    v <- vector(mode = "numeric", length = n)

    for (i in 1:p) {
      for (k in 1:p) {
        for (j in 1:n) {
          v[j] <- (X[j,i] - xbar[i]) * (X[j,k] - xbar[k])
        }

        s[i,k] <- 1/(n-1) * sum(v)
      }


    }
    print(s)
}
