###################################################################
## MATLAB-like function
###################################################################

#' Return the inverse of a matrix
#'
#'
#' @param A a matrix
#' @return The inverse matrix of A, \eqn{A^{-1}}
#' @noRd
inv <- function(A) {
  return(solve(A))
}

#' Return the kronecker product between two matrices 
#' 
#' Kronecker product between two matrices A and B. If A is \eqn{n\times m} and
#' B is \eqn{p \times q}, their Kronecker product is the \eqn{mp \times nq} matrix
#' \deqn{
#' \mathbf{A} \otimes \mathbf{B}=\left[\begin{array}{ccc}
#' a_{11} \mathbf{B} & \cdots & a_{1 n} \mathbf{B} \\
#' \vdots & \ddots & \vdots \\
#' a_{m 1} \mathbf{B} & \cdots & a_{m n} \mathbf{B}
#' \end{array}\right]
#' }
#' 
#'
#' @param A a matrix
#' @param B another matrix
#' @return
#' Kronecker product of A and B 
#' @noRd
kron <- function(A, B) {
  return(Matrix::kronecker(A, B))
}

#' Return the identity matrix in sparse format
#'
#'
#' @param n input matrix
#' @return
#' Identity matrix in sparse format
#' @noRd
speye <- function(n) {
  return(Matrix::Diagonal(n, 1))
}

#' Return a matrix full of zeros
#'
#'
#' @param n desired width
#' @param m desired height
#' @return
#' A zero filled matrix of size n x m
#' @noRd
zeros <- function(n, m) {
  return(Matrix::Matrix(0, n, m))
}

#' Return a matrix full of ones
#'
#'
#' @param n desired width
#' @param m desired height
#' @return
#' A matrix of size n x m full of ones
#' @noRd
ones <- function(n, m) {
  return(Matrix::Matrix(1, n, m))
}

#' Return a canonical matrix
#'
#' A canonical matrix is a square matrix with entries, except for the 
#'   entry \eqn{A_{ij} = 1}.
#' @param n matrix size
#' @param i row index
#' @param j column index
#' @return
#' A matrix of size n with only the component in i,j set to one, while all 
#'   others are zeros.
#' @noRd
spcan <- function(n, i, j) {
  A <- zeros(n, n)
  A[i, j] <- 1
  return(A)
}


#' Return a matrix full of random numbers
#'
#'
#' @param n desired width
#' @param m desired height
#' @return
#' A matrix of size n x m full of random numbers uniformly distributed 
#'   in \eqn{[0, 1]}.
#' @noRd
rand <- function(n, m) {
  return(Matrix::Matrix(stats::runif(n * m), n, m))
}

#' Return a list of matrices in block diagonal form
#'
#'  This function is a wrapper for \link[Matrix]{bdiag}
#'
#' @param ... individual matrices or a list of matrices
#' @seealso \link[Matrix]{bdiag}
#' @export
blkdiag <- function(...) {
  return(Matrix::bdiag(...))
}


#' Return a matrix reshaped according to dimension n and m
#'
#'
#' @param A input matrix
#' @param n desired width
#' @param m desired height
#' @return
#' Input matrix A with shape n x m
#' @export
reshapeR <- function(A, n, m) {
  # cannot call this function just "reshape", because it's an R builtin command
  dim(A) <- c(n, m)
  return(A)
}


#' Return a matrix with elements on a diagonal specified by offset
#'
#'
#' @param x fill diagonal with this value
#' @param n matrix size
#' @param offset start position of diagonal, default: 0 = main diagonal
#' @return A diagonal matrix with \code{x} on its diagonal 
#'   (main or offset diagonal). 
#' @export
diagR <- function(x, n, offset = 0) {
  # cannot call this function just "diag", because it's an R builtin command
  # (to-do) add check on the length of x for offset != 0
  x <- as.numeric(x)
  M <- zeros(n, n)
  M[which(row(M) + offset == col(M))] <- x
  return(M)
}

#' Return the trace of input matrix
#'
#'
#' @param A input matrix
#' @return
#' Trace of input matrix
#' @export
traceR <- function(A) {
  return(sum(diag(A)))
}

#' Return the remainder of x divided by y
#'
#'
#' @param x numerator
#' @param y non-zero denominator
#' @return
#' modulo of x/y
#' @export
modR <- function(x, y) {
  return(unlist(lapply(x, function(z)
    z %% y)))
}

#' Return the sum across rows (n=1) or columns (n=2) of a matrix
#'
#'
#' @param A input matrix
#' @param n if n=1 sum rows, if n=2 sum columns
#' @return
#' sum of rows or columns
#' @export
sumR <- function(A, n) {
  if (n == 1) {
    return(rbind(Matrix::colSums(A)))
  } else if (n == 2) {
    return(cbind(Matrix::rowSums(A)))
  } else {
    stop("ERROR! Not a valid dimension.")
  }
}