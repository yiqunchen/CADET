# ----- general purpose helper functions -----

#' Takes the l2-norm of a vector.
#'
#' @keywords internal
#'
#' @param x the vector to be normed
#'
#' @return Returns the l2-norm of x.
norm_vec <- function(x) {
  sqrt(sum(x^2))
}

#' Checks if input is an integer between a and b
#'
#' @keywords internal
#'
#' @param x input to check
#' @param a lower
#' @param b upper
#'
#' @return Returns TRUE if input is an integer between a and b, FALSE otherwise
is_integer_between_a_b <- function(x, a, b) {
  (x>= min(c(a, b))) && (x %% 1 == 0) && (x <= max(c(a, b)))
}

naive.two.sided.pval <- function(z, mean, sd){
  first_side <- pnorm(abs(z), mean =  mean, sd=sd, lower.tail = F)
  second_side <- pnorm(-1*abs(z), mean =  mean, sd=sd,lower.tail = T)
  two_sided_p_val <- first_side+second_side
  return(two_sided_p_val)
}

