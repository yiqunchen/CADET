# ----- functions for explicitly computing truncation sets S -----

# ----- functions for solving quadratic inequalities -----
#' Solve the roots of quadratic polynomials related to testing for a difference in means
#'
#' Solves \eqn{ax^2 + bx + c \ge 0}, then returns the complement of the solution set
#' wrt to the real line, unless the complement is empty, in which case
#' the function returns NA.
#'
#' @keywords internal
#'
#' @param A, B, C the coefficients of the quadratic equation.
#' @param tol if \eqn{|a|}, \eqn{|b|}, or \eqn{|c|} is not larger than tol, then treat it as zero.
#'
#' @return Returns an "Intervals" object containing NA or the complement of the solution set.
solve_one_ineq <- function(A, B, C, tol=1e-10) {
  # Computes the complement of the set {phi >= 0: B*phi + C >= 0},
  # ignoring (-Inf, 0].
  compute_linear_ineq_complement <- function(B, C, tol=1e-10) {
    # Is B = 0?
    if(abs(B) <= tol) {
      if(C >= -tol) { # C >= 0: inequality automatically satisfied
        return()
      } else { # C < 0: something has gone wrong ...
        warning("B = 0 and C < 0: B*phi + C >=0 is degenerate")
        return(c(0, Inf))
      }
    }

    # We know that B =/= 0
    ratio <- -C/B
    # Is B > 0?
    if(B > tol) {
      if(C >= -tol) { # -C/B <= 0: inequality automatically satisfied
        return()
      } else { # -C/B > 0: the interval extends to the right
        return(c(0, ratio))
      }
    }

    # We know B < 0
    if(C <= tol) { # -C/B <= 0: inequality can't be satisfied
      return(c(0, Inf))
    }

    # We know B < 0 & -C/B > 0: the interval extends to the left
    return(c(ratio, Inf))
  }


  # A = 0?
  if(abs(A) <= tol) {
    return(compute_linear_ineq_complement(B, C, tol))
  }

  # We know A =/= 0
  discrim <- B^2 - 4*A*C

  # No roots or one root?
  if(discrim <= tol) {
    if(A > tol) { # Parabola opens up: inequality automatically satisfied
      return()
    } else { # Parabola opens down: inequality never satisfied
      return(c(0, Inf))
    }
  }

  # We now know that A =/= 0, and that there are two roots
  sqrt_discrim <- sqrt(discrim)
  roots <- sort(c(-B + sqrt_discrim, -B - sqrt_discrim)/(2*A))
  # Parabola opens up? (A > 0?)
  if(A > tol) {
    if(roots[1] > tol) {
      return(c(roots[1], roots[2]))
    }

    if(roots[2] <= tol) {
      return()
    }

    return(c(0, roots[2]))
  }

  # We now know that there are two roots, and parabola opens down (A < 0)
  if(roots[2] < -tol) {
    return(c(0, Inf))
  }

  if(roots[1] > tol) {
    return(c(0, roots[1], roots[2], Inf))
  }
<<<<<<< HEAD

  return(c(roots[2], Inf))
}


# ----- functions for solving quadratic inequalities -----
#' Solve the roots of quadratic polynomials related to testing for a difference in means
#'
#' Solves \eqn{ax^2 + bx + c \ge 0}, then returns the complement of the solution set
#' wrt to the real line, unless the complement is empty, in which case
#' the function returns NA.
#'
#' @keywords internal
#'
#' @param A, B, C the coefficients of the quadratic equation.
#' @param tol if \eqn{|a|}, \eqn{|b|}, or \eqn{|c|} is not larger than tol, then treat it as zero.
#'
#' @return Returns an "Intervals" object containing NA or the complement of the solution set.
solve_one_ineq_complement <- function(A, B, C, tol=1e-10) {
  # Computes the complement of the set {phi: B*phi + C <=  0},
  compute_linear_ineq_complement <- function(B, C, tol=1e-8) {
    #  If B = 0
    if(abs(B) <= tol) {
      if(C <= tol) { # C <= 0: inequality is always satisfied
        return(c(0,0)) # all of real line
      } else { # C > 0: something has gone wrong -- no solution works
        warning("B = 0 and C > 0: B*phi + C <= 0 has no solution")
        return(c(-Inf,Inf)) # do not return any value
      }
    }

    # If B \neq 0
    ratio <- -C/B
    # If B > 0:
    if(B > tol) {
      return(c(ratio,Inf))
    }
    if(B < tol) {
      return(c(-Inf, ratio))
    }
    # we will return a degenerate interval
    # return(c(-Inf,Inf))
  }


  # A = 0?
  if(abs(A) <= tol) {
    return(compute_linear_ineq_complement(B, C, tol))
  }

  # We know A \neq 0
  discrim <- B^2 - 4*A*C

  # If discriminant is small, we assume there is no root
  if(discrim <= tol) {
    if(A > tol) { # Parabola opens up: there is no solution
      return(c(-Inf,Inf))
    } else { # Parabola opens down: every x is a solution
      return(c(0, 0))
    }
  }

  # We now know that A =/= 0, and that there are two roots
  # we compute the roots using the suggestion outlined at
  # https://people.csail.mit.edu/bkph/articles/Quadratics.pdf
  # for numerical stability purposes
  sqrt_discrim <- sqrt(discrim)
  if (B >= tol){
    root_1 <- (-B-sqrt_discrim)/(2*A)
    root_2 <- (2*C)/(-B-sqrt_discrim)
    roots <- sort(c(root_1, root_2))
  }else{
    root_1 <- (-B+sqrt_discrim)/(2*A)
    root_2 <- (2*C)/(-B+sqrt_discrim)
    roots <- sort(c(root_1, root_2))
  }

  if(A > tol) {
    if(roots[1] > tol) {
      return(c(0, roots[1], roots[2], Inf))
    }

    if(roots[2] <= tol) {
      warning("something wrong with the discriminant calculation!")
      return(c(0,Inf))
    }

    return(c(roots[2], Inf))
  }

  # We now know that there are two roots, and parabola opens down (A < 0)
  if(roots[2] < -tol) {
    return(c(0, 0))
  }

  if(roots[1] > tol) {
    return(c(roots[1], roots[2]))
  }

  return(c(-Inf, roots[2]))

  # Parabola opens up? (A > 0?)
  # if(A > tol) {
  #
  #   if(roots[1] > tol){
  #     return()
  #   }
  #
  #   interval_result <- matrix(c(-Inf,roots[1], roots[2], Inf),
  #                             ncol=2, byrow = T)
  #
  #   return(interval_result)
  # }else{
  #   return(roots)
  # }

  # if everything fails -- then we do not have a solution
  #warning("Edge case for quadratic inequality solver!")
  #return(c(-Inf,Inf))
}
#' Represent <x\'(phi)_i, x\'(phi)_j> as a quadratic function in phi ----
  #' @keywords internal
  #'
  #' @param X, matrix n by p
  #' @param v, contrast vector n by 1
  #' @param i, first index
  #' @param j, second index
  #'
  #' @return parameters: a, b, c the coefficients of the quadratic equation such that (ax^2 + bx + c <= 0)
  #'
  inner_product_phi <- function(X, v, i, j){
    v_norm <- norm_vec(v)
    XTv <- t(X)%*%v
    XTv_norm <- norm_vec(XTv)
    dir_XTv <- XTv/norm_vec(XTv)
    quad_coef <- (v[i]*v[j])/(v_norm)^4
    linear_coef <- (v[j]/v_norm^2)*(X[i,]%*%dir_XTv) + (v[i]/v_norm^2)*(X[j,]%*%dir_XTv) -
      2*v[i]*v[j]*XTv_norm/(v_norm^4)
    constant_vec_1 <- X[i,]-v[i]*XTv_norm/v_norm^2*dir_XTv
    constant_vec_2 <- X[j,]-v[j]*XTv_norm/v_norm^2*dir_XTv
    constant_coef <- sum(constant_vec_1*constant_vec_2)
    coef_list <- list("quad" = as.numeric(quad_coef), "linear" = as.numeric(linear_coef),
                      "constant"= as.numeric(constant_coef))
    return(coef_list)
  }



#' Represent ||x'(phi)_i-x'(phi)_j||_2^2 as a quadratic function in phi ----
#' @keywords internal
#'
#' @param XTv, vector p by 1
#' @param XTv_norm, norm of XTv
#' @param dir_XTv, vector p by 1 := XTv/XTv_norm
#' @param v_norm, 2-norm of vector v
#' @param i, first index
#' @param j, second index
#' @param v, the vector
#'
#' @return parameters: a, b, c the coefficients of the quadratic equation
#' such that (ax^2 + bx + c <= 0)
#'
norm_sq_phi <- function(X, v, XTv, XTv_norm, dir_XTv, v_norm, i, j){
  #v_norm <- norm_vec(v)
  #XTv <- t(X)%*%v
  #XTv_norm <- norm_vec(XTv)
  #dir_XTv <- XTv/norm_vec(XTv)
  quad_coef <- (v[i]-v[j])^2/(v_norm)^4
  linear_coef <- 2*((((v[i]-v[j])/v_norm^2)*(X[i,]-X[j,])%*%dir_XTv) - ((v[i]-v[j])/v_norm^2)^2*XTv_norm)
  constant_vec <- X[i,]-X[j,]-(v[i]-v[j])*XTv/(v_norm^2)
  constant_coef <- sum(constant_vec*constant_vec)
  coef_list <- list("quad" = as.numeric(quad_coef), "linear" = as.numeric(linear_coef),
                    "constant"= as.numeric(constant_coef))
  return(coef_list)
}




#' Represent <x'(phi)_i, x'(phi)_j> as a quadratic function in phi ----
#' @keywords internal
#'
#' @param XTv, vector p by 1
#' @param XTv_norm, norm of XTv
#' @param dir_XTv, vector p by 1 := XTv/XTv_norm
#' @param v_norm, 2-norm of vector v
#' @param cl, factor vector n by 1 (most recent cluster assignment)
#' @param k, cluster of interest
#' @param v, contrast vector n by 1
#' @param i, index of observation
#'
#' @return parameters: a, b, c the coefficients of the quadratic equation such that (ax^2 + bx + c <= 0)
#'
norm_phi_canonical_kmeans <- function(X, last_centroids, XTv, XTv_norm, dir_XTv, v_norm, cl, k, v, i){
  n_k <- sum(cl==k)
  indicator_vec <- rep(0, times=length(cl))
  indicator_vec[cl==k] <- 1
  indicator_location <- which(cl==k)
  #v_norm <- norm_vec(v)
  #XTv <- t(X)%*%v
  #XTv_norm <- norm_vec(XTv)
  #dir_XTv <- XTv/norm_vec(XTv)
  # compute quad coef
  v_i_expression <- (v[i]-sum(indicator_vec*v)/n_k)/(v_norm^2)
  #X_current <- X[indicator_location,]
  #x_i_expression <- X[i,] - ((t(indicator_vec) %*% X)/n_k)
  x_i_expression <- X[i,] - last_centroids[k,]
  # n_k and class k
  # .colMeans(X[indicator_location,], n_k, dim(X)[2])
  quad_coef <- (v_i_expression)^2
  # compute lienar coef
  linear_coef_part_1 <- v_i_expression*(x_i_expression%*%dir_XTv)
  linear_coef_part_2 <- (v_i_expression)^2*XTv_norm
  linear_coef <- 2*(linear_coef_part_1-linear_coef_part_2)

  constant_vec <- x_i_expression - c(v_i_expression*XTv)
  constant_coef <- sum(constant_vec*constant_vec)

  coef_list <- list("quad" = as.numeric(quad_coef), "linear" = as.numeric(linear_coef),
                    "constant"= as.numeric(constant_coef))
  return(coef_list)
}

#' Implement the minus operation for two quadratic inequalities
#'
minus_quad_ineq <- function(quad1, quad2){
  coef_list <- list("quad" = quad1$quad-quad2$quad, "linear" = quad1$linear-quad2$linear,
                    "constant"= quad1$constant-quad2$constant)
  return(coef_list)
}


#' Compute set S for isotropic case
#' @export
kmeans_compute_S_iso <- function(X, estimated_k_means, all_T_clusters,
                                 all_T_centroids,
                                 n, XTv, XTv_norm,
                                 dir_XTv, v_vec,v_norm,T_length, k){

  final_interval <- intervals::Intervals(c(0,Inf))
  all_interval_lists <- list()

  # loop through the initialization
  init_list <- estimated_k_means$random_init_obs
  init_cluster <- all_T_clusters[1,]
  # look at covariance matrices -- a different S needed to be computed
  for (i in c(1:n)){
    current_j_prime <- init_cluster[i]
    j_star_quad <- norm_sq_phi(X, v_vec, XTv, XTv_norm, dir_XTv, v_norm,i,init_list[current_j_prime])
    for (j in c(1:length(init_list))){
      current_j_quad <- norm_sq_phi(X, v_vec, XTv, XTv_norm, dir_XTv, v_norm,i,init_list[j])
      curr_quad <- minus_quad_ineq(j_star_quad, current_j_quad)
      curr_interval <- solve_one_ineq_complement(curr_quad$quad, curr_quad$linear, curr_quad$constant)
      all_interval_lists[[(i-1)*length(init_list)+j]] <- curr_interval
      #final_interval <- intervals::interval_intersection(final_interval, curr_interval)

    }
  }

  curr_len <- length(all_interval_lists)
  curr_counter <- 1
  # loop through all sequence t
  if(T_length>1){
    for (l in c(1:(T_length-1))){
      current_cl <- all_T_clusters[(l+1),]
      last_cl <- all_T_clusters[(l),]
      last_centroids <- all_T_centroids[[(l+1)]] # k by q matrix
      # pre-compute the centroids (or maybe extract from kmeans estimation??)
      # loop through all the observations
      for (i in c(1:n)){
        # loop through all cluster classes
        current_cl_i <- current_cl[i]
        k_star_quad <- norm_phi_canonical_kmeans(X, last_centroids, XTv, XTv_norm, dir_XTv, v_norm, last_cl,
                                                 current_cl_i, v_vec, i) #i is the observation
        for (j in c(1:k)){
          if(j!=current_cl_i){
            k_current_quad <- norm_phi_canonical_kmeans(X, last_centroids, XTv, XTv_norm,
                                                        dir_XTv, v_norm, last_cl, j, v_vec, i) #i is the observation
            curr_quad <- minus_quad_ineq(k_star_quad, k_current_quad)
            curr_interval <- solve_one_ineq_complement(curr_quad$quad,
                                                       curr_quad$linear,
                                                       curr_quad$constant)
            all_interval_lists[[curr_len+curr_counter]] <- curr_interval
            curr_counter <- curr_counter + 1
          }
          # interval update
          #final_interval <- intervals::interval_intersection(final_interval, curr_interval)

        }
      }
    }
  }

  # final intervals look correct -- try to find truncation?
  final_interval_complement <- do.call('c', all_interval_lists)
  final_interval_complement <- matrix(final_interval_complement, ncol=2, byrow=T)
  final_interval_complement <- intervals::reduce(intervals::Intervals_full(final_interval_complement),
                                                 check_valid=FALSE)

  final_interval_chisq <- intervals::interval_complement(final_interval_complement)
  #intervals::interval_intersection(intervals::Intervals(c(0,Inf)),
  #                                 final_interval)
  return(final_interval_chisq)
}



#' Compute set S for isotropic case
#' @export
kmeans_compute_S_genCov <- function(X, estimated_k_means, all_T_clusters,
                                    all_T_centroids,
                                    n, XTv, XTv_norm,
                                    dir_XTv, v_vec,v_norm,T_length,
                                    Sig_XTv_norm, k){

  Sig_Inv_factor <- XTv_norm/Sig_XTv_norm
  final_interval <- intervals::Intervals(c(0,Inf))
  # keep track of all the intervals
  all_interval_lists <- list()

  # loop through the initialization
  init_list <- estimated_k_means$random_init_obs
  init_cluster <- all_T_clusters[1,]
  # look at covariance matrices -- a different S needed to be computed
  for (i in c(1:n)){
    current_j_prime <- init_cluster[i]
    j_star_quad <- norm_sq_phi(X, v_vec, XTv, XTv_norm, dir_XTv, v_norm,i,init_list[current_j_prime])
    for (j in c(1:length(init_list))){
      current_j_quad <- norm_sq_phi(X, v_vec, XTv, XTv_norm, dir_XTv, v_norm,i,init_list[j])
      curr_quad <- minus_quad_ineq(j_star_quad, current_j_quad)
      curr_interval <- solve_one_ineq_complement((Sig_Inv_factor)^2*curr_quad$quad,
                                                 (Sig_Inv_factor)*curr_quad$linear,
                                                 curr_quad$constant)
      all_interval_lists[[(i-1)*length(init_list)+j]] <- curr_interval
      #final_interval <- intervals::interval_intersection(final_interval, curr_interval)
    }
  }

  # keep track of the list elements
  curr_len <- length(all_interval_lists)
  curr_counter <- 1
  # loop through all sequence t
  if(T_length>1){
    for (l in c(1:(T_length-1))){
      current_cl <- all_T_clusters[(l+1),]
      last_cl <- all_T_clusters[(l),]
      # get pre-computed centroids
      last_centroids <- all_T_centroids[[(l+1)]] # k by q matrix
      # loop through all the observations
      for (i in c(1:n)){
        # loop through all cluster classes
        current_cl_i <- current_cl[i]
        k_star_quad <- norm_phi_canonical_kmeans(X, last_centroids, XTv, XTv_norm, dir_XTv, v_norm, last_cl,
                                                 current_cl_i, v_vec, i) #i is the observation
        for (j in c(1:k)){

          k_current_quad <- norm_phi_canonical_kmeans(X, last_centroids, XTv, XTv_norm,
                                                      dir_XTv, v_norm, last_cl, j, v_vec, i) #i is the observation
          curr_quad <- minus_quad_ineq(k_star_quad, k_current_quad)
          curr_interval <- solve_one_ineq_complement((Sig_Inv_factor)^2*curr_quad$quad,
                                                     (Sig_Inv_factor)*curr_quad$linear,
                                                     curr_quad$constant)
          # interval update
          all_interval_lists[[curr_len+curr_counter]] <- curr_interval
          curr_counter <- curr_counter + 1

        }
      }
    }
  }

  # final intervals look correct -- try to find truncation?
  final_interval_complement <- do.call('c', all_interval_lists)
  final_interval_complement <- matrix(final_interval_complement, ncol=2, byrow=T)
  final_interval_complement <- intervals::reduce(intervals::Intervals(final_interval_complement),
                                                 check_valid=FALSE)

  final_interval_chisq <- intervals::interval_complement(final_interval_complement)
  #intervals::interval_intersection(intervals::Intervals(c(0,Inf)),
  #                                 final_interval)
  return(final_interval_chisq)

}


#' Solve the roots of quadratic polynomials related to testing for a difference in means
#' wrt a single feature
#'
#' Solves \eqn{ax^2 + bx + c \ge 0}, then returns the complement of the solution set
#' wrt to the real line, unless the complement is empty, in which case
#' the function returns NA.
#'
#' @keywords internal
#'
#' @param A, B, C the coefficients of the quadratic equation.
#' @param tol if \eqn{|a|}, \eqn{|b|}, or \eqn{|c|} is not larger than tol,
#' then we treat it as zero.
#'
#' @return Returns an "Intervals" object containing NA or the complement of the solution set.
solve_one_ineq_1f <- function(A, B, C, tol = 1e-10) {
  # return interval for x,
  # s.t. Bx + C >= 0
  affineIntForZEachSlice <- function(B, C, tol) {
    # degenerate case: B = 0
    if (abs(B) <= tol) {
      if (C >= -tol) { # C >=0 -> hold for all X
        return()
      }
      else {
        return(c(-Inf, Inf))
      }
    }

    # now we know B != 0
    temp <- -C/B
    if (B > tol) { # B > 0 -> X >= -C/B
      return(c(-Inf, temp))
    }

    # now we know B < 0 -> X <= -C/B
    return(c(temp, Inf))
  }


  # denegerate case: A = 0
  if (abs(A) <= tol) {
    return(affineIntForZEachSlice(B = B, C = C, tol = tol))
  }

  # now we know A != 0
  disc <- B^2 - 4*A*C
  if (disc <= tol) { # discriminant <= 0

    # notice: we ignore the case when truncation is a single point

    if (A > tol) { # parabola open upwards
      # polynomial always >= 0
      return()
    }
    else { # parabola open downwars
      # polynomial always <= 0 -> no such X exists
      return(c(-Inf, Inf))
    }
  }

  return(c(roots[2], Inf))
}

# ----- functions for computing truncation sets -----
# ----- isotropic covariance mat -----
#' Computes the conditioning set for single linkage hierarchical clustering
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_single <- function(X, hcl, K, k1, k2) {
  # Initialization and book-keeping
  n <- nrow(X)
  h <- hcl$height[n-K]
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # solve inequalities involving i in cluster k1 and j in cluster k2, and save the results
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      new_intervals <- solve_one_ineq(1, 2*(cross_ij/dist_means - dist_means),
                                      sum(diff_ij^2) + squared_dist_means - 2*cross_ij - h)
      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      new_intervals <- solve_one_ineq(squared_prop_k2,
                                      2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means),
                                      sum(diff_ij^2) + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij - h)
      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      new_intervals <- solve_one_ineq(squared_prop_k1,
                                      2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means),
                                      sum(diff_ij^2) + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij - h)
      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set for average linkage hierarchical clustering
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_average <- function(X, hcl, K, k1, k2) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij/dist_means - dist_means)
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij/dist_means - dist_means)
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }


  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    if(cl[min_cluster_1] == k1) {
      loop_index <- c(k2_obs, other_obs)
    }

    if(cl[min_cluster_1] == k2) {
      loop_index <- c(k1_obs, other_obs)
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      loop_index <- c(k1_obs, k2_obs)
    }

    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- prop_min_1*B[min_cluster_1, j] + prop_min_2*B2
        C[min_cluster_1, j] <- prop_min_1*C[min_cluster_1, j] + prop_min_2*C2
      } else {
        B[j, min_cluster_1] <- prop_min_1*B[j, min_cluster_1] + prop_min_2*B2
        C[j, min_cluster_1] <- prop_min_1*C[j, min_cluster_1] + prop_min_2*C2
      }
    }


    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set for centroid linkage hierarchical clustering
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_centroid <- function(X, hcl, K, k1, k2) {
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))
  inv <- as.double(which(diff(hcl$height, 1) < 0)) # inversion locations
  num_inv <- length(inv)
  min_inv <- min(inv)
  max_inv <- max(inv)

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij/dist_means - dist_means)
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij/dist_means - dist_means)
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      # We want the maximum in [1, upper_ij].
      # If the first inversion is after upper_ij then there are no inversions in there.
      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        # Otherwise we need to find the locations of the inversions
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        # Then take the max over the inversions & upper_ij
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  finished_inversions <- FALSE

  for(step in 1:(n-K-1)) {
    if((step+1) > max_inv) {
      finished_inversions <- TRUE # We no longer have to worry about inversions
    } else {
      first_inversion <- findInterval(step+1, inv, left.open=TRUE)  # Find location of largest inversion st less than (step + 1)
      if(first_inversion != 0) {
        inv <- inv[(first_inversion+1):length(inv)]
      }
    }


    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }


    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    loop_index <- c(k1_obs, k2_obs, other_obs)


    C_constant <- prop_min_1*prop_min_2*C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- prop_min_1*B[min_cluster_1, j] + prop_min_2*B2 # note that B[min_cluster_2, min_cluster_1] = 0
        C[min_cluster_1, j] <- prop_min_1*C[min_cluster_1, j] + prop_min_2*C2 - C_constant
      } else {
        B[j, min_cluster_1] <- prop_min_1*B[j, min_cluster_1] + prop_min_2*B2
        C[j, min_cluster_1] <- prop_min_1*C[j, min_cluster_1] + prop_min_2*C2 - C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    upper_min_cluster_1 <- height_merge[min_cluster_1]

    if(cl[min_cluster_1] == k1) {
      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        # We want the maximum in [(step+1), upper_ij].
        # If the first inversion is after upper_ij or the last inversion is before (step+1)
        # then there are no inversions in there.
        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          # Otherwise we need to find the location of the inversions in [(step+1), upper_ij]
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set for Ward linkage hierarchical clustering
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_ward <- function(X, hcl, K, k1, k2) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  A <- matrix(NA, nrow(X), nrow(X))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        A[i, j] <- 1
        B[i, j] <- 2*(cross_ij/dist_means - dist_means)
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        A[j, i] <- 1
        B[j, i] <- 2*(cross_ij/dist_means - dist_means)
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }


  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        A[i, j] <- squared_prop_k2
        B[i, j] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        A[j, i] <- squared_prop_k2
        B[j, i] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        A[i, j] <- squared_prop_k1
        B[i, j] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        A[j, i] <- squared_prop_k1
        B[j, i] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    min_cluster_1_size <- cluster_sizes[min_cluster_1]
    min_cluster_2_size <- cluster_sizes[min_cluster_2]
    sum_min_cluster_sizes <- min_cluster_1_size + min_cluster_2_size

    loop_index <- c(k1_obs, k2_obs, other_obs)

    C_constant <- C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      cluster_j_size <- cluster_sizes[j]
      sum_cluster_sizes <- sum_min_cluster_sizes + cluster_j_size

      beta <- cluster_j_size/sum_cluster_sizes
      alpha1 <- min_cluster_1_size/sum_cluster_sizes + beta
      alpha2 <- min_cluster_2_size/sum_cluster_sizes + beta

      if(j < min_cluster_2) {
        A2 <- A[min_cluster_2, j]
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        A2 <- A[j, min_cluster_2]
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        A[min_cluster_1, j] <- alpha1*A[min_cluster_1, j] + alpha2*A2 # note that A[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        B[min_cluster_1, j] <- alpha1*B[min_cluster_1, j] + alpha2*B2 # note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[min_cluster_1, j] <- alpha1*C[min_cluster_1, j] + alpha2*C2 - beta*C_constant
      } else {
        A[j, min_cluster_1] <- alpha1*A[j, min_cluster_1] + alpha2*A2 # note that A[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        B[j, min_cluster_1] <- alpha1*B[j, min_cluster_1] + alpha2*B2 # note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[j, min_cluster_1] <- alpha1*C[j, min_cluster_1] + alpha2*C2 - beta*C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- sum_min_cluster_sizes
    cluster_sizes[min_cluster_2] <- NA


    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set for McQuitty linkage hierarchical clustering (WPGMA)
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_mcquitty <- function(X, hcl, K, k1, k2) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij/dist_means - dist_means)
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij/dist_means - dist_means)
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }


  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    if(cl[min_cluster_1] == k1) {
      loop_index <- c(k2_obs, other_obs)
    }

    if(cl[min_cluster_1] == k2) {
      loop_index <- c(k1_obs, other_obs)
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      loop_index <- c(k1_obs, k2_obs)
    }

    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- 0.5*B[min_cluster_1, j] + 0.5*B2
        C[min_cluster_1, j] <- 0.5*C[min_cluster_1, j] + 0.5*C2
      } else {
        B[j, min_cluster_1] <- 0.5*B[j, min_cluster_1] + 0.5*B2
        C[j, min_cluster_1] <- 0.5*C[j, min_cluster_1] + 0.5*C2
      }
    }


    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set for median linkage hierarchical clustering (WPGMC)
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_median <- function(X, hcl, K, k1, k2) {
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))
  inv <- as.double(which(diff(hcl$height, 1) < 0)) # inversion locations
  num_inv <- length(inv)
  min_inv <- min(inv)
  max_inv <- max(inv)

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij/dist_means - dist_means)
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij/dist_means - dist_means)
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij/dist_means - prop_k2*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij/dist_means - prop_k1*dist_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      # We want the maximum in [1, upper_ij].
      # If the first inversion is after upper_ij then there are no inversions in there.
      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        # Otherwise we need to find the locations of the inversions
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        # Then take the max over the inversions & upper_ij
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      # current_height <- max(heights[inv[inv < upper_ij]], heights[upper_ij])

      if(i > j) {
        new_intervals <- solve_one_ineq(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  finished_inversions <- FALSE

  for(step in 1:(n-K-1)) {
    if((step+1) > max_inv) {
      finished_inversions <- TRUE # We no longer have to worry about inversions
    } else {
      first_inversion <- findInterval(step+1, inv, left.open=TRUE)  # Find location of largest inversion st less than (step + 1)
      if(first_inversion != 0) {
        inv <- inv[(first_inversion+1):length(inv)]
      }
    }


    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }


    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    # Update coefficient matrices
    loop_index <- c(k1_obs, k2_obs, other_obs)

    C_constant <- 0.25*C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- 0.5*B[min_cluster_1, j] + 0.5*B2 # Note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[min_cluster_1, j] <- 0.5*C[min_cluster_1, j] + 0.5*C2 - C_constant
      } else {
        B[j, min_cluster_1] <- 0.5*B[j, min_cluster_1] + 0.5*B2
        C[j, min_cluster_1] <- 0.5*C[j, min_cluster_1] + 0.5*C2 - C_constant
      }
    }


    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    upper_min_cluster_1 <- height_merge[min_cluster_1]

    if(cl[min_cluster_1] == k1) {
      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        # We want the maximum in [(step+1), upper_ij].
        # If the first inversion is after upper_ij or the last inversion is before (step+1)
        # then there are no inversions in there.
        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          # Otherwise we need to find the location of the inversions in [(step+1), upper_ij]
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

# ----- non-isotropic covariance mat -----
#' Computes the conditioning set S for single linkage hierarchical clustering,
#' w/o assuming isotropic covariance matrix
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param stat the test statistic, \eqn{||\Sigma^{-1/2} x^T \nu||_2}
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_single_gencov <- function(X, hcl, K, k1, k2, stat) {
  # Initialization and book-keeping
  n <- nrow(X)
  h <- hcl$height[n-K]
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # solve inequalities involving i in cluster k1 and j in cluster k2, and save the results
  gencov_factor <- squared_dist_means/(stat^2)
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      new_intervals <- solve_one_ineq(gencov_factor, 2*(cross_ij - squared_dist_means)/stat,
                                      sum(diff_ij^2) + squared_dist_means - 2*cross_ij - h)
      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  # solve inequalities involving i in cluster k1 and j not in clusters k1 or k2, and save the results
  A <- squared_prop_k2*gencov_factor
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      new_intervals <- solve_one_ineq(A,
                                      2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat,
                                      sum(diff_ij^2) + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij - h)
      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  # solve inequalities involving i in cluster k2 and j not in clusters k1 or k2, and save the results
  A <- squared_prop_k1*gencov_factor
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      new_intervals <- solve_one_ineq(A,
                                      2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat,
                                      sum(diff_ij^2) + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij - h)

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set S for average linkage hierarchical clustering,
#' w/o assuming isotropic covariance matrix
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param stat the test statistic, \eqn{||\Sigma^{-1/2} x^T \nu||_2}
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_average_gencov <- function(X, hcl, K, k1, k2, stat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij - squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij - squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  gencov_factor <- squared_dist_means/(stat^2)

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(gencov_factor, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(gencov_factor, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k2*gencov_factor
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k1*gencov_factor
  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    if(cl[min_cluster_1] == k1) {
      loop_index <- c(k2_obs, other_obs)
    }

    if(cl[min_cluster_1] == k2) {
      loop_index <- c(k1_obs, other_obs)
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      loop_index <- c(k1_obs, k2_obs)
    }

    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- prop_min_1*B[min_cluster_1, j] + prop_min_2*B2
        C[min_cluster_1, j] <- prop_min_1*C[min_cluster_1, j] + prop_min_2*C2
      } else {
        B[j, min_cluster_1] <- prop_min_1*B[j, min_cluster_1] + prop_min_2*B2
        C[j, min_cluster_1] <- prop_min_1*C[j, min_cluster_1] + prop_min_2*C2
      }
    }


    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k2
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      A <- gencov_factor*squared_prop_k2
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)
  return(S)
}

#' Computes the conditioning set S for centroid linkage hierarchical clustering,
#' w/o assuming isotropic covariance matrix
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param stat the test statistic, \eqn{||\Sigma^{-1/2} x^T \nu||_2}
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_centroid_gencov <- function(X, hcl, K, k1, k2, stat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij - squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij - squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  gencov_factor <- squared_dist_means/(stat^2)

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(gencov_factor, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(gencov_factor, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k2*gencov_factor
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k1*gencov_factor
  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    loop_index <- c(k1_obs, k2_obs, other_obs)

    C_constant <- prop_min_1*prop_min_2*C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- prop_min_1*B[min_cluster_1, j] + prop_min_2*B2 # note that B[min_cluster_2, min_cluster_1] = 0
        C[min_cluster_1, j] <- prop_min_1*C[min_cluster_1, j] + prop_min_2*C2 - C_constant
      } else {
        B[j, min_cluster_1] <- prop_min_1*B[j, min_cluster_1] + prop_min_2*B2
        C[j, min_cluster_1] <- prop_min_1*C[j, min_cluster_1] + prop_min_2*C2 - C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k2
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      A <- gencov_factor*squared_prop_k2
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)
  return(S)
}

#' Computes the conditioning set S for ward linkage hierarchical clustering,
#' w/o assuming isotropic covariance matrix
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param stat the test statistic, \eqn{||\Sigma^{-1/2} x^T \nu||_2}
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_ward_gencov <- function(X, hcl, K, k1, k2, stat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  A <- matrix(NA, nrow(X), nrow(X))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)
  gencov_factor <- squared_dist_means/(stat^2)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        A[i, j] <- gencov_factor
        B[i, j] <- 2*(cross_ij - squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        A[j, i] <- gencov_factor
        B[j, i] <- 2*(cross_ij - squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  this_A <- gencov_factor*squared_prop_k2
  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        A[i, j] <- this_A
        B[i, j] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        A[j, i] <- this_A
        B[j, i] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  this_A <- gencov_factor*squared_prop_k1
  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        A[i, j] <- this_A
        B[i, j] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        A[j, i] <- this_A
        B[j, i] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(gencov_factor, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(gencov_factor, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  this_A <- squared_prop_k2*gencov_factor
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(this_A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(this_A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  this_A <- squared_prop_k1*gencov_factor
  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(this_A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(this_A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }


    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    # Update coefficient matrices
    min_cluster_1_size <- cluster_sizes[min_cluster_1]
    min_cluster_2_size <- cluster_sizes[min_cluster_2]
    sum_min_cluster_sizes <- min_cluster_1_size + min_cluster_2_size

    loop_index <- c(k1_obs, k2_obs, other_obs)

    C_constant <- C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      cluster_j_size <- cluster_sizes[j]
      sum_cluster_sizes <- sum_min_cluster_sizes + cluster_j_size

      beta <- cluster_j_size/sum_cluster_sizes
      alpha1 <- min_cluster_1_size/sum_cluster_sizes + beta
      alpha2 <- min_cluster_2_size/sum_cluster_sizes + beta

      if(j < min_cluster_2) {
        A2 <- A[min_cluster_2, j]
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        A2 <- A[j, min_cluster_2]
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        A[min_cluster_1, j] <- alpha1*A[min_cluster_1, j] + alpha2*A2 # note that A[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        B[min_cluster_1, j] <- alpha1*B[min_cluster_1, j] + alpha2*B2 # note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[min_cluster_1, j] <- alpha1*C[min_cluster_1, j] + alpha2*C2 - beta*C_constant
      } else {
        A[j, min_cluster_1] <- alpha1*A[j, min_cluster_1] + alpha2*A2 # note that A[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        B[j, min_cluster_1] <- alpha1*B[j, min_cluster_1] + alpha2*B2 # note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[j, min_cluster_1] <- alpha1*C[j, min_cluster_1] + alpha2*C2 - beta*C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- sum_min_cluster_sizes
    cluster_sizes[min_cluster_2] <- NA

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)

  return(S)
}

#' Computes the conditioning set S for McQuitty linkage hierarchical clustering (WPGMA),
#' w/o assuming isotropic covariance matrix
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param stat the test statistic, \eqn{||\Sigma^{-1/2} x^T \nu||_2}
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_mcquitty_gencov <- function(X, hcl, K, k1, k2, stat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij - squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij - squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  gencov_factor <- squared_dist_means/(stat^2)

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(gencov_factor, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(gencov_factor, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k2*gencov_factor
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k1*gencov_factor
  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    if(cl[min_cluster_1] == k1) {
      loop_index <- c(k2_obs, other_obs)
    }

    if(cl[min_cluster_1] == k2) {
      loop_index <- c(k1_obs, other_obs)
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      loop_index <- c(k1_obs, k2_obs)
    }

    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- 0.5*B[min_cluster_1, j] + 0.5*B2
        C[min_cluster_1, j] <- 0.5*C[min_cluster_1, j] + 0.5*C2
      } else {
        B[j, min_cluster_1] <- 0.5*B[j, min_cluster_1] + 0.5*B2
        C[j, min_cluster_1] <- 0.5*C[j, min_cluster_1] + 0.5*C2
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k2
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      A <- gencov_factor*squared_prop_k2
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)
  return(S)
}

#' Computes the conditioning set S for median linkage hierarchical clustering (WPGMC),
#' w/o assuming isotropic covariance matrix
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param stat the test statistic, \eqn{||\Sigma^{-1/2} x^T \nu||_2}
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_median_gencov <- function(X, hcl, K, k1, k2, stat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list(c(-Inf, 0))
  list_index <- 2

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all coefficients
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2-1
  squared_prop_k1 <- prop_k1^2
  diff_means <- colMeans(X[k1_obs, , drop=FALSE]) - colMeans(X[k2_obs, , drop=FALSE])
  squared_dist_means <- sum(diff_means^2)
  dist_means <- sqrt(squared_dist_means)

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*(cross_ij - squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_dist_means - 2*cross_ij
      } else {
        B[j, i] <- 2*(cross_ij - squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_dist_means - 2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and i' not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_ij - prop_k2*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k2*squared_dist_means - 2*prop_k2*cross_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and i' not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_ij <- sum(diff_ij*diff_means)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[i, j] <- C[i, j] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_ij - prop_k1*squared_dist_means)/stat
        C[j, i] <- C[j, i] + squared_prop_k1*squared_dist_means - 2*prop_k1*cross_ij
      }
    }
  }

  gencov_factor <- squared_dist_means/(stat^2)

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(gencov_factor, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(gencov_factor, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k2*gencov_factor
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  A <- squared_prop_k1*gencov_factor
  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq(A, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(A, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    loop_index <- c(k1_obs, k2_obs, other_obs)

    C_constant <- 0.25*C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- 0.5*B[min_cluster_1, j] + 0.5*B2 # note that B[min_cluster_2, min_cluster_1] = 0
        C[min_cluster_1, j] <- 0.5*C[min_cluster_1, j] + 0.5*C2 - C_constant
      } else {
        B[j, min_cluster_1] <- 0.5*B[j, min_cluster_1] + 0.5*B2
        C[j, min_cluster_1] <- 0.5*C[j, min_cluster_1] + 0.5*C2 - C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k2
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(gencov_factor, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(gencov_factor, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      A <- gencov_factor*squared_prop_k2
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      A <- gencov_factor*squared_prop_k1
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq(A, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq(A, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  S_complement <- do.call('c', S_complement)
  S_complement <- matrix(S_complement, length(S_complement)/2, 2, byrow=TRUE)
  S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)

  # complement the complement to get S
  S <- intervals::interval_complement(S_complement, check_valid=FALSE)
  return(S)
}
<<<<<<< HEAD

# ----- diff in means between a single feature -----
#' Computes the conditioning set S for single linkage hierarchical clustering,
#' when we're testing for a difference in means wrt a single feature
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_single_1f <- function(X, hcl, K, k1, k2, feat) {
  # Initialization and book-keeping
  n <- nrow(X)
  h <- hcl$height[n-K]
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list()
  list_index <- 1

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  orig_dist <- as.matrix(stats::dist(X, method="euclidean")^2)

  # solve inequalities involving i in cluster k1 and j in cluster k2, and save the results
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      new_intervals <- solve_one_ineq_1f(1, 2*(diff_ij - diff_means),
                                      orig_dist[i, j] + squared_diff_means - 2*diff_means*diff_ij - h)

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  # solve inequalities involving i in cluster k1 and j not in clusters k1 or k2, and save the results
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      new_intervals <- solve_one_ineq_1f(squared_prop_k2, 2*prop_k2*(diff_ij - prop_k2*diff_means),
                                         orig_dist[i, j] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij - h)

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  # solve inequalities involving i in cluster k2 and j not in clusters k1 or k2, and save the results
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      new_intervals <- solve_one_ineq_1f(squared_prop_k1, 2*prop_k1*(diff_ij - prop_k1*diff_means),
                                         orig_dist[i, j] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij - h)

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  if(length(S_complement) != 0) {
    S_complement <- do.call('c', S_complement)
    S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
    S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)
    S <- intervals::interval_complement(S_complement, check_valid=F)
  } else {
    S <- intervals::Intervals(c(-Inf, Inf))
  }

  return(S)
}

#' Computes the conditioning set S for average linkage hierarchical clustering,
#' when we're testing for a difference in means wrt a single feature
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_average_1f <- function(X, hcl, K, k1, k2, feat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list()
  list_index <- 1

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*(diff_ij - diff_means)
        C[i, j] <- C[i, j] + squared_diff_means - 2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*(diff_ij - diff_means)
        C[j, i] <- C[j, i] + squared_diff_means - 2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]


      if(i > j) {
        B[i, j] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    if(cl[min_cluster_1] == k1) {
      loop_index <- c(k2_obs, other_obs)
    }

    if(cl[min_cluster_1] == k2) {
      loop_index <- c(k1_obs, other_obs)
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      loop_index <- c(k1_obs, k2_obs)
    }

    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- prop_min_1*B[min_cluster_1, j] + prop_min_2*B2
        C[min_cluster_1, j] <- prop_min_1*C[min_cluster_1, j] + prop_min_2*C2
      } else {
        B[j, min_cluster_1] <- prop_min_1*B[j, min_cluster_1] + prop_min_2*B2
        C[j, min_cluster_1] <- prop_min_1*C[j, min_cluster_1] + prop_min_2*C2
      }
    }


    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  if(length(S_complement) != 0) {
    S_complement <- do.call('c', S_complement)
    S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
    S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)
    S <- intervals::interval_complement(S_complement, check_valid=F)
  } else {
    S <- intervals::Intervals(c(-Inf, Inf))
  }

  return(S)
}

#' Computes the conditioning set S for centroid linkage hierarchical clustering,
#' when we're testing for a difference in means wrt a single feature
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_centroid_1f <- function(X, hcl, K, k1, k2, feat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  inv <- which(diff(hcl$height, 1) < 0) # inversion locations
  num_inv <- length(inv)
  min_inv <- min(inv)
  max_inv <- max(inv)

  S_complement <- list()
  list_index <- 1

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*(diff_ij - diff_means)
        C[i, j] <- C[i, j] + squared_diff_means - 2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*(diff_ij - diff_means)
        C[j, i] <- C[j, i] + squared_diff_means - 2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      }
    }
  }

  # solve inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  finished_inversions <- FALSE


  for(step in 1:(n-K-1)) {
    if((step+1) > max_inv) {
      finished_inversions <- TRUE # We no longer have to worry about inversions
    } else {
      first_inversion <- findInterval(step+1, inv, left.open=TRUE)  # Find location of largest inversion st less than (step + 1)
      if(first_inversion != 0) {
        inv <- inv[(first_inversion+1):length(inv)]
      }
    }

    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    prop_min_1 <- cluster_sizes[min_cluster_1]/(cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2])
    prop_min_2 <- 1 - prop_min_1

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    loop_index <- c(k1_obs, k2_obs, other_obs)


    C_constant <- prop_min_1*prop_min_2*C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- prop_min_1*B[min_cluster_1, j] + prop_min_2*B2 # note that B[min_cluster_2, min_cluster_1] = 0
        C[min_cluster_1, j] <- prop_min_1*C[min_cluster_1, j] + prop_min_2*C2 - C_constant
      } else {
        B[j, min_cluster_1] <- prop_min_1*B[j, min_cluster_1] + prop_min_2*B2
        C[j, min_cluster_1] <- prop_min_1*C[j, min_cluster_1] + prop_min_2*C2 - C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    upper_min_cluster_1 <- height_merge[min_cluster_1]

    if(cl[min_cluster_1] == k1) {
      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        # We want the maximum in [(step+1), upper_ij].
        # If the first inversion is after upper_ij or the last inversion is before (step+1)
        # then there are no inversions in there.
        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          # Otherwise we need to find the location of the inversions in [(step+1), upper_ij]
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  if(length(S_complement) != 0) {
    S_complement <- do.call('c', S_complement)
    S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
    S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)
    S <- intervals::interval_complement(S_complement, check_valid=F)
  } else {
    S <- intervals::Intervals(c(-Inf, Inf))
  }

  return(S)
}

#' Computes the conditioning set S for ward linkage hierarchical clustering,
#' when we're testing for a difference in means wrt a single feature
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_ward_1f <- function(X, hcl, K, k1, k2, feat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list()
  list_index <- 1

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  A <- matrix(NA, nrow(X), nrow(X))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        A[i, j] <- 1
        B[i, j] <- 2*(diff_ij - diff_means)
        C[i, j] <- C[i, j] + squared_diff_means - 2*diff_means*diff_ij
      } else {
        A[j, i] <- 1
        B[j, i] <- 2*(diff_ij - diff_means)
        C[j, i] <- C[j, i] + squared_diff_means - 2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        A[i, j] <- squared_prop_k2
        B[i, j] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      } else {
        A[j, i] <- squared_prop_k2
        B[j, i] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]


      if(i > j) {
        A[i, j] <- squared_prop_k1
        B[i, j] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      } else {
        A[j, i] <- squared_prop_k1
        B[j, i] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }


    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    # Update coefficient matrix
    min_cluster_1_size <- cluster_sizes[min_cluster_1]
    min_cluster_2_size <- cluster_sizes[min_cluster_2]
    sum_min_cluster_sizes <- min_cluster_1_size + min_cluster_2_size

    loop_index <- c(k1_obs, k2_obs, other_obs)

    C_constant <- C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      cluster_j_size <- cluster_sizes[j]
      sum_cluster_sizes <- sum_min_cluster_sizes + cluster_j_size

      beta <- cluster_j_size/sum_cluster_sizes
      alpha1 <- min_cluster_1_size/sum_cluster_sizes + beta
      alpha2 <- min_cluster_2_size/sum_cluster_sizes + beta

      if(j < min_cluster_2) {
        A2 <- A[min_cluster_2, j]
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        A2 <- A[j, min_cluster_2]
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        A[min_cluster_1, j] <- alpha1*A[min_cluster_1, j] + alpha2*A2 # note that A[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        B[min_cluster_1, j] <- alpha1*B[min_cluster_1, j] + alpha2*B2 # note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[min_cluster_1, j] <- alpha1*C[min_cluster_1, j] + alpha2*C2 - beta*C_constant
      } else {
        A[j, min_cluster_1] <- alpha1*A[j, min_cluster_1] + alpha2*A2 # note that A[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        B[j, min_cluster_1] <- alpha1*B[j, min_cluster_1] + alpha2*B2 # note that B[min_cluster_1, min_cluster_2] = 0 by Lemma S2
        C[j, min_cluster_1] <- alpha1*C[j, min_cluster_1] + alpha2*C2 - beta*C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- sum_min_cluster_sizes
    cluster_sizes[min_cluster_2] <- NA

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(A[min_cluster_1, j], B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(A[j, min_cluster_1], B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  if(length(S_complement) != 0) {
    S_complement <- do.call('c', S_complement)
    S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
    S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)
    S <- intervals::interval_complement(S_complement, check_valid=F)
  } else {
    S <- intervals::Intervals(c(-Inf, Inf))
  }

  return(S)
}

#' Computes the conditioning set S for McQuitty linkage hierarchical clustering (WGPMA),
#' when we're testing for a difference in means wrt a single feature
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_mcquitty_1f <- function(X, hcl, K, k1, k2, feat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  S_complement <- list()
  list_index <- 1

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*(diff_ij - diff_means)
        C[i, j] <- C[i, j] + squared_diff_means - 2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*(diff_ij - diff_means)
        C[j, i] <- C[j, i] + squared_diff_means - 2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]


      if(i > j) {
        B[i, j] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }
      current_height <- heights[upper_ij]

      if(i > j) {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(step in 1:(n-K-1)) {
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    if(cl[min_cluster_1] == k1) {
      loop_index <- c(k2_obs, other_obs)
    }

    if(cl[min_cluster_1] == k2) {
      loop_index <- c(k1_obs, other_obs)
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      loop_index <- c(k1_obs, k2_obs)
    }

    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- 0.5*B[min_cluster_1, j] + 0.5*B2
        C[min_cluster_1, j] <- 0.5*C[min_cluster_1, j] + 0.5*C2
      } else {
        B[j, min_cluster_1] <- 0.5*B[j, min_cluster_1] + 0.5*B2
        C[j, min_cluster_1] <- 0.5*C[j, min_cluster_1] + 0.5*C2
      }
    }


    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    if(cl[min_cluster_1] == k1) {
      first_height <- height_merge[min_cluster_1]
      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      first_height <- height_merge[min_cluster_1]
      for(j in k1_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        if(first_height < height_merge[j]) {
          current_height <- heights[first_height]
        } else {
          current_height <- heights[height_merge[j]]
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  if(length(S_complement) != 0) {
    S_complement <- do.call('c', S_complement)
    S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
    S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)
    S <- intervals::interval_complement(S_complement, check_valid=F)
  } else {
    S <- intervals::Intervals(c(-Inf, Inf))
  }

  return(S)
}

#' Computes the conditioning set S for median linkage hierarchical clustering (WGPMC),
#' when we're testing for a difference in means wrt a single feature
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#'
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_median_1f <- function(X, hcl, K, k1, k2, feat) {
  # Initialization and book-keeping
  n <- nrow(X)
  heights <- hcl$height
  merges <- hcl$merge
  cl <- stats::cutree(hcl, K)
  k1_obs <- which(cl == k1)
  k2_obs <- which(cl == k2)
  other_obs <- setdiff(1:n, c(k1_obs, k2_obs))

  inv <- which(diff(hcl$height, 1) < 0) # inversion locations
  num_inv <- length(inv)
  min_inv <- min(inv)
  max_inv <- max(inv)

  S_complement <- list()
  list_index <- 1

  # where is each observation merged away?
  # if it's after the (n-K)th merge, then that cluster still exists at step (n-K)
  height_merge <- rep(n-K, n)
  for(l in 1:(n-K)) {
    minimal_clusters <- merges[l, ]
    height_merge[-minimal_clusters[minimal_clusters < 0]] <- l
  }

  # Step 1
  cluster_sizes <- rep(1, n)
  merged_to_index <- rep(NA, n)

  # Make the coefficients for d(i, i'; x'(\phi))
  B <- matrix(NA, nrow(X), nrow(X))
  C <- matrix(NA, nrow(X), nrow(X))
  C[lower.tri(C)] <- stats::dist(X)^2

  # compute quantities used in all inequalities
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*(diff_ij - diff_means)
        C[i, j] <- C[i, j] + squared_diff_means - 2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*(diff_ij - diff_means)
        C[j, i] <- C[j, i] + squared_diff_means - 2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]

      if(i > j) {
        B[i, j] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(diff_ij - prop_k2*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means - 2*prop_k2*diff_means*diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, feat] - X[j, feat]


      if(i > j) {
        B[i, j] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(diff_ij - prop_k1*diff_means)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means - 2*prop_k1*diff_means*diff_ij
      }
    }
  }

  # solve the inequalities
  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in k2_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      # We want the maximum in [1, upper_ij].
      # If the first inversion is after upper_ij then there are no inversions in there.
      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        # Otherwise we need to find the locations of the inversions
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        # Then take the max over the inversions & upper_ij
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      # current_height <- max(heights[inv[inv < upper_ij]], heights[upper_ij])

      if(i > j) {
        new_intervals <- solve_one_ineq(1, B[i, j], C[i, j] - current_height) # hard-coded first quadratic coef ... proof via Lemma S1 + algebra
      } else {
        new_intervals <- solve_one_ineq(1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  for(i in k1_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k2, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }


  for(i in k2_obs) {
    hm1 <- height_merge[i]
    for(j in other_obs) {
      hm2 <- height_merge[j]
      if(hm1 < hm2) {
        upper_ij <- hm1
      } else {
        upper_ij <- hm2
      }

      if(min_inv >= upper_ij) {
        current_height <- heights[upper_ij]
      } else {
        inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
        current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
      }

      if(i > j) {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(squared_prop_k1, B[j, i], C[j, i] - current_height)
      }

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  finished_inversions <- FALSE

  for(step in 1:(n-K-1)) {
    if((step+1) > max_inv) {
      finished_inversions <- TRUE # We no longer have to worry about inversions
    } else {
      first_inversion <- findInterval(step+1, inv, left.open=TRUE)  # Find location of largest inversion st less than (step + 1)
      if(first_inversion != 0) {
        inv <- inv[(first_inversion+1):length(inv)]
      }
    }
    # Which clusters merged in this step?
    minimal_clusters <- merges[step, ]
    for(i in 1:2) {
      if(minimal_clusters[i] > 0) {
        minimal_clusters[i] <- merged_to_index[minimal_clusters[i]]
      } else {
        minimal_clusters[i] <- -minimal_clusters[i]
      }
    }

    # Merge them to create the (step+1)th clustering
    min_cluster_1 <- min(minimal_clusters)
    min_cluster_2 <- max(minimal_clusters)

    merged_to_index[step] <- min_cluster_1

    # Update last step where each cluster exists
    # If it's merged away before n-K, then that's the step. If it's merged away after n-K,
    # or if it's never merged away, then it should be n-K
    height_merge[min_cluster_2] <- NA
    match_merge_row <- match(step, merges)

    if(is.na(match_merge_row)) {
      height_merge[min_cluster_1] <- n-K
    } else {
      if(match_merge_row > n-1) {
        match_merge_row <- match_merge_row - (n-1)
      }

      height_merge[min_cluster_1] <- min(n-K, match_merge_row)
    }

    # Update coefficient matrices
    # Update cluster indexing
    if(cl[min_cluster_1] == k1) k1_obs <- k1_obs[k1_obs != min_cluster_2]
    if(cl[min_cluster_1] == k2) k2_obs <- k2_obs[k2_obs != min_cluster_2]
    if((cl[min_cluster_1] != k1) & (cl[min_cluster_1] != k2)) other_obs <- other_obs[other_obs != min_cluster_2]

    loop_index <- c(k1_obs, k2_obs, other_obs)


    C_constant <- 0.25*C[min_cluster_2, min_cluster_1]
    for(j in loop_index) {
      if(j < min_cluster_2) {
        B2 <- B[min_cluster_2, j]
        C2 <- C[min_cluster_2, j]
      } else {
        B2 <- B[j, min_cluster_2]
        C2 <- C[j, min_cluster_2]
      }

      if(j < min_cluster_1) {
        B[min_cluster_1, j] <- 0.5*B[min_cluster_1, j] + 0.5*B2 # note that B[min_cluster_2, min_cluster_1] = 0
        C[min_cluster_1, j] <- 0.5*C[min_cluster_1, j] + 0.5*C2 - C_constant
      } else {
        B[j, min_cluster_1] <- 0.5*B[j, min_cluster_1] + 0.5*B2
        C[j, min_cluster_1] <- 0.5*C[j, min_cluster_1] + 0.5*C2 - C_constant
      }
    }

    # Update cluster sizes
    cluster_sizes[min_cluster_1] <- cluster_sizes[min_cluster_1] + cluster_sizes[min_cluster_2]
    cluster_sizes[min_cluster_2] <- NA

    upper_min_cluster_1 <- height_merge[min_cluster_1]

    if(cl[min_cluster_1] == k1) {
      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        # We want the maximum in [(step+1), upper_ij].
        # If the first inversion is after upper_ij or the last inversion is before (step+1)
        # then there are no inversions in there.
        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          # Otherwise we need to find the location of the inversions in [(step+1), upper_ij]
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] == k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in other_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }

    if(cl[min_cluster_1] != k1 & cl[min_cluster_1] != k2) {
      for(j in k1_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }

      for(j in k2_obs) {
        hmj <- height_merge[j]
        if(upper_min_cluster_1 < hmj) {
          upper_ij <- upper_min_cluster_1
        } else {
          upper_ij <- hmj
        }

        if(finished_inversions | min_inv >= upper_ij) {
          current_height <- heights[upper_ij]
        } else {
          inv_locations <- findInterval(upper_ij, inv, left.open=TRUE)
          if(inv_locations != 0) {
            current_height <- max(heights[inv[1:inv_locations]], heights[upper_ij])
          } else {
            current_height <- heights[upper_ij]
          }
        }

        if(min_cluster_1 > j) {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
        }

        if(!is.null(new_intervals)) {
          S_complement[[list_index]] <- new_intervals
          list_index <- list_index + 1
        }
      }
    }
  }

  if(length(S_complement) != 0) {
    S_complement <- do.call('c', S_complement)
    S_complement <- matrix(S_complement, length(S_complement), 2, byrow=TRUE)
    S_complement <- intervals::reduce(intervals::Intervals(S_complement), check_valid=FALSE)
    S <- intervals::interval_complement(S_complement, check_valid=F)
  } else {
    S <- intervals::Intervals(c(-Inf, Inf))
  }

  return(S)
}
