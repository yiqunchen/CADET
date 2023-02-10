
# ----- functions for solving quadratic inequalities -----
#' Solve the roots of quadratic polynomials related to testing for a difference in means
#'
#' Solves \eqn{ax^2 + bx + c \le 0}, then returns the complement of the solution set
#' wrt to the real line, unless the complement is empty, in which case
#' the function returns NA.
#'
#' @keywords internal
#'
#' @param A, B, C the coefficients of the quadratic equation.
#' @param tol if \eqn{|a|}, \eqn{|b|}, or \eqn{|c|} is not larger than tol, then treat it as zero.
#'
#' @return Returns an "Intervals" object containing NA or the complement of the solution set.
solve_one_ineq_complement_1f <- function(A, B, C, tol=1e-10) {
  # Computes the complement of the set {phi: B*phi + C <=  0},
  compute_linear_ineq_complement_1f <- function(B, C, tol=1e-8) {
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
  }

  # A = 0?
  if(abs(A) <= tol) {
    return(compute_linear_ineq_complement_1f(B, C, tol))
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
    #roots <- sort(c(root_1, root_2))
  }else{
    root_1 <- (-B+sqrt_discrim)/(2*A)
    root_2 <- (2*C)/(-B+sqrt_discrim)
    #roots <- sort(c(root_1, root_2))
  }

  if(A > tol) {
    return(c(-Inf, min(root_1,root_2), max(root_1,root_2), Inf))
  } else {
    # We now know that there are two roots, and parabola opens down (A < 0)
    return(c(min(root_1,root_2), max(root_1,root_2)))
  }

}

#' Represent ||x'(phi, psi)_i-x'(phim psi)_j||_2^2 as a quadratic function of phi
#' when we're testing for a difference in means wrt a single feature  ----
#' @keywords internal
#'
#' @param XTv, vector p by 1
#' @param v_norm, ell_2 norm of vector v
#' @param i, first index
#' @param j, second index
#' @param v, the vector
#' @param feat, integer, the index of the feature involved in the test
#' @param scaledSigRow a vector p by 1 containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @param scaledSigRow_2_norm ell_2 norm of the vector scaledSigRow
#' @param UPsi the matrix-vector product of U and Psi (defined in xx TODO:
#' YC add reference to paper)
#'
#' @return parameters: a, b, c the coefficients of the quadratic equation
#' such that (ax^2 + bx + c <= 0)
#'
norm_sq_phi_kmeans_1f_less_cond <- function(X, v, a, B,
                                            diff_means_feat, mean_feat,
                                            BXj,v_norm, a_norm, i, j, feat,
                                            scaledSigRow,
                                            scaledSigRow_2_norm,UPsi){
  # avoid repeated computation
  # compute quadratic coef

  # the quadratic part stays the same!
  quad_coef <- (v[i]-v[j])^2/(v_norm)^4*(scaledSigRow_2_norm)^2
  # this part has some major changes
  linear_coef <- 2*((v[i]-v[j])/(v_norm^2)*(scaledSigRow_2_norm^2))*
    ((UPsi[i]-UPsi[j])-(X[i,feat]-X[j,feat])+(a[i]-a[j])/a_norm*mean_feat+
       ((B[i,]-B[j,])%*%BXj))

  constant_vec <- X[i,]-X[j,]-((UPsi[i]-UPsi[j])-
      (X[i,feat]-X[j,feat])+(a[i]-a[j])/a_norm*mean_feat+((B[i,]-B[j,])%*%BXj))*scaledSigRow

  # compute 2 norm squared based on the constant vec
  constant_coef <- sum(constant_vec*constant_vec)
  coef_list <- list("quad" = as.numeric(quad_coef), "linear" = as.numeric(linear_coef),
                    "constant"= as.numeric(constant_coef))
  return(coef_list)
}

#' Represent <x'(phi)_i, x'(phi)_j> as a quadratic function in phi ----
#' @keywords internal
#'
#' @param XTv, vector p by 1
#' @param v_norm, 2-norm of vector v
#' @param cl, factor vector n by 1 (most recent cluster assignment)
#' @param k, cluster of interest
#' @param v, contrast vector n by 1
#' @param i, index of observation
#' @param feat, integer, the index of the feature involved in the test
#' @param scaledSigRow a vector p by 1 containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @param scaledSigRow_2_norm ell_2 norm of the vector scaledSigRow
#'
#' @return parameters: a, b, c the coefficients of the quadratic equation such that (ax^2 + bx + c <= 0)
#' @export
#'
norm_phi_canonical_kmeans_1f_less_cond <- function(X, last_centroids,
                                                   diff_means_feat, v_norm,
                                         cl, k, v, i, weighted_v_i,
                                         feat,mean_feat,
                                         scaledSigRow, scaledSigRow_2_norm,
                                         UPsi, B, BXj, a, a_norm,
                                         weighted_UPsi, weighted_B,
                                         weighted_xj, weighted_a){
  # replace the j part with weighted sum for easier computation
  v_i_expression <- (v[i]-weighted_v_i[k])/(v_norm^2)
  x_i_expression <- X[i,] - last_centroids[k,]
  UPsi_i_expression <- UPsi[i]-weighted_UPsi[k]
  B_i_expression <- B[i,]-weighted_B[k,]
  x_ij_expression <- X[i,j]-weighted_xj[k]
  a_i_expression <- a[i]-weighted_a[k]
  # n_k and class k
  quad_coef <- (v_i_expression*scaledSigRow_2_norm)^2 # updated, same as before
  # compute lienar coef
  linear_coef_multipler <- 2*v_i_expression*(scaledSigRow_2_norm^2)
  linear_coef_part_1 <- UPsi_i_expression-x_ij_expression
  linear_coef_part_2 <- (a_i_expression/a_norm*mean_feat)+(B_i_expression%*%BXj)
  linear_coef <- linear_coef_multipler*(linear_coef_part_1+linear_coef_part_2)
  # compute constant vec
  constant_vec_part_2 <- (UPsi_i_expression-x_ij_expression+
                            (a_i_expression/a_norm*mean_feat)+(B_i_expression%*%BXj))
  constant_vec <- x_i_expression - constant_vec_part_2 %*% scaledSigRow
  constant_coef <- sum(constant_vec*constant_vec)

  coef_list <- list("quad" = as.numeric(quad_coef), "linear" = as.numeric(linear_coef),
                    "constant"= as.numeric(constant_coef))
  return(coef_list)
}

#' Compute set S for the isotropic case
#' TODO: Yiqun: think about if that's still needed now that we have
#' the general case now -- let's keep it as stale for now.
#' @keywords internal
#' @export
#' @param XTv, vector p by 1
#' @param v_norm, 2-norm of vector v
#' @param cl, factor vector n by 1 (most recent cluster assignment)
#' @param k, cluster of interest
#' @param v, contrast vector n by 1
#' @param i, index of observation
#' @param feat, integer, the index of the feature involved in the test
#' @param scaledSigRow a vector p by 1 containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @param scaledSigRow_2_norm ell_2 norm of the vector scaledSigRow
#' @return Returns an "Intervals" object containing the conditioning set.
#'
kmeans_compute_S_1f_iso_less_cond <- function(X, estimated_k_means,
                                                      all_T_clusters,
                                    all_T_centroids,
                                    n, diff_means_feat, v_vec,v_norm, T_length, k,
                                    feat, sigma_2_feat){

  scaledSigRow <- rep(0,ncol(X))
  scaledSigRow[feat] <- sigma_2_feat
  scaledSigRow_2_norm <- sigma_2_feat
  final_interval <- intervals::Intervals(c(-Inf,Inf))
  # keep track of all the intervals
  all_interval_lists <- list()
  # loop through the initialization
  init_list <- estimated_k_means$random_init_obs
  init_cluster <- all_T_clusters[1,]
  # look at covariance matrices -- a different S needed to be computed
  for (i in c(1:n)){
    current_j_prime <- init_cluster[i]
    j_star_quad <- norm_sq_phi_kmeans_1f(X, v_vec, diff_means_feat, v_norm,
                                         i,init_list[current_j_prime],
                                         feat, scaledSigRow, scaledSigRow_2_norm)
    for (j in c(1:length(init_list))){
      current_j_quad <- norm_sq_phi_kmeans_1f(X, v_vec, diff_means_feat, v_norm,
                                              i,init_list[j],
                                              feat, scaledSigRow, scaledSigRow_2_norm)

      curr_quad <- minus_quad_ineq(j_star_quad, current_j_quad)
      curr_interval <- solve_one_ineq_complement_1f(curr_quad$quad,
                                                    curr_quad$linear,
                                                    curr_quad$constant)
      all_interval_lists[[(i-1)*length(init_list)+j]] <- curr_interval
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
      weighted_v_i_all_k <- rep(0, times=k)
      for (j in c(1:k)){
        # v_vec
        n_k <- sum(last_cl==j) # sum for n_k
        indicator_vec <- rep(0, times=length(last_cl))
        indicator_vec[last_cl==j] <- 1
        weighted_v_i_all_k[j] <- sum(indicator_vec*v_vec)/n_k
      }

      # loop through all the observations
      for (i in c(1:n)){
        # loop through all cluster classes
        current_cl_i <- current_cl[i]
        #i is the observation
        k_star_quad <- norm_phi_canonical_kmeans_1f(X, last_centroids, diff_means_feat,
                                                    v_norm, last_cl, current_cl_i,
                                                    v_vec, i, weighted_v_i_all_k,
                                                    feat, scaledSigRow, scaledSigRow_2_norm)

        for (j in c(1:k)){
          #i is the observation
          # save one class - we know it's the real line!
          if(j!=current_cl_i){
            k_current_quad <- norm_phi_canonical_kmeans_1f(X, last_centroids, diff_means_feat,
                                                           v_norm, last_cl, j,
                                                           v_vec, i, weighted_v_i_all_k,
                                                           feat, scaledSigRow, scaledSigRow_2_norm)

            curr_quad <- minus_quad_ineq(k_star_quad, k_current_quad)
            curr_interval <- solve_one_ineq_complement_1f(curr_quad$quad,
                                                          curr_quad$linear,
                                                          curr_quad$constant)
            # interval update
            all_interval_lists[[curr_len+curr_counter]] <- curr_interval
            curr_counter <- curr_counter + 1
          }
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

  return(final_interval_chisq)

}

#' Compute set S for the general covariance case
#' @keywords internal
#' @export
#' @param XTv, vector p by 1
#' @param v_norm, 2-norm of vector v
#' @param cl, factor vector n by 1 (most recent cluster assignment)
#' @param k, cluster of interest
#' @param v, contrast vector n by 1
#' @param i, index of observation
#' @param feat, integer, the index of the feature involved in the test
#' @param scaledSigRow a vector p by 1 containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @param scaledSigRow_2_norm ell_2 norm of the vector scaledSigRow
#'
#' @return Returns an "Intervals" object containing the conditioning set.
kmeans_compute_S_1f_genCov_less_cond  <-
  function(X, estimated_k_means, all_T_clusters,
           all_T_centroids,n, diff_means_feat,
           mean_feat, v_vec, a_vec, v_norm, a_norm, B,
           T_length, k, feat, scaledSigRow, scaledSigRow_2_norm,
           UPsi){

  final_interval <- intervals::Intervals(c(-Inf,Inf))
  # keep track of all the intervals
  all_interval_lists <- list()
  # loop through the initialization
  init_list <- estimated_k_means$random_init_obs
  init_cluster <- all_T_clusters[1,]
  # look at covariance matrices -- a different S needed to be computed
  for (i in c(1:n)){
    current_j_prime <- init_cluster[i]
    j_star_quad <- norm_sq_phi_kmeans_1f_less_cond(X=X, v=v_vec,
                                                   a=a_vec, B=B,
                                                   mean_feat=mean_feat,
                                                   diff_means_feat=diff_means_feat,
                                                   feat=feat,
                                                   v_norm=v_norm,
                                                   a_norm=a_norm,
                                                   i = i,
                                                   j=init_list[current_j_prime],
                                                   scaledSigRow=scaledSigRow,
                                                   scaledSigRow_2_norm=scaledSigRow_2_norm,
                                                   UPsi=UPsi)
    for (j in c(1:length(init_list))){
      current_j_quad <- norm_sq_phi_kmeans_1f_less_cond(X=X, v=v_vec,
                                            a=a_vec, B=B,
                                            mean_feat=mean_feat,
                                            diff_means_feat=diff_means_feat,
                                            feat=feat,
                                            v_norm=v_norm,
                                            a_norm=a_norm,
                                            i = i,
                                            j=init_list[j],
                                            scaledSigRow=scaledSigRow,
                                            scaledSigRow_2_norm=scaledSigRow_2_norm,
                                            UPsi=UPsi)
      curr_quad <- minus_quad_ineq(j_star_quad, current_j_quad)
      curr_interval <- solve_one_ineq_complement_1f(curr_quad$quad,
                                                    curr_quad$linear,
                                                    curr_quad$constant)
      all_interval_lists[[(i-1)*length(init_list)+j]] <- curr_interval
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
      # this is just the weighted version for x_i
      last_centroids <- all_T_centroids[[(l+1)]] # k by q matrix

      weighted_v_i_all_k <- rep(0, times=k)
      weighted_a <- rep(0, times=k)
      weighted_xj <- rep(0, times=k)
      # we need to store this in a k by n matrix
      # using list for conceptual simplicity for now
      weighted_B <- vector('list', length=k)
      weighted_UPsi <- rep(0, times=k)
      for (j in c(1:k)){
        # this
        n_k <- sum(last_cl==j) # sum for n_k
        indicator_vec <- rep(0, times=length(last_cl))
        indicator_vec[last_cl==j] <- 1
        # compute the weighted v
        weighted_v_i_all_k[j] <- sum(indicator_vec*v_vec)/n_k
        weighted_a[j] <- sum(indicator_vec*a_vec)/n_k
        # X[,feat] is the feature vector
        weighted_xj[j] <- sum(indicator_vec*X[,feat])/n_k
        # check dimension for B
        weighted_B[[j]] <- indicator_vec%*%B/n_k # this should help
        # silent errors are the worst lolol
        #sum(indicator_vec*X[,feat])/n_k
        # dimension should match up
        weighted_UPsi[j] <- sum(indicator_vec*UPsi)/n_k
        # compute the weighted v
      }

      # loop through all the observations
      for (i in c(1:n)){
        # loop through all cluster classes
        current_cl_i <- current_cl[i]
        #i is the observation
        # current_cl_i
        k_star_quad <- norm_phi_canonical_kmeans_1f_less_cond(X=X,
                                                last_centroids=last_centroids,
                                                diff_means_feat=diff_means_feat,
                                                v_norm=v_norm,
                                                cl=last_cl,
                                                k=current_cl_i,
                                                v_vec,
                                                i=i,
                                                weighted_v_i=weighted_v_i_all_k,
                                                feat=feat,
                                                mean_feat=mean_feat,
                                                scaledSigRow=scaledSigRow,
                                                scaledSigRow_2_norm=scaledSigRow_2_norm,
                                                UPsi=UPsi,
                                                B=B,
                                                a=a_vec,
                                                a_norm=a_norm,
                                                weighted_UPsi=weighted_UPsi,
                                                weighted_B=weighted_B,
                                                weighted_xj=weighted_xj,
                                                weighted_a=weighted_a)

        for (j in c(1:k)){
          # i is the observation
          # save one class - we know it's the real line!
          if(j!=current_cl_i){

            k_current_quad <- norm_phi_canonical_kmeans_1f_less_cond(X=X,
                                                 last_centroids=last_centroids,
                                                 diff_means_feat=diff_means_feat,
                                                 v_norm=v_norm,
                                                 cl=last_cl,
                                                 k=j,
                                                 v_vec,
                                                 i=i,
                                                 weighted_v_i=weighted_v_i_all_k,
                                                 feat=feat,
                                                 mean_feat=mean_feat,
                                                 scaledSigRow=scaledSigRow,
                                                 scaledSigRow_2_norm=scaledSigRow_2_norm,
                                                 UPsi=UPsi,
                                                 B=B,
                                                 a=a_vec,
                                                 a_norm=a_norm,
                                                 weighted_UPsi=weighted_UPsi,
                                                 weighted_B=weighted_B,
                                                 weighted_xj=weighted_xj,
                                                 weighted_a=weighted_a)

            curr_quad <- minus_quad_ineq(k_star_quad, k_current_quad)
            curr_interval <- solve_one_ineq_complement_1f(curr_quad$quad,
                                                          curr_quad$linear,
                                                          curr_quad$constant)
            # interval update
            all_interval_lists[[curr_len+curr_counter]] <- curr_interval
            curr_counter <- curr_counter + 1
          }
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
  return(final_interval_chisq)

}
