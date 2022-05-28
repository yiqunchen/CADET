
#' Represent ||x'(phi)_i-x'(phi)_j||_2^2 as a quadratic function of phi
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
#'
#' @return parameters: a, b, c the coefficients of the quadratic equation
#' such that (ax^2 + bx + c <= 0)
#'
norm_sq_phi_kmeans_1f <- function(X, v, diff_means_feat, v_norm, i, j,
                                  feat, scaledSigRow, scaledSigRow_2_norm){
  # avoid repeated computation
  # scaledSigRow_2_norm <- sum(scaledSigRow^2)
  # compute quadratic coef

  quad_coef <- (v[i]-v[j])^2/(v_norm)^4*(scaledSigRow_2_norm)^2
  linear_coef <- 2*((((v[i]-v[j])/v_norm^2)*(X[i,]-X[j,])%*%scaledSigRow) -
                      ((v[i]-v[j])/v_norm^2)^2*diff_means_feat*scaledSigRow_2_norm)
  constant_vec <- X[i,]-X[j,]-(v[i]-v[j])*diff_means_feat/(v_norm^2)*scaledSigRow
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
#'
norm_phi_canonical_kmeans_1f <- function(X, last_centroids, diff_means_feat, v_norm,
                                         cl, k, v, i, weighted_v_i, feat, scaledSigRow, scaledSigRow_2_norm){
  # compute quad coef
  v_i_expression <- (v[i]-weighted_v_i[k])/(v_norm^2)
  #X_current <- X[indicator_location,]
  x_i_expression <- X[i,] - last_centroids[k,]
  # n_k and class k
  quad_coef <- (v_i_expression*scaledSigRow_2_norm)^2 # updated
  # compute lienar coef
  linear_coef_part_1 <- v_i_expression*(x_i_expression%*%scaledSigRow) # updated
  linear_coef_part_2 <- (v_i_expression)^2*diff_means_feat*(scaledSigRow_2_norm)^2
  linear_coef <- 2*(linear_coef_part_1-linear_coef_part_2)

  constant_vec <- x_i_expression - c(v_i_expression)*diff_means_feat*scaledSigRow
  constant_coef <- sum(constant_vec*constant_vec)

  coef_list <- list("quad" = as.numeric(quad_coef), "linear" = as.numeric(linear_coef),
                    "constant"= as.numeric(constant_coef))
  return(coef_list)
}

#' Compute set S for the isotropic case
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
kmeans_compute_S_1f_iso <- function(X, estimated_k_means, all_T_clusters,
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
      curr_interval <- solve_one_ineq_complement(curr_quad$quad,
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
kmeans_compute_S_1f_genCov <- function(X, estimated_k_means, all_T_clusters,
                                    all_T_centroids,n, diff_means_feat,
                                    v_vec,v_norm, T_length, k,
                                    feat, scaledSigRow, scaledSigRow_2_norm){

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
      curr_interval <- solve_one_ineq_complement(curr_quad$quad,
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
