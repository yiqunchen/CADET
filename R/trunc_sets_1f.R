# ----- functions for explicitly computing truncation sets S (single feature case) -----

# ----- functions for solving quadratic inequalities -----
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

  # now we know A !=0 & disc > 0
  negB2A <- -B/(2*A)
  rtDisc2A <- sqrt(disc)/(2*A)
  # two real roots s.t. root1 < root2
  roots <- sort(c(negB2A - rtDisc2A, negB2A + rtDisc2A))

  if (A > tol) { # parabola open upwards
    return(roots)
  }

  # now we know A < 0 & disc > 0
  # parabola open downwards
  return(c(-Inf, roots[1], roots[2], Inf))
}

# ----- single feature (isotropic covariance mat) -----
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

# ----- single feature (non-isotropic covariance mat) -----
#' Computes the conditioning set S for single linkage hierarchical clustering,
#' when testing for a difference in means wrt a single feature. This is for the
#' dependent features case
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#' @param scaledSigRow a vector containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_single_1f_gencov <- function(X, hcl, K, k1, k2, feat, scaledSigRow) {
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
  dist_Sig <- sum(scaledSigRow^2)
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
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      new_intervals <- solve_one_ineq_1f(dist_Sig,
                                         2*(cross_diff_ij - diff_means*dist_Sig),
                                         orig_dist[i, j] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij - h)

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  # solve inequalities involving i in cluster k1 and j not in clusters k1 or k2, and save the results
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2,
                                         2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig),
                                         orig_dist[i, j] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij - h)

      if(!is.null(new_intervals)) {
        S_complement[[list_index]] <- new_intervals
        list_index <- list_index + 1
      }
    }
  }

  # solve inequalities involving i in cluster k2 and j not in clusters k1 or k2, and save the results
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1,
                                         2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig),
                                         orig_dist[i, j] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij - h)

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
#' when testing for a difference in means wrt a single feature. This is for the
#' dependent features case
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#' @param scaledSigRow a vector containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_average_1f_gencov <- function(X, hcl, K, k1, k2, feat, scaledSigRow) {
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
  dist_Sig <- sum(scaledSigRow^2)
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[i, j] <- C[i, j] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[j, i] <- C[j, i] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
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
        new_intervals <- solve_one_ineq_1f(dist_Sig, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, i], C[j, i] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
#' when testing for a difference in means wrt a single feature. This is for the
#' dependent features case
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#' @param scaledSigRow a vector containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_centroid_1f_gencov <- function(X, hcl, K, k1, k2, feat, scaledSigRow) {
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
  dist_Sig <- sum(scaledSigRow^2)
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[i, j] <- C[i, j] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[j, i] <- C[j, i] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
      } else {
        B[j, i] <-  2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
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
        new_intervals <- solve_one_ineq_1f(dist_Sig, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, i], C[j, i] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
#' when testing for a difference in means wrt a single feature. This is for the
#' dependent features case
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#' @param scaledSigRow a vector containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_ward_1f_gencov <- function(X, hcl, K, k1, k2, feat, scaledSigRow) {
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
  dist_Sig <- sum(scaledSigRow^2)
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        A[i, j] <- dist_Sig
        B[i, j] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[i, j] <- C[i, j] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      } else {
        A[j, i] <- dist_Sig
        B[j, i] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[j, i] <- C[j, i] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        A[i, j] <- dist_Sig*squared_prop_k2
        B[i, j] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      } else {
        A[j, i] <- dist_Sig*squared_prop_k2
        B[j, i] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        A[i, j] <- dist_Sig*squared_prop_k1
        B[i, j] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
      } else {
        A[j, i] <- dist_Sig*squared_prop_k1
        B[j, i] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
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
        new_intervals <- solve_one_ineq_1f(A[i, j], B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(A[j, i], B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, i], C[j, i] - current_height)
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

#' Computes the conditioning set S for cQuitty linkage hierarchical clustering (WGPMA),
#' when testing for a difference in means wrt a single feature. This is for the
#' dependent features case
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#' @param scaledSigRow a vector containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_mcquitty_1f_gencov <- function(X, hcl, K, k1, k2, feat, scaledSigRow) {
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
  dist_Sig <- sum(scaledSigRow^2)
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[i, j] <- C[i, j] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[j, i] <- C[j, i] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
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
        new_intervals <- solve_one_ineq_1f(dist_Sig, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, i], C[j, i] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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

#' Computes the conditioning set S for median linkage hierarchical clustering,
#' when testing for a difference in means wrt a single feature. This is for the
#' dependent features case
#'
#' @param X the n x q data set
#' @param hcl hclust object obtained by clustering X
#' @param K number of clusters
#' @param k1 the index of first cluster involved in the test
#' @param k2 the index of second cluster involved in the test
#' @param feat the index of the feature involved in the test
#' @param scaledSigRow a vector containing the (feat)th row of covariance
#'        matrix Sigma divided by Sigma_{feat, feat}
#' @keywords internal
#'
#' @return Returns an "Intervals" object containing the conditioning set.
compute_S_median_1f_gencov <- function(X, hcl, K, k1, k2, feat, scaledSigRow) {
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
  dist_Sig <- sum(scaledSigRow^2)
  prop_k2 <- length(k2_obs)/(length(k1_obs) + length(k2_obs))
  squared_prop_k2 <- prop_k2^2
  prop_k1 <- prop_k2 - 1
  squared_prop_k1 <- prop_k1^2
  diff_means <- mean(X[k1_obs, feat]) - mean(X[k2_obs, feat])
  squared_diff_means <- diff_means^2

  # compute coefficients involving i in cluster k1 and i' in cluster k2
  for(i in k1_obs) {
    for(j in k2_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[i, j] <- C[i, j] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*(cross_diff_ij - diff_means*dist_Sig)
        C[j, i] <- C[j, i] + dist_Sig*squared_diff_means - 2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k1 and j not in clusters k1 or k2
  for(i in k1_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k2*(cross_diff_ij - prop_k2*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k2*squared_diff_means*dist_Sig - 2*prop_k2*diff_means*cross_diff_ij
      }
    }
  }

  # compute coefficients involving i in cluster k2 and j not in clusters k1 or k2
  for(i in k2_obs) {
    for(j in other_obs) {
      diff_ij <- X[i, ] - X[j, ]
      cross_diff_ij <- sum(diff_ij*scaledSigRow)

      if(i > j) {
        B[i, j] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[i, j] <- C[i, j] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
      } else {
        B[j, i] <- 2*prop_k1*(cross_diff_ij - prop_k1*diff_means*dist_Sig)
        C[j, i] <- C[j, i] + squared_prop_k1*squared_diff_means*dist_Sig - 2*prop_k1*diff_means*cross_diff_ij
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
        new_intervals <- solve_one_ineq(dist_Sig, B[i, j], C[i, j] - current_height) # hard-coded first quadratic coef ... proof via Lemma S1 + algebra
      } else {
        new_intervals <- solve_one_ineq(dist_Sig, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq(dist_Sig*squared_prop_k2, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(dist_Sig*squared_prop_k2, B[j, i], C[j, i] - current_height)
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
        new_intervals <- solve_one_ineq(dist_Sig*squared_prop_k1, B[i, j], C[i, j] - current_height)
      } else {
        new_intervals <- solve_one_ineq(dist_Sig*squared_prop_k1, B[j, i], C[j, i] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k2, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[min_cluster_1, j], C[min_cluster_1, j] - current_height)
        } else {
          new_intervals <- solve_one_ineq_1f(dist_Sig*squared_prop_k1, B[j, min_cluster_1], C[j, min_cluster_1] - current_height)
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
