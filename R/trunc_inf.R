# ----- functions to test for a difference in means between a single feature -----
#' Exact significance test (with respect to a single feature) for hierarchical clustering
#'
#' This tests the null hypothesis of no difference in means in the mean of
#' feature \code{feat} between clusters \code{k1} and \code{k2} at
#' level \code{K} in a hierarchical clustering. (The \code{K} clusters are
#' numbered as per the results of the \code{cutree} function in the
#' \code{stats} package.)
#'
#' In order to account for the fact that the clusters have been estimated from the data,
#' the p-values are computed conditional on the fact that those clusters were estimated.
#' This function computes p-values exactly via an analytic characterization of the conditioning set.
#'
#' Currently, this function supports squared Euclidean distance as a measure of dissimilarity
#' between observations, and the following six linkages: single, average, centroid, Ward,
#' McQuitty (also known as WPGMA), and median (also kown as WPGMC).
#'
#' By default, this function assumes that the features are independent. If known,
#' the variance of feature \code{feat} (\eqn{\sigma}) can be passed in using the
#' \code{sigma} argument; otherwise, an estimate of \eqn{\sigma} will be used.
#'
#' Setting \code{ind} to \code{FALSE} allows the features to be dependent, i.e.
#' \eqn{Cov(X_i) = \Sigma}. If known, \eqn{\Sigma} can be passed in using the \code{covMat} argument;
#' otherwise, an estimate of \eqn{\Sigma} will be used.
#'
#' @export
#'
#' @param X \eqn{n} by \eqn{p} matrix containing numeric data.
#' @param link String selecting the linkage. Supported options are \code{"single", "average", "centroid", "ward.D", "median"}, and \code{"mcquitty"}.
#' @param hcl Object of the type \code{hclust} containing the hierarchical clustering of X.
#' @param K Integer selecting the total number of clusters.
#' @param k1,k2 Integers selecting the clusters to test.
#' @param feat Integer selecting the feature to test.
#' @param indpt Boolean. If \code{TRUE}, assume independent features, otherwise not.
#' @param sig Optional scalar specifying \eqn{\sigma}, relevant if \code{ind} is \code{TRUE}.
#' @param covMat Optional matrix specifying \eqn{\Sigma}, relevant if \code{ind} is \code{FALSE}.
#'
#' @return
#' \item{stat}{the test statistic: the absolute difference between the mean of feature \code{feat} in cluster \code{k1} and the mean of feature \code{feat} in cluster \code{k2}}
#' \item{pval}{the p-value}
#' \item{trunc}{object of the type \code{Intervals} containing the conditioning set}
#'
#' @examples
#' # Simulates a 100 x 2 data set with three clusters
#' set.seed(123)
#' dat <- rbind(c(-1, 0), c(0, sqrt(3)), c(1, 0))[rep(1:3, length=100), ] +
#' matrix(0.2*rnorm(200), 100, 2)
#'
#' # Average linkage hierarchical clustering
#' hcl <- hclust(dist(dat, method="euclidean")^2, method="average")
#'
#' # plot dendrograms with the 1st and 2nd clusters (cut at the third split)
#' # displayed in blue and orange
#' plot(hcl)
#' rect_hier_clusters(hcl, k=3, which=1:2, border=c("blue", "orange"))
#'
#' # tests for a difference in means between the blue and orange clusters
#' # with respect to the 1st feature
#' test_hier_clusters_exact_1f(X=dat, link="average", hcl=hcl, K=3, k1=1, k2=2, feat=1)
#'
#' @seealso \code{\link{rect_hier_clusters}} for visualizing clusters \code{k1} and \code{k2} in the dendrogram;
#'
#' \code{\link{test_complete_hier_clusters_approx_1f}} for approximate p-values for complete linkage hierarchical clustering;
#'
#' \code{\link{test_clusters_approx_1f}} for approximate p-values for a user-specified clustering function;
#'
#' \code{\link{test_hier_clusters_exact}} for exact p-values for a difference in the mean of any feature.
#'
#' @references Lucy L. Gao et al. "Selective inference for hierarchical clustering". arXiv preprint (2020).
test_hier_clusters_exact_1f <- function(X, link, hcl, K, k1, k2, feat, indpt=TRUE, sig=NULL, covMat=NULL) {
  if(!is.matrix(X)) stop("X should be a matrix")

  n <- nrow(X)
  q <- ncol(X)

  if(link == "complete") stop("Exact p-value not supported. See 'test_complete_hier_clusters_approx' for an approximate p-value.")
  if(!link %in% c("single", "average", "centroid", "ward.D", "mcquitty", "median")) stop("Linkage should be 'single', 'average', 'centroid', 'ward.D', 'mcquitty', or 'median'")
  if(!is_integer_between_a_b(K, 2, n)) stop("number of clusters (K) should be between 2 and n")
  if(!is_integer_between_a_b(k1, 1, K) | !is_integer_between_a_b(k2, 1, K)) stop(paste("cluster indices should be between 1 and K", sep=""))

  hcl_at_K <- stats::cutree(hcl, K)

  n1 <- sum(hcl_at_K == k1)
  n2 <- sum(hcl_at_K == k2)
  squared_norm_nu <- 1/n1 + 1/n2

  # compute test statistic
  stat <- mean(X[hcl_at_K == k1, feat]) - mean(X[hcl_at_K == k2, feat])


  if(indpt) {
    if(is.null(sig)) {
      sig <- sqrt(sum(scale(X, scale=FALSE)^2)/(n*q - q))
    }

    # compute truncation set
    if(link == "single") S <- compute_S_single_1f(X, hcl, K, k1, k2, feat)
    if(link == "average") S <- compute_S_average_1f(X, hcl, K, k1, k2, feat)
    if(link == "centroid") S <- compute_S_centroid_1f(X, hcl, K, k1, k2, feat)
    if(link == "ward.D") S <- compute_S_ward_1f(X, hcl, K, k1, k2, feat)
    if(link == "mcquitty") S <- compute_S_mcquitty_1f(X, hcl, K, k1, k2, feat)
    if(link == "median") S <- compute_S_median_1f(X, hcl, K, k1, k2, feat)

    # set distribution of phi
    scale_factor <- squared_norm_nu*sig^2
  } else {
    if(is.null(covMat)) {
      covMat <- stats::cov(scale(X, scale=F))
    }

    sig_squared <- covMat[feat, feat]
    scaledSigRow <- covMat[feat, ]/sig_squared

    if(link == "single") S <- compute_S_single_1f_gencov(X, hcl, K, k1, k2, feat, scaledSigRow)
    if(link == "average") S <- compute_S_average_1f_gencov(X, hcl, K, k1, k2, feat, scaledSigRow)
    if(link == "centroid") S <- compute_S_centroid_1f_gencov(X, hcl, K, k1, k2, feat, scaledSigRow)
    if(link == "ward.D") S <- compute_S_ward_1f_gencov(X, hcl, K, k1, k2, feat, scaledSigRow)
    if(link == "mcquitty") S <- compute_S_mcquitty_1f_gencov(X, hcl, K, k1, k2, feat, scaledSigRow)
    if(link == "median") S <- compute_S_median_1f_gencov(X, hcl, K, k1, k2, feat, scaledSigRow)

    # set distribution of phi
    scale_factor <- squared_norm_nu*sig_squared
  }

  # compute p-value using truncated normal distribution
  if(stat > 0) {
    pval <-  TNSurv(stat, 0, sqrt(scale_factor), S) +
             TNSurv(stat, 0, sqrt(scale_factor), intervals::Intervals(as.matrix(-S)[, 2:1]))
  } else {
    pval <-  TNSurv(-stat, 0, sqrt(scale_factor), S) +
             TNSurv(-stat, 0, sqrt(scale_factor), intervals::Intervals(as.matrix(-S)[, 2:1]))
  }

  return(list(stat=abs(stat), pval=pval, trunc=S))
}

#' Monte Carlo significance test (with respect to a single feature) for complete linkage hierarchical clustering
#'
#' This tests the null hypothesis of no difference in means in the mean of
#' feature \code{feat} between clusters \code{k1} and \code{k2} at
#' level \code{K} in a complete linkage hierarchical clustering. (The \code{K}
#'clusters are  numbered as per the results of the \code{cutree} function in the
#' \code{stats} package.)
#'
#' Important note: Before calling \code{hclust} and this function, make sure to
#' load the package \code{fastcluster}. This is because the p-value approximation
#' procedure requires running hierarchical clustering on a large number of simulated
#' data sets, and the version of \code{hclust} in the \code{fastcluster} package
#' is much faster than the version of \code{hclust} in \code{stats}.
#'
#' In order to account for the fact that the clusters have been estimated from the data,
#' the p-values are computed conditional on the fact that those clusters were estimated.
#' This function approximates p-values via importance sampling.
#'
#' Currently, this function supports squared Euclidean distance as a measure of dissimilarity
#' between observations. (Note that complete linkage is invariant under monotone transformations
#' of the measure of dissimilarity between observations, so unsquared Euclidean distance
#' would produce the same hierarchical clustering.)
#'
#' This function assumes that the covariance matrix of the features is isotropic
#' i.e. \eqn{Cov(X_i) = \sigma^2 I_p}. If known, \eqn{\sigma} can be passed in using the \code{sigma}
#' argument; otherwise, an estimate of \eqn{\sigma} will be used.
#'
#' @export
#'
#' @param X \eqn{n} by \eqn{p} matrix containing numeric data.
#' @param hcl Object of the type \code{hclust} containing the hierarchical clustering of X.
#' @param K Integer selecting the total number of clusters.
#' @param k1,k2 Integers selecting the clusters to test.
#' @param feat Integer selecting the feature to test.
#' @param sig Optional scalar specifying \eqn{\sigma}.
#' @param ndraws Integer selecting the number of importance samples, default of 2000.
#'
#' @return
#' \item{stat}{the test statistic: the absolute difference between the mean of feature \code{feat} in cluster \code{k1} and the mean of feature \code{feat} in cluster \code{k2}}
#' \item{pval}{the approximate p-value}
#' \item{stderr}{standard error of the p-value estimate}
#'
#' @examples
#' # Simulates a 100 x 2 data set with no clusters
#' set.seed(1)
#' dat <-  matrix(rnorm(200), 100, 2)
#'
#' # Complete linkage hierarchical clustering
#' library(fastcluster)
#' hcl <- hclust(dist(dat, method="euclidean")^2, method="complete")
#'
#' # plot dendrograms with the 1st and 2nd clusters (cut at the third split)
#' # displayed in blue and orange
#' plot(hcl)
#' rect_hier_clusters(hcl, k=3, which=1:2, border=c("blue", "orange"))
#'
#' # Monte Carlo test for a difference in means between the blue and orange clusters
#' # wrt the 2nd feature
#' test_complete_hier_clusters_approx_1f(X=dat, hcl=hcl,
#' K=3, k1=1, k2=2, feat=2, ndraws=1000)
#'
#' @seealso \code{\link{rect_hier_clusters}} for visualizing clusters \code{k1} and \code{k2} in the dendrogram;
#'
#' \code{\link{test_hier_clusters_exact_1f}} for exact p-values for other linkages;
#'
#' \code{\link{test_clusters_approx_1f}} for approximate p-values for a user-specified clustering function;
#'
#' \code{\link{test_complete_hier_clusters_approx}} for approximate p-values for a difference in the mean of any feature.
#'
#' @references Lucy L. Gao et al. "Selective inference for hierarchical clustering". arXiv preprint (2020).
test_complete_hier_clusters_approx_1f <- function(X, hcl, K, k1, k2, feat, sig=NULL, ndraws=2000) {
  if(!("fastcluster" %in% .packages())) stop("The fastcluster package must be loaded before calling hclust and before calling this function!")

  if(!is.matrix(X)) stop("X should be a matrix")

  n <- nrow(X)
  q <- ncol(X)

  if(!is_integer_between_a_b(K, 2, n)) stop("number of clusters (K) should be between 2 and n")
  if(!is_integer_between_a_b(k1, 1, K) | !is_integer_between_a_b(k2, 1, K)) stop(paste("cluster indices should be between 1 and K", sep=""))

  hcl_at_K <- stats::cutree(hcl, K)

  n1 <- sum(hcl_at_K == k1)
  n2 <- sum(hcl_at_K == k2)
  squared_norm_nu <- 1/n1 + 1/n2
  prop_k2 <- n2/(n1+n2)

  if(is.null(sig)) {
    sig <- sqrt(sum(scale(X, scale=FALSE)^2)/(n*q - q))
  }

  scale_factor <- sqrt(sig^2*squared_norm_nu)

  # compute test statistic
  stat <- mean(X[hcl_at_K == k1, feat]) - mean(X[hcl_at_K == k2, feat])

  log_survives <- rep(NA, ndraws)
  phi <- stats::rnorm(ndraws)*scale_factor + c(stat, -stat)[sample(1:2, ndraws, replace=T)]

  orig_k1 <- X[hcl_at_K == k1, feat]
  orig_k2 <- X[hcl_at_K == k2, feat]

  Xphi <- X
  for(j in 1:ndraws) {
    # Compute perturbed data set
    Xphi[hcl_at_K == k1, feat] <- orig_k1 + prop_k2*(phi[j] - stat)
    Xphi[hcl_at_K == k2, feat] <- orig_k2 + (prop_k2 - 1)*(phi[j] - stat)

    # Recluster the perturbed data set
    hcl_Xphi <- fastcluster::hclust(stats::dist(Xphi)^2, method="complete")
    clusters_Xphi <- stats::cutree(hcl_Xphi, K)
    if(same_cl(hcl_at_K, clusters_Xphi, K)) {
      log_mix <- c(stats::dnorm(phi[j], mean=-stat, sd=scale_factor, log=TRUE),
                   stats::dnorm(phi[j], mean=stat, sd=scale_factor, log=TRUE))
      log_mix_shift <- max(log_mix)
      log_survives[j] <- stats::dnorm(phi[j], mean=0, sd=scale_factor, log=TRUE) -
        (log(0.5) + log_mix_shift + log(sum(exp(log_mix - log_mix_shift))))
    }
  }


  # Trim down to only survives
  phi <- phi[!is.na(log_survives)]
  log_survives <- log_survives[!is.na(log_survives)]

  survives <- length(log_survives)

  # Return nothing if nothing survives
  if(survives == 0) {
    warning("Oops - we didn't generate any samples that preserved the clusters! Try re-running with a larger value of ndraws.")
    return(list(stat=abs(stat), pval=NA, stderr=NA))
  }

  #  Approximate p-values
  log_survives_shift <- log_survives - max(log_survives)
  props <- exp(log_survives_shift)/sum(exp(log_survives_shift))
  pval <-  sum(props[phi >= abs(stat) | phi <= -abs(stat)])
  var_pval <- (1 - pval)^2*sum(props[phi >= abs(stat) | phi <= -abs(stat)]^2) +
    pval^2*sum(props[abs(phi) < abs(stat)]^2)

  return(list(stat=abs(stat), pval=pval, stderr=sqrt(var_pval)))
}

#'  Monte Carlo significance test (with respect to a single feature) for any clustering function
#'
#' This function performs a user-specified clustering method \code{cl_fun} on the rows of a
#' data matrix to obtain \code{K} clusters, and tests the null hypothesis of no difference
#' between the mean of feature \code{feat} in clusters \code{k1} and \code{k2}.
#'
#' In order to account for the fact that the clusters have been estimated from the data,
#' the p-values are computed conditional on the fact that those clusters were estimated.
#' This function approximates p-values via importance sampling.
#'
#' This function assumes that \code{cl_fun} takes a \eqn{n \times p} numeric data matrix as input
#' and outputs integer assignments to clusters 1 through \code{K}.
#'
#' @export
#'
#' @param X \eqn{n} by \eqn{p} matrix containing numeric data.
#' @param k1,k2 Integers selecting the clusters to test.
#' @param feat Integer selecting the feature to test.
#' @param sig Optional scalar specifying \eqn{\sigma}.
#' @param ndraws Integer selecting the number of importance samples, default of 2000.
#' @param cl_fun Function returning assignments to clusters 1 through \code{K}.
#' @param cl Optionally pass in the results of calling \code{cl_fun} on your data. This is for
#' efficiency and reproducibility (when the clustering function is non-deterministic).
#'
#' @return
#' \item{stat}{the test statistic: the Euclidean distance between the mean of cluster \code{k1} and the mean of cluster \code{k2}  }
#' \item{pval}{the p-value}
#' \item{clusters}{the estimated cluster assignments}
#'
#' @examples
#' # Simulates a 100 x 2 data set with three clusters
#' set.seed(123)
#' dat <- rbind(c(-1, 0), c(0, sqrt(3)), c(1, 0))[rep(1:3, length=100), ] +
#' matrix(0.2*rnorm(200), 100, 2)
#'
#' # Function to run k-means clustering w/ k = 3 and 50 random starts
#' km_cluster <- function(X) {
#'  km <- kmeans(X, 3, nstart=50)
#'  return(km$cluster)
#' }
#'
#' # Cluster data using k-means
#' clusters <- km_cluster(dat)
#' table(rep(1:3, length=100), clusters)
#'
#' # tests for a difference in means between clusters 1 and 2
#' results <- test_clusters_approx_1f(dat, k1=1, k2=2, feat = 1, cl_fun=km_cluster,
#' ndraws=500, cl=clusters)
#' results$stat
#' results$pval
#' results$stderr
#'
#' @seealso \code{\link{test_clusters_approx}} for approximate p-values for a difference in the mean of any feature.
#'
#' @references Lucy L. Gao et al. "Selective inference for hierarchical clustering". arXiv preprint (2020).
test_clusters_approx_1f <- function(X, k1, k2, feat, sig=NULL, ndraws=2000, cl_fun, cl=NULL) {
  if(!is.matrix(X)) stop("X should be a matrix")

  n <- nrow(X)
  q <- ncol(X)

  if(is.null(cl)) cl <- cl_fun(X)
  K <- length(unique(cl))

  if(!is_integer_between_a_b(K, 2, n)) stop("number of clusters (K) should be between 2 and n")
  if(!is_integer_between_a_b(k1, 1, K) | !is_integer_between_a_b(k2, 1, K)) stop(paste("cluster indices should be between 1 and K", sep=""))

  n1 <- sum(cl == k1)
  n2 <- sum(cl == k2)
  squared_norm_nu <- 1/n1 + 1/n2
  prop_k2 <- n2/(n1+n2)

  if(is.null(sig)) {
    sig <- sqrt(sum(scale(X, scale=FALSE)^2)/(n*q - q))
  }

  scale_factor <- sqrt(sig^2*squared_norm_nu)

  # compute test statistic
  stat <- mean(X[cl == k1, feat]) - mean(X[cl == k2, feat])

  log_survives <- rep(NA, ndraws)
  phi <- stats::rnorm(ndraws)*scale_factor + c(stat, -stat)[sample(1:2, ndraws, replace=TRUE)]

  Xphi <- X
  orig_k1 <- X[cl == k1, feat]
  orig_k2 <- X[cl == k2, feat]

  for(j in 1:ndraws) {
    # Compute perturbed data set
    Xphi[cl == k1, feat] <- orig_k1 + prop_k2*(phi[j] - stat)
    Xphi[cl == k2, feat] <- orig_k2 + (prop_k2 - 1)*(phi[j] - stat)

    # Recluster the perturbed data set
    clusters_Xphi <- cl_fun(Xphi)
    if(preserve_cl(cl, clusters_Xphi, k1, k2)) {
      log_mix <- c(stats::dnorm(phi[j], mean=-stat, sd=scale_factor, log=TRUE),
                   stats::dnorm(phi[j], mean=stat, sd=scale_factor, log=TRUE))
      log_mix_shift <- max(log_mix)
      log_survives[j] <- stats::dnorm(phi[j], mean=0, sd=scale_factor, log=TRUE) -
        (log(0.5) + log_mix_shift + log(sum(exp(log_mix - log_mix_shift))))
    }
  }

  # Trim down to only survives
  phi <- phi[!is.na(log_survives)]
  log_survives <- log_survives[!is.na(log_survives)]

  survives <- length(log_survives)

  # Return nothing if nothing survives
  if(survives == 0) {
    warning("Oops - we didn't generate any samples that preserved the clusters! Try re-running with a larger value of ndraws.")
    return(list(stat=abs(stat), pval=NA, stderr=NA, clusters=cl))
  }

  #  Approximate p-values
  log_survives_shift <- log_survives - max(log_survives)
  props <- exp(log_survives_shift)/sum(exp(log_survives_shift))
  pval <-  sum(props[phi >= abs(stat) | phi <= -abs(stat)])

  var_pval <- (1 - pval)^2*sum(props[phi >= abs(stat) | phi <= -abs(stat)]^2) +
    pval^2*sum(props[abs(phi) < abs(stat)]^2)

  return(list(stat=abs(stat), pval=pval, stderr=sqrt(var_pval), clusters=cl))
}
