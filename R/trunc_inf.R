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
#' library(CADET)
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
#' @references Yiqun T. Chen and Lucy L. Gao "Testing for a difference in means of a single feature after clustering". arXiv preprint (2023).
test_hier_clusters_exact_1f <- structure(function(X, link, hcl, K, k1, k2, feat, indpt=TRUE, sig=NULL, covMat=NULL) {
  if(!is.matrix(X)) stop("X should be a matrix")

  n <- nrow(X)
  q <- ncol(X)

  if(link == "complete") stop("Exact p-value not supported. ")
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

  p_naive <- naive.two.sided.pval(z = stat,
                                  mean = 0,
                                  sd = sqrt(scale_factor))
  result_list <- list("stat"=stat,
                      "cluster_1" = k1,
                      "cluster_2" = k2,
                      "pval"=pval,
                      "p_naive"=p_naive,
                      "trunc"=S,
                      "linkage"=link)
  class(result_list) <- "hier_inference"
  return(result_list)
})
