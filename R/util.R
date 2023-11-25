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
  first_side <- stats::pnorm(abs(z), mean =  mean, sd=sd, lower.tail = F)
  second_side <- stats::pnorm(-1*abs(z), mean =  mean, sd=sd,lower.tail = T)
  two_sided_p_val <- first_side+second_side
  return(two_sided_p_val)
}


#' Summarize the inferential result for k-means clustering
#' @param object output from running kmeans_inference
#' @param ... to be passed to methods
#' @return A data frame with summarized results
#' @export
#' @examples
#' library(CADET)
#' library(ggplot2)
#' set.seed(2022)
#' n <- 150
#' true_clusters <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' delta <- 10
#' q <- 2
#' mu <- rbind(c(delta/2,rep(0,q-1)),
#' c(rep(0,q-1), sqrt(3)*delta/2),
#' c(-delta/2,rep(0,q-1)) )
#' sig <- 1
#' # Generate a matrix normal sample
#' X <- matrix(rnorm(n*q, sd=sig), n, q) + mu[true_clusters, ]
#' # Visualize the data
#' ggplot(data.frame(X), aes(x=X1, y=X2)) +
#' geom_point(cex=2) + xlab("Feature 1") + ylab("Feature 2") +
#'  theme_classic(base_size=18) + theme(legend.position="none") +
#'  scale_colour_manual(values=c("dodgerblue3", "rosybrown", "orange")) +
#'  theme(legend.title = element_blank(),
#'  plot.title = element_text(hjust = 0.5))
#'  k <- 3
#'  # Run k-means clustering with K=3
#'  estimated_clusters <- kmeans_estimation(X, k,iter.max = 20,seed = 2023)$final_cluster
#'  table(true_clusters,estimated_clusters)
#'  # Visualize the clusters
#'  ggplot(data.frame(X), aes(x=X1, y=X2, col=as.factor(estimated_clusters))) +
#'  geom_point(cex=2) + xlab("Feature 1") + ylab("Feature 2") +
#'  theme_classic(base_size=18) + theme(legend.position="none") +
#'  scale_colour_manual(values=c("dodgerblue3", "rosybrown", "orange")) +
#'  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
#' # Let's test the difference between first feature across estimated clusters 1 and 2:
#' cl_1_2_feat_1 <-  kmeans_inference_1f(X, k=3, 1, 2,
#'                                      feat=1, iso=TRUE,
#'                                      sig=sig,
#'                                      covMat=NULL, seed=2023,
#'                                      iter.max = 30)
#' summary(cl_1_2_feat_1)
summary.kmeans_inference <- function(object, ...){
  result <- data.frame(cluster_1 = object$cluster_1,
                       cluster_2 = object$cluster_2,
                       test_stat = object$test_stat,
                       p_selective = object$pval,
                       p_naive = object$p_naive)
  return(result)
}




#' Summarize the inferential result for hierarhical clustering
#' @param object output from running `test_hier_clusters_exact_1f`
#' @param ... to be passed to methods
#' @return A data frame with summarized results
#' @export
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
#' cl_1_2_feat_1 <- test_hier_clusters_exact_1f(X=dat, link="average", hcl=hcl, K=3, k1=1, k2=2, feat=1)
#' summary(cl_1_2_feat_1)
summary.hier_inference <- function(object, ...){
  result <- data.frame(cluster_1 = object$cluster_1,
                       cluster_2 = object$cluster_2,
                       test_stat = object$stat,
                       p_selective = object$pval,
                       p_naive = object$p_naive)
  return(result)
}

