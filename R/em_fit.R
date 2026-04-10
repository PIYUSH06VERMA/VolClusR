#' Latent Regime Discovery via Expectation-Maximization
#'
#' @description Fits a Gaussian Mixture Model (GMM) to reduced principal
#' components to identify hidden market states (e.g., "Quiet/Mean-Reverting" vs.
#' "Volatile/Crisis") using the Expectation-Maximization (EM) algorithm.
#'
#' @details This function treats market regimes as latent (unobserved) variables.
#' Unlike "hard" clustering methods like K-means, GMM provides a "soft"
#' probabilistic assignment. It assumes the data is generated from a mixture of
#' several multivariate normal distributions, each characterized by its own
#' mean vector \eqn{\mu} and covariance matrix \eqn{\Sigma}.
#'
#' The EM process involves two recurring steps:
#' \enumerate{
#'   \item \strong{Expectation (E-step):} Calculates the posterior probability
#'   (responsibility) that each data point belongs to a specific regime.
#'   \item \strong{Maximization (M-step):} Updates the parameters (\eqn{\mu, \Sigma, \pi})
#'   to maximize the expected log-likelihood found in the E-step.
#' }
#'
#' If a range is provided for \code{centers}, the \code{mclust} engine
#' automatically selects the optimal number of regimes and covariance structure
#' (e.g., spherical vs. ellipsoidal) by maximizing the **Bayesian Information
#' Criterion (BIC)**, effectively penalizing over-fitting.
#'
#' @param pca_data A \code{data.frame} containing a 'Date' column and the numeric
#' orthogonal components from \code{\link{reduce_features_pca}}.
#' @param centers Integer vector. The number of clusters (G) to evaluate.
#' If a vector is provided (e.g., \code{2:5}), the model with the highest BIC
#' score is automatically selected. (Default is 2:5).
#'
#' @return The input \code{data.frame} augmented with two diagnostic columns:
#' \itemize{
#'   \item \code{Regime}: A factor indicating the most likely state assignment
#'   for each observation.
#'   \item \code{Uncertainty}: The probability of misclassification (\eqn{1 - \max(z)}).
#'   High values indicate "transition zones" where the market is shifting
#'   between states.
#' }
#' @examples
#' # Simulate PCA data with two distinct clusters (Quiet vs. Volatile states)
#' set.seed(101)
#' n_points <- 100
#' dates <- seq(as.Date("2024-01-01"), by = "day", length.out = n_points * 2)
#'
#' # Cluster 1: Low variance, centered at -2 (Quiet Market)
#' cluster_1 <- matrix(rnorm(n_points * 2, mean = -2, sd = 0.7), ncol = 2)
#'
#' # Cluster 2: High variance, centered at 2 (Volatile Market)
#' cluster_2 <- matrix(rnorm(n_points * 2, mean = 2, sd = 1.5), ncol = 2)
#'
#' # Combine into a mock PCA dataframe
#' pc_matrix <- rbind(cluster_1, cluster_2)
#' mock_pca <- data.frame(
#'   Date = dates,
#'   PC1  = pc_matrix[,1],
#'   PC2  = pc_matrix[,2]
#' )
#'
#' # Run the EM algorithm
#' # We test for 2 to 4 regimes;
#' regime_results <- em_fit(mock_pca, centers = 2:4)
#'
#' # Inspect classification and average uncertainty
#' table(regime_results$Regime)
#' mean(regime_results$Uncertainty)
#'
#'
#'
#' @seealso \code{\link[mclust]{Mclust}} for the underlying EM implementation
#' and BIC-based model selection.
#'
#'
#'
#' @export
#' @import mclust
em_fit <- function(pca_data, centers = 2:5) {

  # 1. Safety Check for Date
  if (!"Date" %in% colnames(pca_data)) {
    stop("Error: The input data must contain a 'Date' column.")
  }

  # 2. Isolate the Principal Components (Drop Date for the math)
  features <- pca_data[, !(colnames(pca_data) %in% c("Date"))]

  # 3. Fit the GMM using the EM algorithm
  gmm_model <- Mclust(features, G = centers, verbose = FALSE)

  # 4. Convergence Check
  if (is.null(gmm_model)) {
    stop("Error: The EM algorithm failed to converge. Check your input data.")
  }

  # 5. Extract Classification and Uncertainty
  # We convert classification to a factor because regimes are categorical (e.g., Regime 1 or 2)
  pca_data$Regime <- as.factor(gmm_model$classification)
  pca_data$Uncertainty <- gmm_model$uncertainty

  return(pca_data)
}
