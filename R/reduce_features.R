#' Principal Component Dimensionality Reduction
#'
#' @description Performs an orthogonal linear transformation (PCA) on rolling
#' market features to project high-dimensional volatility metrics into a
#' lower-dimensional subspace.
#'

#'
#'
#' Note: The underlying C++ engine standardizes features before this step,
#' so further scaling is disabled to preserve the relative magnitude of
#' volatility shocks.
#'
#' @param data A dataframe containing a 'Date' column and the numeric features
#' returned by \code{\link{extract_rolling_features}}.
#' @param ncomp Integer. The number of principal components to retain.
#' Higher values capture more variance but increase the risk of over-fitting
#' the EM algorithm. (Default is 2).
#'
#' @return A \code{data.frame} containing:
#' \itemize{
#'   \item \code{Date}: The original temporal index.
#'   \item \code{PC1, PC2, ... PCn}: The projected orthogonal components
#'   representing the "latent" volatility states.
#' }
#'
#' @seealso \code{\link[stats]{prcomp}} for the underlying SVD implementation.
#' @examples
#' # Assume 'features' is already generated from extract_rolling_features
#' # Let's simulate that output:
#' set.seed(123)
#' mock_features <- data.frame(
#'   Date = seq(as.Date("2024-01-01"), by = "day", length.out = 50),
#'   Variance = rnorm(50),
#'   Abs_Return = rnorm(50),
#'   Parkinson = rnorm(50),
#'   Vol_Shock = rnorm(50)
#' )
#'
#' # Reduce to 2 Principal Components
#' pca_res <- reduce_features_pca(mock_features, ncomp = 2)
#' plot(pca_res$PC1, pca_res$PC2, main = "Feature Space Projection")
#' head(pca_res)
#'
#' @export
#' @importFrom stats prcomp
reduce_features_pca <- function(data, ncomp = 2) {

  # 1. Safety Check for Date (Moved to the very top)
  if (!"Date" %in% colnames(data)) {
    stop("Error: The input data must contain a 'Date' column")
  }

  # 2. Safety Check for maximum components
  maxcomp <- ncol(data) - 1 # -1 to exclude the Date column
  if (ncomp > maxcomp) {
    warning(paste("Requested more components than total features, defaulting to", maxcomp))
    ncomp <- maxcomp
  }

  # 3. Isolate the mathematical features
  df <- data[, !(colnames(data) %in% c("Date"))]

  # 4. Run PCA
  # Note: center and scale are FALSE because the C++ engine already standardized the features
  pca_res <- prcomp(df, scale. = FALSE, center = FALSE)

  # 5. Extract the projected data
  pc_data <- as.data.frame(pca_res$x[, 1:ncomp])

  # set up column names
  colnames(pc_data) <- paste0("PC", 1:ncomp)

  # 6. Reattach the Date column
  pc_data <- cbind(Date = data$Date, pc_data)

  return(pc_data)
}
