# VolClusR 
**Quantitative Regime Terminal for Financial Markets**

*An R Package developed for the MTH209 Course Project*

VolClusR is an end-to-end quantitative R package designed to systematically identify and cluster hidden financial market states (regimes) using high-frequency OHLCV data. By combining a highly optimized C++ backend with advanced machine learning clustering, VolClusR allows analysts to stop guessing what the market is doing and start measuring it objectively.

## Key Features

* **High-Performance C++ Engine:** Bypasses standard R bottlenecks using `Rcpp`. Implements an $O(N)$ sliding window algorithm to extract rolling features like Parkinson volatility, maximum drawdowns, and real-time EWMA baseline residuals in milliseconds.
* **Machine Learning Pipeline:** Utilizes Principal Component Analysis (PCA) to resolve ill-conditioned covariance matrices, followed by Expectation-Maximization (EM) to fit Gaussian Mixture Models (GMM) for probabilistic regime classification.
* **Interactive Shiny Dashboard:** A built-in, polished UI (`bslib`) allowing users to upload datasets, adjust engine parameters, and visualize multi-dimensional clustering without writing code.
* **Volatility Shock Detection:** Generates a high-frequency "Now-casting" indicator to flag mathematically significant deviations from historical baselines before standard lagging indicators react.
* **Markov Risk Management:** Computes an empirical Transition Matrix to calculate the exact probability of shifting between market states (e.g., from a calm drift to a highly volatile crash).

---

## 📦 Installation

**Local Installation:**
To install the package from the provided source tarball, run the following command in your R console. Ensure the path points to the location of the `.tar.gz` file on your local machine.

```R
# Install required dependencies if not already installed
install.packages(c("mclust", "ggplot2", "shiny", "bslib", "Rcpp"))

# Install VolClusR from source
install.packages("/path/to/VolClusR_0.1.0.tar.gz", repos = NULL, type = "source")
