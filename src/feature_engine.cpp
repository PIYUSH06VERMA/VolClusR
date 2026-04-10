#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calculate_rolling_features_cpp(NumericMatrix data, int window) {

  int n = data.nrow();
  NumericMatrix out(n, 6); // extracting out 6 useful features

  // If the data is somehow shorter than the window, return NAs safely
  if (n < window) {
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < 6; j++) out(i, j) = NA_REAL;
    }
    return out;
  }

  double sum_abs = 0.0, sum_sq_returns = 0.0, sum_ret = 0.0, sum_parkinson = 0.0, sum_volume = 0.0;
  
  // Initialize EWMA with the first day's squared return
  // EWMA: Exponential weighed movibg average
  
  double ewma_var = data(0, 0) * data(0, 0);
  double lambda = 0.94; // JP Morgan RiskMetrics standard decay

  // Initialize sums for the first 'window - 1' days (Warm-up Phase)
  for(int i = 0; i < window - 1; i++) {
    for(int j = 0; j < 6; j++) {
      out(i, j) = NA_REAL; 
    }

    double ret = data(i, 0);
    double high = data(i, 1);
    double low = data(i, 2);
    double vol = data(i, 3);

    sum_abs += std::abs(ret);
    sum_sq_returns += ret * ret;
    sum_ret += ret;
    sum_volume += vol;

    double h_ratio = std::log(high / low);
    sum_parkinson += h_ratio * h_ratio;
    
    // Warm up the EWMA model in the background (skip i=0 since it's the seed)
    if (i > 0) {
      ewma_var = lambda * ewma_var + (1.0 - lambda) * (ret * ret);
    }
  }

  // Sliding window optimization
  for(int i = window - 1; i < n; i++) {

    // Add the new day's data
    double ret = data(i, 0);
    double high = data(i, 1);
    double low = data(i, 2);
    double vol = data(i, 3);

    sum_abs += std::abs(ret);
    sum_sq_returns += ret * ret;
    sum_ret += ret;
    sum_volume += vol;

    double h_ratio = std::log(high / low);
    sum_parkinson += h_ratio * h_ratio;
    
    // Predict today's variance using EWMA
    ewma_var = lambda * ewma_var + (1.0 - lambda) * (ret * ret);

    // Find the rolling minimum
    double min_ret = data(i, 0);
    for(int j = i - window + 1; j <= i; j++) {
      if(data(j, 0) < min_ret) {
        min_ret = data(j, 0);
      }
    }

    // 1. Variance (Realized)
    double variance = (sum_sq_returns - (sum_ret * sum_ret) / window) / (window - 1);
    if (variance < 0) variance = 0; 
    out(i, 0) = variance;

    // 2. Mean Absolute Return
    out(i, 1) = sum_abs / window;

    // 3. Minimum Return
    out(i, 2) = min_ret;

    // 4. Parkinson Volatility
    out(i, 3) = sum_parkinson / (4.0 * std::log(2.0) * window);

    // 5. Relative Volume
    double mean_volume = sum_volume / window;
    out(i, 4) = vol / mean_volume;
    
    // 6. VOLATILITY SHOCK (Realized Variance - Predicted Variance)
    out(i, 5) =variance-ewma_var;

    // Subtract the oldest day's data
    double old_ret = data(i - window + 1, 0);
    double old_high = data(i - window + 1, 1);
    double old_low = data(i - window + 1, 2);
    double old_vol = data(i - window + 1, 3);

    sum_abs -= std::abs(old_ret);
    sum_sq_returns -= old_ret * old_ret;
    sum_ret -= old_ret;
    sum_volume -= old_vol;

    double old_h_ratio = std::log(old_high / old_low);
    sum_parkinson -= old_h_ratio * old_h_ratio;
  }

  return out;
}