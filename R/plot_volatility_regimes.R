#' Visualize Volatility Regimes
#'
#' @description Generates a comprehensive suite of five quantitative visualizations
#' designed to diagnose and interpret the market regimes identified by the
#' Expectation-Maximization (EM) clustering engine.
#'
#' @details This function merges price, features, and regime data into a unified
#' temporal dataframe to visualize how statistical clusters correlate with
#' real-world market behavior..
#'
#' @param price_data Original dataframe containing at least 'Date' and 'Close'.
#' @param feature_data Output dataframe from \code{\link{extract_rolling_features}}.
#' @param regime_data Output dataframe from \code{\link{em_fit}}.
#'
#' @return A named list of five \code{ggplot2} objects:
#' \describe{
#'   \item{Timeline}{A time-series plot of the Close price where each data point
#'   is colored by its assigned regime, highlighting market phases .}
#'   \item{Density}{An overlapping density plot of log returns for each regime.
#'   Useful for spotting "fat tails" and differences in variance across states.}
#'   \item{Returns}{A volatility clustering plot showing daily log returns over time,
#'   colored by regime to visualize how high/low volatility persists in chunks.}
#'   \item{Transitions}{An empirical transition heatmap (Markov Matrix) showing
#'   the probability of staying in a current regime versus switching to another.}
#'   \item{VolShock}{A high-frequency "spike" plot of EWMA residuals (Z-scores),
#'   indicating sudden market action that deviates from the historical baseline.}
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom stats na.omit
plot_volatility_regimes <- function(price_data, feature_data, regime_data) {

  # 1. Merge pipeline based on Date
  df_merged <- merge(price_data[, c("Date", "Close")], feature_data, by = "Date")
  df_merged <- merge(df_merged, regime_data, by = "Date")

  # Calculate Log Returns on the fly for the new plots
  df_merged$Return <- c(NA, diff(log(df_merged$Close)))

  # Define a robust color palette for up to 5 regimes
  quant_colors <- c(
    "#00BFC4", # Teal
    "#F8766D", # Coral
    "#C77CFF", # Purple
    "#E6AB02", # Gold
    "#66A61E", # Green
    "#FB61D7", # Pink
    "#A3A500", # Olive
    "#00A9FF"  # Blue
  )
  p1 <- ggplot2::ggplot(df_merged, ggplot2::aes(x = Date, y = Close)) +
    ggplot2::geom_line(color = "grey70", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = Regime), size = 1.5) +
    ggplot2::scale_color_manual(values = quant_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Market Price vs. Identified Volatility Regimes",
                  subtitle = "Points represent daily closes colored by EM clustering",
                  x = "Date", y = "Close Price") +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"))

  # ---------------------------------------------------------
  # PLOT 2: Overlapping Density Plot of Returns
  # ---------------------------------------------------------
  p2 <- ggplot2::ggplot(na.omit(df_merged), ggplot2::aes(x = Return, fill = Regime)) +
    ggplot2::geom_density(alpha = 0.5, color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = quant_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Return Distributions by Regime",
                  subtitle = "Wider, flatter curves indicate high variance (fat tails)",
                  x = "Daily Log Return", y = "Density") +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"))

  # ---------------------------------------------------------
  # PLOT 3: The Classic Volatility Clustering Plot
  # ---------------------------------------------------------
  p3 <- ggplot2::ggplot(na.omit(df_merged), ggplot2::aes(x = Date)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_line(ggplot2::aes(y = Return), color = "grey80", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(y = Return, color = Regime), size = 1.5) +
    ggplot2::scale_color_manual(values = quant_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Volatility Clustering Phenomenon",
                  subtitle = "Visualizing how large market fluctuations clump together in specific states",
                  x = "Date", y = "Log Return") +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"))

  # ---------------------------------------------------------
  # PLOT 4: Empirical Transition Heatmap (Markov Stickiness)
  # ---------------------------------------------------------
  # Create a 'Next_Regime' column by shifting the Regime column up by 1
  df_merged$Next_Regime <- c(as.character(df_merged$Regime[-1]), NA)

  # Isolate and clean the transition data
  trans_data <- na.omit(df_merged[, c("Regime", "Next_Regime")])

  # Calculate empirical transition probabilities row-wise
  trans_matrix <- prop.table(table(trans_data$Regime, trans_data$Next_Regime), 1)
  trans_df <- as.data.frame(trans_matrix)
  colnames(trans_df) <- c("Current_Regime", "Next_Regime", "Probability")

  p4 <- ggplot2::ggplot(trans_df, ggplot2::aes(x = Next_Regime, y = Current_Regime, fill = Probability)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Probability)), color = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Regime Transition Matrix",
                  subtitle = "Probability of switching from Current Regime to Next Regime tomorrow",
                  x = "Tomorrow's Regime", y = "Today's Regime") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  plot_df5 <- na.omit(df_merged[, c("Date", "Vol_Shock", "Regime")])
  plot_df5$Regime <- as.factor(plot_df5$Regime)  # ensure discrete fill

  p5 <- ggplot2::ggplot(plot_df5, ggplot2::aes(x = Date, xend = Date,
                                               y = 0, yend = Vol_Shock,
                                               color = Regime)) +
    ggplot2::geom_segment(linewidth = 0.6, alpha = 0.85) +      # <-- spikes, not bars
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    ggplot2::scale_color_manual(values = quant_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = "High-Frequency Volatility Shocks (EWMA Residuals)",
      subtitle = "Spikes indicate market action deviating from historical baseline",
      x = "Date", y = "Scaled Shock (Z-Score)"
    ) +
    ggplot2::theme(
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(face = "bold")
    )

  # Return all plots as a named list
  return(list(
    Timeline = p1,
    Density = p2,
    Returns = p3,
    Transitions = p4,
    VolShock=p5
  ))
}
