#' Compute Weighted Statistics for Ordinal or Binary Survey Data
#'
#' Calculates weighted mean, variance, standard deviation, and coefficient of variation.
#' For binary variables, includes entropy and Gini impurity.
#'
#' @param df A data frame containing two columns: response values and their corresponding counts.
#' @param response_col A string naming the column with response values (numeric, ordinal, or binary).
#' @param count_col A string naming the column with counts (frequencies) for each response value.
#' @param binary Logical; set to TRUE if the variable is binary (e.g., coded as 0/1, 1/2, etc).
#' @param missing_values A vector of numeric values (e.g., c(98, 99)) to exclude from calculations.
#'
#' @return A named list with:
#' \describe{
#'   \item{weighted_mean}{The weighted mean of the responses.}
#'   \item{weighted_variance}{The weighted variance. For binary, uses p(1 - p).}
#'   \item{weighted_sd}{The weighted standard deviation.}
#'   \item{coefficient_of_variation}{Standard deviation divided by the mean. NA if mean is 0.}
#'   \item{gini_impurity}{Only for binary; a measure of class impurity.}
#'   \item{entropy}{Only for binary; a measure of uncertainty in the distribution.}
#' }
#'
#' @export
compute_weighted_stats <- function(df, response_col, count_col, binary = FALSE, missing_values = NULL) {
  response <- df[[response_col]]
  counts <- df[[count_col]]
  
  # Exclude missing codes
  if (!is.null(missing_values)) {
    valid_idx <- !response %in% missing_values
    response <- response[valid_idx]
    counts <- counts[valid_idx]
  }
  
  extra_metrics <- list()
  
  if (binary) {
    unique_vals <- sort(unique(response))
    if (length(unique_vals) != 2) {
      stop("Binary variable must contain exactly two unique values after excluding missing values.")
    }
    response_norm <- ifelse(response == unique_vals[1], 0, 1)
    p <- sum(response_norm * counts) / sum(counts)
    
    w_mean <- p
    w_var <- p * (1 - p)
    entropy <- ifelse(p == 0 || p == 1, 0, -p * log2(p) - (1 - p) * log2(1 - p))
    gini <- 2 * p * (1 - p)
    
    extra_metrics$entropy <- entropy
    extra_metrics$gini_impurity <- gini
  } else {
    w_mean <- sum(response * counts) / sum(counts)
    w_var <- sum(counts * (response - w_mean)^2) / sum(counts)
  }
  
  w_sd <- sqrt(w_var)
  cv <- ifelse(w_mean != 0, w_sd / w_mean, NA)
  
  return(c(
    list(
      weighted_mean = w_mean,
      weighted_variance = w_var,
      weighted_sd = w_sd,
      coefficient_of_variation = cv
    ),
    extra_metrics
  ))
}

