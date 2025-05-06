#' Calculate Financial Literacy Score
#'
#' This function calculates a financial literacy (FL) score based on seven survey responses.
#' It counts a response of 98 ("Don't know") as incorrect, and excludes 99 ("Prefer not to say")
#' from both numerator and denominator in percent scoring.
#'
#' @param r1 Response to M6 (correct answer = 1)
#' @param r2 Response to M7 (correct answer = 3)
#' @param r3 Response to M8 (correct answer = 2)
#' @param r4 Response to M31 (correct answer = 2)
#' @param r5 Response to M50 (correct answer = 1)
#' @param r6 Response to M9 (correct answer = 1)
#' @param r7 Response to M10 (correct answer = 2)
#'
#' @return A named list with:
#' \describe{
#'   \item{raw_score}{Count of correct answers (0–7)}
#'   \item{percent_score}{Percent correct based on non-99 responses (0–100). NA if all are 99s.}
#' }
#'
#' @examples
#' get_fl_score(1, 3, 2, 2, 98, 1, 2)
#' get_fl_score(99, 99, 2, 2, 1, 1, 99)

get_fl_score <- function(r1, r2, r3, r4, r5, r6, r7) {
  
  responses <- c(r1, r2, r3, r4, r5, r6, r7)
  
  correct_answers <- c(1, 3, 2, 2, 1, 1, 2)
  
  valid_idx <- which(responses != 99)
  
  if (length(valid_idx) == 0) {
    
    return(list(raw_score = NA_integer_, percent_score = NA_real_))
    
  }
  
  valid_responses <- responses[valid_idx]
  
  valid_correct <- correct_answers[valid_idx]
  
  raw_score <- sum(valid_responses == valid_correct, na.rm = TRUE)
  
  percent_score <- round(100 * raw_score / length(valid_responses), 1)
  
  return(list(raw_score = raw_score, percent_score = percent_score))
  
}
