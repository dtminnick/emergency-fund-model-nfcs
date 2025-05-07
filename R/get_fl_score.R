#' Get Financial Literacy Score
#'
#' This function calculates a financial literacy (FL) score based on seven survey responses.
#' It counts a response of 98 ("Don't Know") as incorrect, and excludes 99 ("Prefer Not to Say")
#' from both numerator and denominator in percent scoring.
#' 
#' It also produces a normalized score based on the proportion of valid responses and
#' questions answered, i.e. number of responses not equal to 99.
#' 
#' Note that for accurate results, survey responses must be passed to the function in 
#' the following order: M6, M7, M8, M31, M50, M9, M10.  This is also the order in which the questions
#' are presented in the NFCS survey.
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
#'   \item{normalized_score}{Percent score scaled by the proportion of valid responses.}
#'   \item{questions_answered}{Number of responses not equal to 99 (0–7)}
#' }
#'
#' @examples
#' get_fl_score(1, 3, 2, 2, 1, 1, 2)
#' 
#' Returns:
#'   $raw_score
#'   [1] 7
#'   $percent_score
#'   [1] 100
#'   $normalized_score
#'   [1] 100
#'   $questions_answered
#'   [1] 7
#' 
#' get_fl_score(1, 3, 2, 2, 1, 1, 98)
#' 
#' Returns:
#'   $raw_score
#'   [1] 6
#'   $percent_score
#'   [1] 85.7
#'   $normalized_score
#'   [1] 85.7
#'   $questions_answered
#'   [1] 7
#' 
#' get_fl_score(1, 3, 2, 2, 1, 1, 99)
#' 
#' Returns:
#'   $raw_score
#'   [1] 6
#'   $percent_score
#'   [1] 100
#'   $normalized_score
#'   [1] 85.7
#'   $questions_answered
#'   [1] 6
#' 
#' get_fl_score(1, 3, 2, 2, 1, 99, 99)
#' 
#' Returns:
#'   $raw_score
#'   [1] 5
#'   $percent_score
#'   [1] 100
#'   $normalized_score
#'   [1] 71.4
#'   $questions_answered
#'   [1] 5
#' 

get_fl_score <- function(r1, r2, r3, r4, r5, r6, r7) {
  
    responses <- c(r1, r2, r3, r4, r5, r6, r7)
  
    correct_answers <- c(1, 3, 2, 2, 1, 1, 2)
  
    valid_idx <- which(responses != 99)
  
    questions_answered <- length(valid_idx)
  
    if (questions_answered == 0) {
      
        return(list(
        
            raw_score = NA_integer_,
        
            percent_score = NA_real_,
        
            normalized_score = NA_real_,
        
            questions_answered = 0L
        
        ))
      
    }
  
    valid_responses <- responses[valid_idx]
  
    valid_correct <- correct_answers[valid_idx]
  
    raw_score <- sum(valid_responses == valid_correct, na.rm = TRUE)
  
    percent_score <- round(100 * raw_score / questions_answered, 1)
  
    normalized_score <- round(percent_score * (questions_answered / 7), 1)
    
    return(list(
      
        raw_score = raw_score,
      
        percent_score = percent_score,
      
        normalized_score = normalized_score,
      
        questions_answered = questions_answered
      
    ))
  
}


get_fl_score(1, 3, 2, 2, 1, 1, 2)

get_fl_score(1, 3, 2, 2, 1, 1, 98)

get_fl_score(1, 3, 2, 2, 1, 1, 99)

get_fl_score(1, 3, 2, 2, 1, 99, 99)

