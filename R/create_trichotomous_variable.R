
#' Create a trichotomous variable from multiple binary variables
#'
#' This function evaluates a set of input variables for each row and assigns:
#' - 1 if any variable is `1` (Yes)
#' - 0 if all are `2` (No)
#' - 2 if all values are missing, 98, or 99
#'
#' @param data A data frame or tibble.
#' 
#' @param new_var_name A string. The name of the new variable to create.
#' 
#' @param ... Unquoted variable names to evaluate.
#'
#' @return The original data frame with the new trichotomous variable added.
#'
#' @examples
#' df <- tibble(M21_1 = c(1, 2, NA, 98, NA),
#'              M21_2 = c(NA, 2, 1, 99, NA),
#'              M21_3 = c(NA, 2, NA, 99, NA))
#'
#' #' df <- tibble(
#'   M21_1 = c(1, 2, NA, 98, NA),
#'   M21_2 = c(NA, 2, 1, 99, NA),
#'   M21_3 = c(NA, 2, NA, 99, NA)
#' )
#'
#' df <- create_trichotomous_variable(df, "ReceivedFinEd", M21_1, M21_2, M21_3)
#'
#' @import dplyr, rlang, purrr
#' 
#' @export

create_trichotomous_variable <- function(data, new_var_name, ...) {
  
  vars <- enquos(...)
  
  data %>%
    mutate(!!sym(new_var_name) := pmap_int(select(., !!!vars), function(...) {
      
          responses <- c(...)
          
          informative <- responses[!is.na(responses) & !(responses %in% c(98, 99))]
          
          if (length(informative) == 0) {
            
            return(2)  # Unknown
            
          } else if (any(informative == 1)) {
            
            return(1)  # Yes
            
          } else {
            
            return(0)  # No
            
          }
    }))
  
}
