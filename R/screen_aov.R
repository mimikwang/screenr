#' @title
#' screen_aov
#'
#' @description
#' Use ANOVA to screen variables to factors
#'
#' @details
#' \code{screen_aov} uses \code{aov} to screen responses against factors one to one
#'
#' @param df, data.frame, a \code{data.frame} with factors and responses as columns
#' @param responses, character, a vector of characters corresponding to column names
#' in \code{df} to be used as responses
#' @param factors, character, a vector of characters corresponding to column names
#' in \code{df} to be used as factors
#'
#' @return
#' A \code{data.frame} with the following columns
#'
#' @export
screen_aov <- function(df, responses, factors) {
  # Check for correct input types
  if (class(df) != 'data.frame') {
    stop("df not of type data.frame")
  } else if (class(responses) != 'character') {
    stop("responses not of type character")
  } else if (class(factors) != 'character') {
    stop("factors not of type character")
  }

  # Check responses and factors in columns of df
  if (!(responses %in% names(df))) {
    stop("responses not in columns of df")
  } else if (!(factors %in% names(df))) {
    stop("factors not in columns of df")
  }


}
