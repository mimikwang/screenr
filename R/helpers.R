.check_inputs <- function(df, responses, factors) {
  # Check for correct input types
  if (class(df) != 'data.frame') {
    stop("df not of type data.frame")
  } else if (class(responses) != 'character') {
    stop("responses not of type character")
  } else if ((class(factors) != 'character') & (!is.null(factors))) {
    stop("factors not of type character or NULL")
  }

  if (is.null(factors)) {
    col_select <- sapply(colnames(df), function(x) { is.numeric(df[x][[1]]) })
    factors    <- colnames(df)[col_select]
    factors    <- factors[!(factors %in% response)]
  }

  # Check responses and factors in columns of df
  if (!all(responses %in% names(df))) {
    stop("responses not in columns of df")
  } else if (!all(factors %in% names(df))) {
    stop("factors not in columns of df")
  }

  # Get rid of duplicates
  responses <- unique(responses)
  factors   <- unique(factors)

  # Return
  return(list(df = df, responses = responses, factors = factors))
}
