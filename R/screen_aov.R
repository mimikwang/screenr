#' @title
#' screen_aov
#'
#' @description
#' Use ANOVA to screen numeric variables to numeric factors
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
#' A list with the results data frame in \code{results} and inputs in \code{input}.
#'
#' @examples
#' out <- screen_aov(mtcars, c("cyl", "disp"), c("wt"))
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
  if (!all(responses %in% names(df))) {
    stop("responses not in columns of df")
  } else if (!all(factors %in% names(df))) {
    stop("factors not in columns of df")
  }

  # Get rid of duplicates
  responses <- unique(responses)
  factors   <- unique(factors)

  # Loop over responses and factors
  df_out <- lapply(responses, function(res) {
    out <- lapply(factors, function(fac) {
      # Truncate
      df_temp <- df[,c(res, fac)]
      df_temp <- na.omit(df_temp)

      # If nrow < 1
      if (nrow(df_temp) < 1) {
        N                       <- NA
        Pval                    <- NA
        `SS Residual`           <- NA
        `SS Factor`             <- NA
        `Response Mean`         <- NA
        `Response Sd`           <- NA
        `Response Shapiro Pval` <- NA
        `Factor Mean`           <- NA
        `Factor Sd`             <- NA
        `Factor Shapiro Pval`   <- NA
      } else {
        # Model
        mod <- aov(as.formula(paste(res, fac, sep = "~")), data = df_temp)

        # Shapiro Test
        tryCatch({
          `Response Shapiro Pval` <- shapiro.test(df_temp[, res])$p.value
        }, error = function(e) {
          `Response Shapiro Pval` <- NA
        })

        tryCatch({
          `Factor Shapiro Pval` <- shapiro.test(df_temp[, fac])$p.value
        }, error = function(e) {
          `Factor Shapiro Pval` <- NA
        })

        # Summary
        N                       <- nrow(df_temp)
        Pval                    <- unlist(summary(mod))["Pr(>F)1"]
        `SS Residual`           <- sum(mod$residuals^2)
        `SS Factor`             <- sum(mod$effects[2]^2)
        `Response Mean`         <- mean(df_temp[, res])
        `Response Sd`           <- sd(df_temp[, res])
        `Factor Mean`           <- mean(df_temp[, fac])
        `Factor Sd`             <- sd(df_temp[, fac])
      }

      # Error Catching
      Pval <- if (is.na(Pval)) NA else Pval

      out <- data.frame(
        Response                = res,
        Factor                  = fac,
        N                       = N,
        Pval                    = Pval,
        `SS Residual`           = `SS Residual`,
        `SS Factor`             = `SS Factor`,
        `Response Mean`         = `Response Mean`,
        `Response Sd`           = `Response Sd`,
        `Response Shapiro Pval` = `Response Shapiro Pval`,
        `Factor Mean`           = `Factor Mean`,
        `Factor Sd`             = `Factor Sd`,
        `Factor Shapiro Pval`   = `Factor Shapiro Pval`,
        stringsAsFactors = FALSE)

      row.names(out) <- NULL

      return(out)
    })
    out <- do.call(rbind, out)
  })

  df_out     <- do.call(rbind, df_out)
  out        <- list(
    results = df_out,
    input   = list(df = df, responses = responses, factors = factors))

  class(out) <- c("screen_aov")
  return(out)
}

#' @method print screen_aov
#' @export
print.screen_aov <- function(x, ...) {
  print(x$results, ...)
}
