#' @title
#' screen_lm
#'
#' @description
#' Use lm to screen numeric variables to numeric factors
#'
#' @details
#' \code{screen_lm} uses \code{lm} to screen responses against factors one to one
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
#' out <- screen_lm(mtcars, c("cyl", "disp"), c("wt"))
#'
#' @importFrom stats as.formula coef lm na.omit sd
#' @export
screen_lm<- function(df, responses, factors) {
  # Checks
  checks    <- .check_inputs(df, responses, factors)
  df        <- checks$df
  responses <- checks$responses
  factors   <- checks$factors

  # Loop over responses and factors
  df_out <- lapply(responses, function(res) {
    out <- lapply(factors, function(fac) {
      # Truncate
      df_temp <- df[,c(res, fac)]
      df_temp <- na.omit(df_temp)

      # If nrow < 1
      if (nrow(df_temp) < 1) {
        N                <- NA
        Rsquared         <- NA
        Intercept        <- NA
        Slope            <- NA
        `Response Mean`  <- NA
        `Response Sd`    <- NA
        `Factor Mean`    <- NA
        `Factor Sd`      <- NA
      } else {
        # Model
        mod <- lm(as.formula(paste(res, fac, sep = "~")), data = df_temp)

        # Summary
        N                <- nrow(df_temp)
        Rsquared         <- summary(mod)$r.squared
        Intercept        <- coef(mod)[1][[1]]
        Slope            <- coef(mod)[2][[1]]
        `Response Mean`  <- mean(df_temp[, res])
        `Response Sd`    <- sd(df_temp[, res])
        `Factor Mean`    <- mean(df_temp[, fac])
        `Factor Sd`      <- sd(df_temp[, fac])
      }

      out <- data.frame(
        Response        = res,
        Factor          = fac,
        N               = N,
        Rsquared        = Rsquared,
        Intercept       = Intercept,
        Slope           = Slope,
        `Response Mean` = `Response Mean`,
        `Response Sd`   = `Response Sd`,
        `Factor Mean`   = `Factor Mean`,
        `Factor Sd`     = `Factor Sd`,
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

  class(out) <- c("screen_lm")
  return(out)
}

#' @method print screen_lm
#' @export
print.screen_lm <- function(x, ...) {
  print(x$results, ...)
}
