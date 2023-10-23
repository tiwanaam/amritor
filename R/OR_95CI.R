#' @title Calculate odds ratios and 95% confidence intervals
#' @param coef Coefficient estimates.
#' @param se Standard errors.
#' @param siglevel Significance level (e.g., 0.05 for a 95% confidence interval).
#' @param roundto Number of decimal places to round the results to.
#' @return A character vector with odds ratios and their 95% confidence intervals.
#' @author Amrit Tiwana
#' @examples
#' model1 <- glm(y ~ x1 + x2, family = binomial("logit"), data = toydata)
#' model1coef <- summary(model1)$coef
#' OR_95CI(model1coef[,1], model1coef[,2], 0.05, 2)
#' @export
OR_95CI <- function(coef, se, siglevel, roundto) {
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(
    format(round(OR, roundto), nsmall = roundto),
    " (",
    format(round(ORlcl, roundto), nsmall = roundto),
    ", ",
    format(round(ORucl, roundto), nsmall = roundto),
    ")"
  )
  return(ORresult)
}

