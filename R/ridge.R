#' RIDGE estimates of linear coefficients for given lambda
#'
#' @param formula A formula specifying the coefficients to use in the model.
#' @param data A tibble or data frame containing the data of interest.
#' @param lambda The value of the penalty parameter to use. Default to `0.01`.
#'
#' @return A `dusty` object, which is a list with elements: `lambda`, `beta` and `formula`.
#' @export
#'
#' @examples
#'
#' fit <- dusty(Sepal.Width ~ ., iris)
#' tidy(fit)
#'
dusty <- function(formula, data, lambda = 0.01) {

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(mf, data)
  y <- stats::model.response(mf)

  beta <- solve(crossprod(x) + lambda * diag(ncol(x))) %*% crossprod(x, y)

  fit <- list(
    beta = beta,
    lambda = lambda,
    formula = formula
  )

  class(fit) <- "dusty"
  fit
}

#' Tidy a dusty object
#'
#' @param x A `dusty` object.
#' @param ... Unused, included for generic consistency only.
#'
#' @export
#' @inherit dusty examples
tidy.dusty <- function(x, ...) {
  ret <- as.data.frame(x$beta)
  ret <- tibble::rownames_to_column(ret)
  names(ret) <- c("term", "estimate")
  tibble::as_tibble(ret)
}
