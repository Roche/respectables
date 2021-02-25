#' Function constructor for injecting independent missing-at-random NAs to columns
#' @param p numeric(1). Proportion of observations to change to missing
#' @param nvars numeric(1). Number of columns this behavior should be applied to.
#' @return a function suitable for use in missingness recipe, which when given \code{.df}, the data
#' will return an N x nvars logical matrix.
#' @export
miss_at_random <- function(p, nvars) {
    function(.df) {
        matrix(sample(c(TRUE, FALSE), nrow(.df)*nvars,
                      replace = TRUE, prob = c(p, 1-p)), ncol = nvars)
    }

}

#' Function constructor for injecting "missing together" NAs to columns
#'
#' This constructor creates a function which will set all \code{nvars} columns
#' to NA together for rows selected for missing data.
#'
#' @inheritParams miss_at_random
#' @inherit miss_at_random return
#' @export
miss_as_block <- function(p, nvars) {
    function(.df) {
        col <- sample(c(TRUE, FALSE), nrow(.df),
                      replace = TRUE, prob = c(p, 1-p))
        do.call(cbind, rep(list(col), nvars))
    }
}
