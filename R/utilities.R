is_numeric_single <- function(x) is(x, "numeric") && length(x) == 1L

#' Lookup function from value you recipe column
#'
#' This function is an internal utility exported for its usefulness
#' when debugging recipes. Normal workflows will not involve calling
#' it directly.
#'
#' @param str ANY. A function (immediately returned) or a character(1) value of the form \code{func},
#'   \code{"pkg::func"} or \code{"pkg:::func"}. Any other value will result in an error.
#'
#' @return A function
#'
#' @export
#'
#' @examples
#' lookup_fun(rnorm)
#' lookup_fun("rnorm")
#' lookup_fun("stats::rnorm")
#' lookup_fun("stats:::rnorm")
lookup_fun <- function(str) {
    if(is.function(str))
        return(str)
    if(!is(str, "character") || length(str) != 1)
        stop("Invalid function specification in recipe. Got non-function value that is not a length one character.")
    if(grepl("::", str, fixed=TRUE)) {
        spl <- strsplit(str, ":+")[[1]]
        getExportedValue(spl[1], spl[2])
    } else {
        get(str, mode = "function")
    }
}
