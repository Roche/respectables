## Helper Utilities related to generating variables in a single table go here
##
## See also:
## scaffold_join_utils.R - for utilities for defining relationships between tables
## missingness_utils.R - for utilities for defining missingness content/patterns


#' Create a factor with random elements of x
#'
#' Sample elements from x with replacing and build a factor
#'
#' @param x character vector or factor, if character vector then it is also used
#'   as levels of the returned factor, otherwise if it is a factor then the
#'   levels get used as the new levels
#' @param n number of observations to sample.
#' @param ... arguments passed on to \code{\link{sample}}
#'
#' @return a factor of length N
#'
#' @export
#' @rdname samps
#'
#' @examples
#' sample_fct(letters[1:3], 10)
#' sample_fct(iris$Species, 10)
#'
#' rep_n("aaa", 5)
#'
sample_fct <- function(x, n, ...) { # nolint
  stopifnot(is_numeric_single(n))

  factor(sample(x, n, replace = TRUE, ...), levels = if (is.factor(x)) levels(x) else x)
}


#' @export
#' @rdname samps
sample_yn <- function(n) sample_fct(c("Y", "N"), n)


#' @rdname samps
#' @param val ANY. Single value to be repeated n times
#' @export
rep_n <- function(val, n, ...) rep(val, n)

#' @rdname samps
#' @export
seq_n <- function(n, ...) 1:n

#' Generate sequence of "subject id"s
#' @param n numeric(1). number of ids to generate. Values will be padded with leading 0s so all resulting ids have equal width
#' @param prefix character(1). Prefix to prepend to the generated numeric ids. Defaults to \code{"id"}
#' @param suffix character(1). Suffix to append to generated ids. Defautls to \code{NULL} (no suffix).
#' @param sep character(1). String to use as separator when combining \code{prefix}, number, and \code{suffix}.
#' @return sequence from 1 to \code{n}, prepended with \code{prefix}, and appended with \code{suffix}, separated by \code{sep}
#' @examples
#' subjid_func(5)
#' @export
subjid_func <- function(n, prefix = "id", suffix = NULL, sep = "-") {
    ndigits <- ceiling(log(n, 10))
    id <- formatC(seq_len(n), width = ndigits, flag=0)
    if(any(nzchar(prefix)))
        id <- paste(prefix, id, sep = sep)
    if(any(nzchar(suffix)))
        id <- paste(id, suffix, sep = sep)
    id
}


#' @rdname rand_dtm
#' @export
pct_orig <- "1970-01-01"
#' @export
#' @rdname rand_dtm
secs_per_year <- 31556952#
#' @export
#' @rdname rand_dtm
secs_per_day <- 60*60*24

#' Generate a sample of random datetimes
#'
#' Generates random datetimes that are, elementwise, between \code{start} and \code{end}
#' @param start POSIXct or Date. Earliest possible datetime for thte sample
#' @param end POSIXct or Date. latest possible datetime for thte sample
#' @param max_duration_secs numeric. Number of seconds to use to generate alernate end if \code{end} has a missing value.
#' @param multiplier numeric. Used internally.
#' @param n numeric. Length of sample. Default to max of \code{length(start)} and \code{length(end)}.
#' @return A \code{POSIXct} vector of datetimes.
#' @export
#' @rdname rand_dtm
rand_posixct <- function( start, end,
                         max_duration_secs = NULL,
                         multiplier = if(is(start, "Date")) secs_per_day else 1,
                         n = max(length(start), length(end)))
 {
     if(is.character(start))
         start <- as.POSIXct(start)

     if(!any(vapply(class(start), function(x) inherits(end, x), NA)))
         end <- as.POSIXct(end)
     ## assuming this is fixed.
     if(length(max_duration_secs) > 1)
         max_duration_secs <- max_duration_secs[1]

     if(anyNA(end)) {
         inds <- which(is.na(end))
         if(!is.null(max_duration_secs))
             vals <- start[inds] + max_duration_secs/multiplier
         else
             vals <- Sys.time()
         end[inds] <- vals
     }

     ## note floor want last possible value to be ~ as likely as the rest
     ## TECHNICALLY +1 below here could get us outside of our desired range (with probability 0...)
     vals <- floor(runif(n, min = unclass(start), max = unclass(end) + .9999999))

     if(multiplier != 1)
         vals <- vals * multiplier
     as.POSIXct(vals, origin = pct_orig)
 }
