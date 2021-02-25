

#' Convenience helper functions for defining relational-join recipes
#'
#' These function constructors create functions suitable for use in
#' relational-join recipes that expand or contract the row-dimension
#' of the incoming data.
#'
#' \code{rep_per_key} creates functions  which generate dimension-scaffolds
#' that contain a constant number of  rows per key value (ie row of
#' the incoming data), e.g., the the map from ADSL requires 3 rows per
#' patient  (foreign key)  to synthesize  the long-form  PARAMCD-based
#' ADTTE data.
#'
#' \code{rand_per_key} creates functions which generate dimension-scaffolds
#' which contain a uniformly distributed random number of rows
#' per key value. An example of this would be that
#' for adverse events, a patient can have anywehre from 0
#' to 20 adverse events, each of which is a separate row
#' in the new dimensions.
#'
#' @param keyvar character(1).  The name of the column to treat as a foreign key.
#' @param count numeric(1). The number of times each foreign-key value should appear in the
#' scaffold data.
#' @param prop_present numeric(1). Proportion of the key values in the
#'     foreign    table     to    include    rows    for     in    the
#'     dimension-scaffold. Defaults to 1 (all values present).
#' @rdname reljoin_funcs
#' @aliases reljoin_funcs
#' @export
rep_per_key <- function(keyvar, count, prop_present = 1) {
    function(n, .dbtab, .df) {
        keys <- unique(.dbtab[[keyvar]])

        if(prop_present < 1) {
            rnd <- runif(length(keys))
            keep <- rnd <= prop_present
            keys <- keys[keep]
        }
        newdf <- setNames(data.frame(rep(keys, rep(count, length(keys)))), keyvar)
        merge(.dbtab, newdf, by = keyvar,
              all.x = FALSE, all.y = TRUE)
    }
}

#' @param mincount numeric(1). Minimum replications for a present key
#' @param maxcount numeric(1). Maximum replications for a pressent key
#' @rdname reljoin_funcs
#' @examples
#' foreign_tbl <- data.frame(id = 1:5)
#' perkey_fun <- rep_per_key("id", 2, .6)
#' perkey_fun(.dbtab = foreign_tbl)
#'
#' randrep_fun <- rand_per_key("id", mincount = 1, maxcount = 5)
#' randrep_fun(.dbtab = foreign_tbl)
#' @export
rand_per_key <- function(keyvar, mincount = 1, maxcount = 20, prop_present = .5) {
    function(n, .dbtab, .df) {
        keys <- unique(.dbtab[[keyvar]])

        if(prop_present < 1) {
            rnd <- runif(length(keys))
            keep <- rnd <= prop_present
            keys <- keys[keep]
        }
        newdf <- setNames(data.frame(rep(keys, sample(mincount:maxcount, length(keys), replace = TRUE))),
                         keyvar)
        merge(.dbtab, newdf, by = keyvar,
              all.x = FALSE, all.y = TRUE)
    }
}
