
noop_func_constr <- function(vars) {
    function(n, .df, ...) {
        cols <- lapply(vars, function(v) rep("", n))
        if(length(cols) > 1)
            as.data.frame(cols, stringsAsFactors = FALSE, col.names = vars)
        else
            cols[[1]]
    }
}

noop_func <- function(n, .df, ...) ""

## being clever here but I think its ok for now
## XXX TODO do this right later
## covered by tests now so we're safe(ish)
#' Validate recipe for circular dependencies
#'
#' @param recipe tibble. Table recipe.
#' @param seed_df tibble. Seed/pre-existing data.frame or NULL.
#' @return \code{TRUE} if successful, throws an error if not
#' @export
validate_recipe_deps <- function(recipe, seed_df = NULL) {

    badvs1 <- vapply(recipe$variables, function(vs) length(vs) == 0 || !all(nzchar(vs)), NA)
    if(any(badvs1))
        stop("Bad variable specification(s) in rows: ", paste(which(badvs1), collapse = ", "))
    vars <- unlist(recipe$variables)
    dupl <- duplicated(vars)
    if(any(dupl))
        stop("Repeated variable(s) to be generated: ", paste(unique(vars[dupl]), collapse = ", "))
    rec2 <- recipe

    rec2$func <- lapply(rec2$variables, noop_func_constr)
    ## this will throw an error if the dependency structure doesn't work out
    out <- gen_table_data(N = 1, recipe = rec2, df = seed_df)
    TRUE
}

