




#' Construct a table specification from one or more recipes
#' @param data_rec tibble. A recipe for the data of the table
#' @param scaffold_rec tibble or NULL. A scaffolding join recipe.
#' @param missing_rec tibble or NULL. A recipe for injecting missingness
#'
#' @return An object representing the collection of recipes corresponding
#' this single table. Currently a named list.
#' @export
#' @import methods
table_spec <- function(data_rec, scaffold_rec = NULL, missing_rec = NULL) {
    list(data = data_rec, scaffold = scaffold_rec, missing = missing_rec)
}




setClass("DBRecipeBook",
         representation(tablespecs = "list",
                        dep_network = "data.frame"))

DBRecipeBook = function(..., specs = list(...)) {

    if(is.null(names(specs)))
        stop("when specifying the specs list directly it must be a named list.")

    nms <- names(specs)
    ftbs <- sapply(specs, function(sp) if(is.null(sp$scaffold)) "" else sp[["scaffold"]][["foreign_tbl"]])
    new("DBRecipeBook", tablespecs = specs, dep_network = data.frame(table = nms, table_dep = ftbs, stringsAsFactors = FALSE))
}

