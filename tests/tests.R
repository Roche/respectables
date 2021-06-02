# library(tinytest)
# require(tibble)
# library(respectables)
# noop_fun = respectables:::noop_func_constr("INDEP")
# badrec1 <- tibble(variables = "MISSINGDEP", dependencies = "MISSING", func = list(function(n, .df, ...) rep("", n)), func_args = list(NULL))
# expect_error(validate_recipe_deps(badrec1), "No independent recipe components found")
# badrec2 <- tribble(~variables, ~dependencies, ~func, ~func_args,
#                    "INDEP", no_deps, noop_fun, NULL,
#                    "CDEP1", "CDEP2", noop_fun, NULL,
#                    "CDEP2", "CDEP1", noop_fun, NULL)
#
# expect_error(validate_recipe_deps(badrec2), "Unable to generate some dependent variables: CDEP1, CDEP2")
#
# badrec3 <- tibble(variables = list("REAL", character()), list(noop_fun, noop_fun), list(NULL, NULL))
# s
# badrec4 <- rbind(badrec3[1,], badrec3[1,])
# expect_error(validate_recipe_deps(badrec4), "Repeated variable")
#
# goodrec <- tribble(~variables, ~dependencies, ~func, ~func_args, ~keep,
#                    "VAR1", no_deps, seq_n, NULL, TRUE)
#
# goodrec2 <- tribble(~variables, ~dependencies, ~func, ~func_args, ~keep,
#                    "VAR1", no_deps, seq_n, NULL, TRUE)
#
# tribble(~variables, ~dependencies, ~func, ~func_args, ~keep)
#
# result_id <- subjid_func(5, prefix = "p", suffix = "x")[4]
# expect_id <- "p-4-x"
# expect_identical(result_id, expect_id)
