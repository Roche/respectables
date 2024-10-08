% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_recipe.R
\name{table_spec}
\alias{table_spec}
\title{Construct a table specification from one or more recipes}
\usage{
table_spec(data_rec, scaffold_rec = NULL, missing_rec = NULL)
}
\arguments{
\item{data_rec}{tibble. A recipe for the data of the table}

\item{scaffold_rec}{tibble or NULL. A scaffolding join recipe.}

\item{missing_rec}{tibble or NULL. A recipe for injecting missingness}
}
\value{
An object representing the collection of recipes corresponding
this single table. Currently a named list.
}
\description{
Construct a table specification from one or more recipes
}
