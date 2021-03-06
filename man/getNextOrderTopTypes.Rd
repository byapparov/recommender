% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complementaryProducts.R
\name{getNextOrderTopTypes}
\alias{getNextOrderTopTypes}
\title{The function that return the n nearest types to the given ones}
\usage{
getNextOrderTopTypes(types, nextOrderMatrix, n.of.types = 3,
  exclude.same = TRUE)
}
\arguments{
\item{types}{- the vector of types (subcategory/category/type) to be checked}

\item{nextOrderMatrix}{- the matrix of connections between the types}

\item{n.of.types}{- the number of n nearest types}

\item{exclude.same}{- remove the same type from the results}
}
\description{
The function that return the n nearest types to the given ones
}
