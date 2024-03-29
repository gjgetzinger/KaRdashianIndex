% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{k_index}
\alias{k_index}
\title{Calculate the Kardashian Index for a researcher}
\usage{
k_index(twitter_handle, first_name, last_name, affiliation = NA)
}
\arguments{
\item{twitter_handle}{A twitter handle}

\item{first_name}{Researcher first name.}

\item{last_name}{Researcher last name.}

\item{affiliation}{Researcher affiliation.}
}
\value{
The K-index of a researcher.
}
\description{
https://en.wikipedia.org/wiki/Kardashian_Index
}
\examples{
k_index(twitter_handle = "gjgetzinger", first_name = "gordon", last_name = "getzinger")
}
