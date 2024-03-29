% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/childesr.R
\name{get_collections}
\alias{get_collections}
\title{Get collections}
\usage{
get_collections(connection = NULL, db_version = "current", db_args = NULL)
}
\arguments{
\item{connection}{A connection to the CHILDES database}

\item{db_version}{String of the name of database version to use}

\item{db_args}{List with host, user, and password defined}
}
\value{
A `tbl` of Collection data. If `connection` is supplied, the result
  remains a remote query, otherwise it is retrieved into a local tibble.
}
\description{
Get collections
}
\examples{
\dontrun{
get_collections()
}
}
