% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_table_list.R
\name{get_table_list}
\alias{get_table_list}
\title{Get a list of tables in the schema}
\usage{
get_table_list(
  schema = Sys.getenv("SUPABASE_SCHEMA"),
  conn = make_connection()
)
}
\arguments{
\item{schema}{The schema name.}

\item{conn}{A database connection object.}
}
\value{
A vector of table names.
}
\description{
Get a list of tables in the schema
}
