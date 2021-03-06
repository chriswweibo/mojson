% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.r
\name{loadj}
\alias{loadj}
\title{JSON Load}
\usage{
loadj(file, encoding = "UTF-8")
}
\arguments{
\item{file}{\code{character}. A JSON file connection.}

\item{encoding}{\code{character}. Encoding method to use. Defaults to UTF-8.}
}
\value{
\code{list}. The loading result.
}
\description{
Load a JSON file into an R list
}
\details{
This function provides a simple interface to load a JSON file, meanwhile prints some loading information.
\itemize{
\item{\code{num_of_loaded_obj} tells the length of the JSON object.}
\item{\code{duration_seconds} tells the loading duration.}
\item{\code{speed_objs_sec} tells the loading speed in objects per second.}
\item{\code{obj_len_summary} gives the length summary of each JSON object.}
}
}
\examples{
library(mojson)
j <- list(a = list(1, 2), b = 3)
tf <- tempfile()
writeLines(RJSONIO::toJSON(j), tf)
loadj(tf)
}
