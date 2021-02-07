#' JSON Load
#' @description Load a JSON file into an R list
#' @param file \code{character}. A JSON file connection.
#' @param encoding \code{character}. Encoding method to use. Defaults to UTF-8.
#' @details This function provides a simple interface to load a JSON file, meanwhile prints some loading information.
#'     \itemize{
#'     \item{`num_of_loaded_obj` tells the length of the JSON object.}
#'     \item{`duration_seconds` tells the loading duration.}
#'     \item{`speed_objs_sec` tells the loading speed in objects per second.}
#'     \item{`obj_len_summary` gives the length summary of each JSON object.}
#'     }
#' @return \code{list}. The loading result.
#' @export
#' @importFrom RJSONIO fromJSON
#'
#' @examples
#' library(mojson)
#' j <- list(a = list(1, 2), b = 3)
#' tf <- tempfile()
#' writeLines(RJSONIO::toJSON(j), tf)
#' loadj(tf)

loadj <- function(file, encoding = "UTF-8")
{
  tic <- Sys.time()
  dat <- fromJSON(file, encoding = encoding)
  toc <- Sys.time()
  info <- list()
  info$num_of_loaded_obj <- length(dat)
  info$duration_seconds <- as.numeric(toc - tic)
  info$speed_objs_sec <- info$num_of_loaded_obj/info$duration
  each_len <- sapply(dat, length)
  info$obj_len_summary <- summary(each_len)
  print(info)
  return(dat)
}
