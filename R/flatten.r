#' JSON Flatten
#' @description Transform multiple JSON objects into a flattened data frame.
#' @param dat \code{list}. Loaded result from a JSON file.
#' @param sep \code{character}. A character/string passed to \code{\link{flattenj_one}}.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#'     When \code{compact=FALSE}, it is unnecessary to assign \code{sep} explicitly, unless @ has been used in the key fields.
#' @param compact logical. Whether to generate the compact or completely expanded data frame. Defaults to \code{TRUE}.
#' @details The function flattens multiple JSON objects into a new data frame. The result contains multiple columns.
#'     If \code{compact=TRUE}, it returns \code{paths}, \code{values} and \code{index} columns, otherwise \code{level1}, \code{level2}, ..., \code{values} and \code{index}.
#'     The \code{index} column stores the id of each JSON object.
#' @seealso \code{\link{flattenj_one}}.
#' @return \code{data frame}. The flattened result.
#' @export
#' @importFrom iterators iter nextElem
#' @importFrom magrittr %>%
#'
#' @examples
#' library(mojson)
#' j <- list(a = list(x = 1, y = 2),
#'          b = c(3, 4, list(z = 5, s = 6, t = list(m = 7, n = 8))))
#' j_multi <- list(j, j, j)
#' flattenj(j_multi)
#' flattenj(j_multi, compact=FALSE)
#'

flattenj <- function(dat, sep = "@", compact = TRUE) {
  i <- iter(1:length(dat))
  warning("Please make sure the sep value or @ does NOT appear in the JSON key fields.
          Or you can specify a non-overriding value for the sep variable.")

  message("(1/2) flattening to the compact result...")
  tmp <- lapply(dat, function(x) flattenj_one(x, sep = sep, compact = TRUE) %>%
                     suppressWarnings() %>%
                     cbind(index = nextElem(i))) %>%
    do.call(what = "rbind")

  # if (!is.null(row.names(tmp))){ tmp$index = row.names(tmp) } else{ }
  if (compact) {
    message("(2/2) returning the compact result...")
    return(tmp)
  } else {
    message("(2/2) returning the expanded result...")
    return(expanddf(tmp, column = "paths", sep = sep))
  }
}
