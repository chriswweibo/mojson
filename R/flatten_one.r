#' Single JSON Object Flatten
#' @description Transform a JSON object into a flattened data frame in a serialization way.
#' @param dat \code{list}. The list from a JSON object.
#' @param sep \code{character}. A character/string used to separate keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#'     When \code{compact = FALSE}, it is unnecessary to assign \code{sep} explicitly, unless @ has been used in the key fields.
#' @param compact logical. Whether to generate the compact or completely expanded data frame. Defaults to \code{TRUE}.
#' @details The function flattens a single JSON object into a data frame with two different schemas according to the \code{compact} value.
#'     \itemize{
#'     \item{For \code{compact = TRUE}, the data frame contains two columns. One is `paths` which stores the absolute path of each record.
#'     And the other is `values` which stores the corresponding values of each path.}
#'     \item{For \code{compact = FALSE}, the data frame has more columns based on the global nesting situation.}
#'     }
#'     \bold{It actually applies the serialization way for flattening, which means the early values correspondingly appear in the heading rows of the data frame.}
#'     And if the value is a list object in the original data or a non-named list/vector in the R environment,
#'     the path will be correspondingly appended with an integer to specify each list element.
#'     For example, in the raw JSON file, "\{'a':\[1, 2, 3\]\}" will be \code{data.frame(paths = c('a1', 'a2', 'a3'), values = c(1, 2, 3))}.
#'     Great credits to the answer of \href{https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion/8139959#8139959}{Tommy}.
#' @seealso \code{\link{expanddf}}.
#'
#' @return \code{data frame}. The flattened result.
#' @export
#'
#'
#' @examples
#' library(mojson)
#' j <- list(a = list(x = 1, y = 2),
#'           b = c(3, 4, list(z = 5, s = 6, t = list(m = 7, n = 8))))
#' flattenj_one(j)
#' flattenj_one(j, compact = FALSE)
#'
flattenj_one <- function(dat, sep = '@', compact = TRUE) {
  warning(
    'Please make sure the sep character or @ does NOT appear in the JSON key fields.
    Or you can specify a non-overriding value for the sep variable.'
  )


  len <- sum(rapply(dat, function(x)
    1L))
  flat <- vector("list", len)
  i <- 0L
  items <- rapply(dat, function(x) {
    i <<- i + 1L
    flat[[i]] <<- x
    TRUE
  })
  if (!is.null(nm <- names(items)))
    names(flat) <- nm %>%
    sapply(gsub, pattern = '.', replacement = sep, fixed = T)

  expanded <- list(root = 0)
  for (i in 1:length(flat))
  {
    if (!is.null(names(flat[[i]])))
    {
      father_name <- names(flat[i])
      children <- as.list(flat[[i]])
      names(children) <-
        paste(father_name, names(children), sep = sep)
      expanded <- unlist(c(expanded, children))
    } else
    {
      expanded <- unlist(c(expanded, flat[i]))
    }
  }
  expanded = expanded[-1]
  result = data.frame(
    paths = names(expanded),
    values = unlist(expanded),
    row.names = NULL
  )
  if (compact) {
    return(result)
  }
  else{
    return(expanddf(result, column = 'paths', sep = sep))
  }

}
