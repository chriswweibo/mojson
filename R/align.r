#' JSON lists alignment
#'
#' @description Align the two JSON lists by specifying the primary path(keys), to fulfill
#'     the left/inner/right-join style comparison.
#'
#' @param json_new \code{list}. The new JSON list.
#' @param json_old \code{list}. The old JSON list.
#' @param sep \code{character}. A character/string passed to \code{\link{flattenj}}.
#'     Defaults to @ to avoid the occasional overriding. Not allowed to use some risky characters like . and \.
#' @param primary \code{character}. The primary path(keys) for identifying a unique JSON object.
#'     The value provided should contain the \code{sep} value to specify the nesting information unless it is an outermost key.
#'
#' @return \code{list}. The result list contains the alignment information including the primary paths only in the new JSON,
#'     only in the old JSON, and in both.
#' @details The function borrows the idea from the data set operation, whose result contains five data frames:
#'     \itemize{
#'     \item{`new` contains the flattening result of `json_new`.}
#'     \item{`old` contains the flattening result of `json_old`.}
#'     \item{`common_primary` contains the primary paths both in `json_new` and `json_old`.}
#'     \item{`new_primary` contains the primary paths only in `json_new`.}
#'     \item{`old_primary` contains the primary paths only in `json_old`.}}
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' library(mojson)
#' j1 = list(a=list(x=1, y=2), b=list(x=2, y=2))
#' j2 = list(b=list(x=2, y=3), d = list(x=3,y=1))
#' alignj(j1, j2, primary = 'x')
alignj = function(json_new, json_old, sep = "@", primary) {
  new = flattenj(json_new)
  old = flattenj(json_old)

  primary_new = subset(new, paths == primary)$values
  primary_old = subset(old, paths == primary)$values
  common_part = intersect(primary_new, primary_old)
  only_new = setdiff(primary_new, primary_old)
  only_old = setdiff(primary_old, primary_new)
  return(list(new = new,
              old = old,
              common_primary = common_part,
              new_primary = only_new,
              old_primary = only_old))

}
