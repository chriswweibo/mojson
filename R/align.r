#' JSON list alignment
#'
#' @description Align the two JSON lists by specifying the primary key(path), to fulfill
#'     the left/inner/right-join style comparison.
#'
#' @param json_new \code{list}. The new JSON list.
#' @param json_old \code{list}. The new old list.
#' @param sep \code{character}. A character/string used to separate keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#' @param primary \code{character}. The primary path(keys) for identifying a unique JSON object.
#'     The value provided should contains the \code{sep} value to specify the nesting information unless it is an outermost key.
#'
#' @return \code{list}. The result list contains the alignment information including objects only in the new JSON,
#'     objects only in the old JSON, and those in both.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
alignj = function(json_new, json_old, sep = '@', primary) {
  new = flattenj(json_new)
  old = flattenj(json_old)

  primary_new = subset(new, paths == primary)$values
  primary_old = subset(old, paths == primary)$values
  common_part = intersect(primary_new, primary_old)
  only_new = setdiff(primary_new, primary_old)
  only_old = setdiff(primary_old, primary_new)
  return(
    list(
      new = new ,
      old = old,
      common = common_part,
      new_primary = only_new,
      old_primary = only_old
    )
  )

}
