#' Multiple JSON Objects Diff
#'
#' @description Find the differences between multiple JSON objects yielded by create, delete and update operations.
#' @param json_new \code{list}. The new json objects.
#' @param json_old \code{list}. The old json objects.
#' @param sep \code{character}. A character/string used to separate keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#'     When \code{compact=FALSE}, it is unnecessary to assign \code{sep} explicitly, unless @ has been used in the keys.
#' @param primary \code{character}. The primary key or path to identify a unique JSON object.
#'     The value provided should contains the \code{sep} value to specify the nesting information unless it is an outermost key.
#' @details This function can find the difference between two JSON/list objects.
#'     The difference information has three sources: path addition, path removal and value change.
#'     All differences are reflected by '+(add)', and '-(delete)' chng_type in the result.
#' @seealso \code{\link{diffj_one}}.
#' @return \code{list}. Contains the difference result, including create, delete and update information.
#' @export
#'
#' @examples
#'
#' library(mojson)
#' j1 = list(list(x=1, y=2,b = list(m=1,n=1)), list(x=2, y=2,b = list(m=1,n=1)))
#' j2 = list(list(x=2, y=3,b = list(m=1)), list(x=3, y=2,b = list(m=1,n=1)))
#' diffj(j1, j2, primary = 'a@x')

diffj = function(json_new, json_old, sep = '@', primary) {
  align_result = alignj(json_new, json_old, primary, sep = sep)
  new = align_result$new
  old = align_result$old
  only_new = align_result$new_primary
  only_old = align_result$old_primary
  common = align_result$common_primary

  new_index = subset(new, paths == primary & values == only_new)$index
  new_ = subset(new, index == new_index)

  old_index = subset(old, paths == primary & values == only_old)$index
  old_ = subset(old, index == old_index)

  new_common_index = subset(new, paths == primary & values == common)$index
  new_common = subset(new, index == new_common_index)

  old_common_index = subset(old, paths == primary & values == common)$index
  old_common = subset(old, index == old_common_index)

  common_result = suppressWarnings(compare_df(new_common[,-3], old_common[,-3]))
  return(
    list(
      create = new_,
      delete = old_,
      change = common_result$comparison_df,
      change_summary = common_result$change_summary
    )
  )


}
