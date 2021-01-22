#' Multiple JSON Objects Diff
#'
#' @description Find the differences between multiple JSON objects including create, delete and update.
#' @param json_new \code{list}. The new json objects.
#' @param json_old \code{list}. The old json objects.
#' @param sep \code{character}. A character/string used to separate keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky words like . and \.
#'     When \code{compact=FALSE}, it is unnecessary to assign \code{sep} explicitly, unless @ has been used in the keys.
#' @param primary \code{character}. The primary key or path to identify a unique JSON object.
#'     The value provided should contains the \code{sep} value to specify the nesting information unless it is an outermost key.
#' @seealso \code{\link{diffj_one}}.
#' @return \code{list}. Contains the difference result, including create, delete and update information.
#' @export
#'
#' @examples

diffj = function(json_new, json_old, sep = '@', primary) {
  align_result = alignj(json_new, json_old, primary, sep = '@')
  new = align_result$new
  old = align_result$old
  only_new = align_result$new_primary
  only_old = align_result$old_primary

  new_index = subset(new, paths == primary &
                       values == only_new)$index
  new_ = subset(new, index == new_index)
  old_index = subset(old, paths == primary &
                       values == only_old)$index
  old_ = subset(old, index == old_index)

  new_common = subset(new, !is.element(index, new_index))
  # rownames(new_common)=NULL
  old_common = subset(old, !is.element(index, old_index))
  # rownames(old_common)=NULL
  common_result = suppressWarnings(compare_df(new_common, old_common))
  return(
    list(
      create = new_,
      delete = old_,
      update = common_result$comparison_df,
      summary = common_result$change_summary
    )
  )


}
