
#' Multiple JSON Objects Diff
#'
#' @description find the differences between multiple JSON objects including create, delete and update.
#' @param json_new list. The new json objects.
#' @param json_old list. The old json objects.
#' @param sep character. A character/string used to separate full keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not allowed to use some risky words like . and \.
#'     When `compact=FALSE`, you need not to assign `sep` explicitly, unless @ has been used in the keys.
#' @param primary character. The primary key or path to identify a unique JSON object.
#'     Usually a nesting path containing the `sep` character provided previously.
#'
#' @return list. Contains thw difference result, including create, delete and update information.
#' @export
#'
#' @examples

diffj=function(json_new,json_old, sep='@', primary){


  align_result = alignj(json_new,json_old, primary, sep='@')
  new = align_result$new
  old = align_result$old
  only_new = align_result$new_primary
  only_old = align_result$old_primary

  new_index = new[new[primary]==only_new,]$index
  new_ = subset(new, index == new_index)
  old_index= old[old[primary]==only_old,]$index
  old_ = subset(old, index == old_index)

  new_common = subset(new, !is.element(index, new_index))
  old_common = subset(old, !is.element(index, old_index))

  common_result = suppressWarnings(compare_df(new_common, old_common))
  return(list(create =new_, delete=old_, update = common_result$comparison_df, summary = common_result$change_summary))


}
