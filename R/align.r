#' Title
#'
#' @param json_new
#' @param json_old
#' @param sep
#' @param primary
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
alignj=function(json_new, json_old, sep='@', primary){
  new = flattenj(json_new)
  old = flattenj(json_old)

  primary_new=subset(new, paths == primary)$values
  primary_old=subset(old, paths == primary)$values
  common_part = intersect(primary_new,primary_old)
  only_new = setdiff(primary_new, primary_old)
  only_old = setdiff(primary_old, primary_new)
  return(list(new=new , old = old, common=common_part, new_primary=only_new, old_primary=only_old))

}
