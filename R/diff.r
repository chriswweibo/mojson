#' Title calculating the difference between two jsons.
#'
#' @param json_from list. The original json list.
#' @param json_to list. The target json list.
#'
#' @return data.frame. The difference between two json lists.
#' @importFrom tidyr separate
#' @export
#'
#' @examples
diffj=function(json_new, json_old){
new= flattenj(json_new)
old = flattenj(json_old)

result = suppressWarnings(compare_df(new, old))
return(list(difference = result$comparison_df, summary = result$change_summary))

}
