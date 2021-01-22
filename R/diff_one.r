#' Single JSON Diff
#'
#' @description Calculate the difference between two JSON objects.
#' @param json_new \code{list}. The new JSON list.
#' @param json_old \code{list}. The old JSON list.
#' @param sep \code{character}. A character/string used to separate keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#'
#' @details This function can find the difference between two JSON/list objects.
#'     The difference information has three sources: path addition, path removal and value change.
#'     All differences are reflected by '+(add)', and '-(delete)' chng_type in the result.
#' @seealso \code{\link{flattenj_one}}
#' @return \code{list}. The difference between two JSON objects. The `update` stores the data frame about the details of each type of change.
#'     And the `summary` provides some useful summarization.
#' @importFrom compareDF compare_df
#' @export
#'
#' @examples
#' library(mojson)
#' j1 = list(a=list(x=1,y=2),b=c(3,4,list(z=5,s=6,t=list(m=7,n=8))))
#' j2= list(a=list(x=1,y=3),b=c(3,list(z=5,s=4,t=list(n=8))))
#' diffj_one(j1, j2)
#'
diffj_one = function(json_new, json_old, sep = '@') {
  new = flattenj_one(json_new, sep = sep) %>% suppressWarnings()
  old = flattenj_one(json_old, sep = sep) %>% suppressWarnings()
  warning('Please make sure the sep character or @ does NOT appear in the JSON key fields. Or you can specify a non-overiding value for the sep variable.')

  result = suppressWarnings(compare_df(new, old))
  return(list(
    update = result$comparison_df,
    summary = result$change_summary
  ))

}
