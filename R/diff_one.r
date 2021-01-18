#' Single JSON Diff
#'
#' @description calculating the difference between two JSON objects.
#' @param json_new list. The new json list.
#' @param json_old list. The old json list.
#' @param sep character. A character/string used to separate full keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not allowed to use some risky words like . and \.
#'     See `flattenj_one()`.
#' @details This function can find the difference between two JSON/list objects.
#'     The difference information is categorized into three types: key addition, key removal and value change.
#'     All these differences can be reflected by '+(add)', and '-(delete)' chng_type in the new JSON.
#'
#' @return list. The difference between two JSON objects. The `difference` stores the data frame about the details of each change.
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
diffj_one=function(json_new, json_old, sep='@'){
new= flattenj_one(json_new, sep=sep)
old = flattenj_one(json_old, sep=sep)

result = suppressWarnings(compare_df(new, old))
return(list(update = result$comparison_df, summary = result$change_summary))

}
