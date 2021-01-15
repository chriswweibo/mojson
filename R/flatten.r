#' JSON flatten
#' @description transform multiple json objects into a flattened data frame.
#' @param dat list. loaded result from a json file.
#' @param sep character. A character/string used to separate full keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not allowed to use some risky words like . and \.
#'     When `compact=FALSE`, you need not to assign `sep` explicitly, unless @ has been used in the keys.
#' @param compact Boolean. Whether to generate the compact or completely expanded data frame. Defaults to `TRUE`.
#' @details The function can flatten multiple json objects into a new data frame. The result contains multiple columns.
#'     If `compact=TRUE`, it returns paths, values and index columns, otherwise level1,level2..., values and index.
#'     The `index` column stores the id of each json object. For other information, see `flattenj_one()`.
#'
#' @return data frame. The flattened result.
#' @export
#' @importFrom rlist list.rbind
#' @importFrom rlist list.apply
#' @importFrom iterators iter
#' @importFrom iterators nextElem
#' @importFrom magrittr %>%
#'
#' @examples
#' library(mojson)
#' j = list(a=list(x=1,y=2),b=c(3,4,list(z=5,s=6,t=list(m=7,n=8))))
#' j_multi = list(j,j,j)
#' flattenj(j_multi)
#' flattenj(j_multi, compact=F)
#'

flattenj <- function(dat, sep='@', compact=TRUE){
  i = iter(1:length(dat))
  warning('Please make sure the sep provided or @ does NOT appear in the JSON key fields.')
  message('(1/2) flattening to the compact result...')
  tmp = list.apply(dat, function(x) flattenj_one(x, sep=sep, compact=TRUE) %>% cbind(.,index=nextElem(i))) %>% list.rbind()
  if(compact){
    return(tmp)
  }
  else {
    message('(2/2) flattening to the expanded result...')
    return(expanddf(tmp, column = 'paths', sep = sep))
  }
}

