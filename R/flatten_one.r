#' Length-one JSON flatten
#' @description transform a json object into a flattened data frame.
#' @param dat list. loaded result from a json file.
#' @param sep character. A character/string used to separate full keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not allowed to use some risky words like . and \.
#'     When `compact=FALSE`, you need not to assign `sep` explicitly, unless @ has been used in the keys.
#' @param compact Boolean. Whether to generate the compact or completely expanded data frame. Defaults to `TRUE`.
#' @details The function can flatten a json object whose length is one, into a new data frame.
#'     The data frame has two different styles of output according to the `complete` value.#'
#'     For `compact=TRUE`, the data frame has two columns. One is `paths` which stores the absolute path of each record.
#'     And the other is `values` which stores the corresponding values of each path.#'
#'     For `compact=FALSE`, the data frame has more columns based on the nesting way. see `expanddf()`.
#'     It actually applies the serialization way for flattening, which means the early values always consistently appear in the heading rows of the data frame.
#'     And if the value is a list object in the original data or a non-named list/vector in the R environment, the path will correspondingly be appended with a integer to specify each list element.
#'
#' @return data frame. The flattened result.
#' @export
#' @importFrom rlist list.flatten
#'
#' @examples
#' library(mojson)
#' j = list(a=list(x=1,y=2),b=c(3,4,list(z=5,s=6,t=list(m=7,n=8))))
#' flattenj_one(j)
#' flattenj_one(j, compact=F)
#'
flattenj_one <- function(dat, sep='@', compact=TRUE){
  warning('Please make sure the sep provided or @ does NOT appear in the JSON key fields.')
  flat <- list.flatten(dat)
  expanded <- list(root = 0)
  for (i in 1:length(flat))
  {
    if (!is.null(names(flat[[i]])))
    {
      father_name <- names(flat[i])
      children <- as.list(flat[[i]])
      names(children) <- paste(father_name, names(children), sep = sep)
      expanded <- unlist(c(expanded, children))
    } else
    {
      expanded <- unlist(c(expanded, flat[i]))
    }
  }
  expanded = expanded[-1]
  result = data.frame(paths = names(expanded), values = unlist(expanded), row.names = NULL)
  if(compact){
    return(result)
  }
  else{
    return(expanddf(result, column = 'paths', sep = sep))
  }

}


