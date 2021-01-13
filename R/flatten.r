#' Title transform a loaded json objects into a flatted data frame.
#'
#' @param dat list. loaded result from a json file.
#'
#' @return data frame. flattening result.
#' @export
#' @importFrom rlist list.rbind
#' @importFrom iterators iter
#' @importFrom iterators nextElem
#' @importFrom magrittr %>%
#' @importFrom tidyr seperate
#'
#' @examples
#'

flattenj <- function(dat, complete=FALSE){
  i= iter(1:length(dat))
  tmp = list.apply(dat, function(x) flattenj_one(x) %>% cbind(.,index=nextElem(i))) %>% list.rbind()
  if(!complete){
    result = tmp
  }
  else {
    result = expanddf(tmp)
  }

  return(result)
}

