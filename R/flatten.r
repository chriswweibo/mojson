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
#'
#' @examples

flattenj <- function(dat){
  i= iter(1:length(dat))
  result = list.apply(dat, function(x) flattenj_one(x) %>% cbind(.,index=nextElem(i))) %>% list.rbind()

  return(result)
}
