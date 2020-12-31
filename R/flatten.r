#' Title transform a loaded json objects into a flatted data frame.
#'
#' @param dat list. loaded result from a json file.
#'
#' @return data frame. flattening result.
#' @export
#' @importFrom rlist list.rbind
#' @importFrom iterators iter
#' @importFrom iterators nextElem
#'
#' @examples
flattenj <- function(dat)
{
  pb <- txtProgressBar(style = 3)
  len= length(dat)
  result <- list()
  for (i in 1:len){
    tmp = cbind(flattenj_one(dat[i]),index=i)
    result[[i]] = tmp
    setTxtProgressBar(pb, value = i/len)
  }
  result = list.rbind(result)
  return(result)
}

