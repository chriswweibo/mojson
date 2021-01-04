#' Title calculating the difference between two jsons.
#'
#' @param json_from list. The original json list.
#' @param json_to list. The target json list.
#'
#' @return data.frame. The difference between two json lists.
#' @export dplyr setdiff
#'
#' @examples
diffj=function(json_from,json_to){
from= flattenj(json_from)
to = flattenj(json_to)

d1= setdiff(to,from)
d2= setdiff(from,to)
if (nrow(d1)==0){
  d1=data.frame(NULL)
}
else{
  d1= cbind(d1,source='to')
}
if (nrow(d2)==0){
  d2=data.frame(NULL)
}
else{
  d2= cbind(d2,source='from')
}


return(rbind(d1,d2))
}
