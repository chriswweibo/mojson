#' JSON load
#' @description Load a json file into an R list
#' @param file character. A json file connection
#' @param encoding character. Encoding method to use. Defaults to UTF-8.
#' @details This function provides a simple interface to load a json file, which prints some loading information.
#' `num_of_loaded_obj` tells the length of the json object.
#' `duration_seconds` tells the loading duration.
#' `speed_objs_sec` tells the loading speed in objects per second.
#' `obj_len_summary` gives the length summary of each json object.
#' @return list. The loading result.
#' @export
#' @importFrom RJSONIO fromJSON
#'
#' @examples
#' library(mojson)
#' j =list(a=list(1,2),b=3)
#' tf=tempfile()
#' writeLines(RJSONIO::toJSON(j),tf)
#' loadj(tf)

loadj=function(file='R/test.JSON', encoding='UTF-8'){
  tic = Sys.time()
 dat = fromJSON(file, encoding=encoding)
 toc = Sys.time()
 info = list()
 info$num_of_loaded_obj=length(dat)
  info$duration_seconds = as.numeric(toc-tic)
  info$speed_objs_sec = info$num_of_loaded_obj/info$duration
  each_len = sapply(dat,length)
  info$obj_len_summary = summary(each_len)
  print(info)
  return(dat)
}

