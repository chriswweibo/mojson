#' Title load a json file connection into an R list
#'
#' @param file character. a json file connection
#' @param encoding character.Defaul to UTF-8. encoding method
#'
#' @return list. the loading result
#' @export
#' @importFrom RJSONIO fromJSON
#'
#' @examples
loadj=function(file){
  tic = Sys.time()
 dat = RJSONIO::fromJSON(file)
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

# tmp = fromJSON("~/projects/淋巴瘤多中心/友谊医院/youyi_checked_20200430.JSON")
