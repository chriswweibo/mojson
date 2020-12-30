library(RJSONIO)


loadj=function(file,encoding = 'UTF-8'){
  tic = Sys.time()
 dat = fromJSON(file,encoding = encoding)
 toc = Sys.time()
 info = list()
 info$num_of_loaded_obj=length(dat)
  info$`duration(seconds)` = as.numeric(toc-tic)
  info$`speed(objs/sec)` = info$num_of_loaded_obj/info$duration
  info$num_of_loaded_obj=length(dat)
  each_len = sapply(dat,length)
  info$obj_len_summary = summary(each_len)
  print(info)
  return(dat)
}
