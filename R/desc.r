#' Title give the some description about the json list, such as keys and values.
#'
#' @param dat list. a JSOn list.
#'
#' @return list. the result contains three parts. key_summary presents the description
#'     of keys. value_summary presents the description of values. And stream_summary
#'     presents the description of keys' direct father and children.
#' @export
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#'
#' @examples
descj= function(dat){
  pb <- txtProgressBar(style=3)
  print('generating key summary...')
  dat =flattenj(dat)
  paths = dat$paths # all paths in the records
  path_keys = paths %>%  sapply(., str_split, '\\.')  # all keys in the path strings
  keys_summary= path_keys %>% unlist() %>% table() %>% as.data.frame() # keys occurrance

  print('generating stream summary...')
  idx= nrow(keys_summary)
  stream_summary=as.list(1:idx)
  names(stream_summary)=keys_summary$.
  for(i in 1:idx){
    key = keys_summary$.[i]
    extracted = paths[grepl(key, paths)]
    up= str_remove_all(extracted, paste('\\.*', key,'.*',sep = '')) %>%
      sapply(., function(x) str_split(x, '\\.')[[1]] %>% .[length(.)]) %>% table() %>% as.data.frame() %>% .[order(-.$Freq),]
    down= str_remove_all(extracted, paste('.*',key,'\\.*', sep = '')) %>%
      sapply(., function(x) str_split(x, '\\.')[[1]] %>% .[1]) %>% table() %>% as.data.frame() %>% .[order(-.$Freq),]

    stream_summary[[i]]=list(father=up,children=down)

setTxtProgressBar(pb, i/idx)
  }


  print('generating value summary...')
unique_innermost=paths %>% sapply(., function(x) str_split(x, '\\.')[[1]] %>% .[length(.)]) %>% unique()
value_summary=list()
for (j in 1:length(unique_innermost)){
  value_set= subset(dat, grepl(unique_k[j], dat$paths))
  value_summary[[j]] = value_set$values %>% table() %>% as.data.frame()
  setTxtProgressBar(pb, j/length(unique_innermost))
}
names(value_summary)=unique_innermost
    return(list(keys_summary=keys_summary,stream_summary=stream_summary, value_summary=value_summary ))
}
