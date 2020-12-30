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
  dat =flattenj(dat)
  paths = names(dat)
  keys_summary= sapply(paths, strsplit, '\\.') %>% unlist() %>% table() %>% as.data.frame()

  idx= nrow(keys_summary)
  stream_summary=as.list(1:idx)
  names(stream_summary)=keys_summary$.

  for(i in 1:idx){
    key = keys_summary$.[i]
    extracted = paths[grepl(key, paths)]
    up= str_remove_all(extracted, paste('\\.', key,'.*',sep = '')) %>%
      sapply(., function(x) str_split(x, '\\.')[[1]] %>% .[length(.)]) %>% table() %>% as.data.frame() %>% .[order(-.$Freq),]
    down= str_remove_all(extracted, paste('.*',key,sep = '')) %>%
      sapply(., function(x) str_split(x, '\\.')[[1]] %>% .[1]) %>% table() %>% as.data.frame() %>% .[order(-.$Freq),]

    stream_summary[[i]]=list(father=up,children=down)

setTxtProgressBar(pb, i/idx)
  }
innermost=dat
innermost$keys= innermost$. %>% sapply(., function(x) str_split(x, '\\.')[[1]] %>% .[length(.)])
unique_k=unique(innermost$keys)
value_summary=as.list(1:len(ununique_k))
names(value_summary)=unique_k

for (j in 1:len(unique_k)){
  value_innermost[[j]]=innermost %>% subset(., key==k)$n %>% table() %>% as.data.frame()
}
    return(list(keys_summary=keys_summary,stream_summary=stream_summary, value_summary=value_summary ))
}
