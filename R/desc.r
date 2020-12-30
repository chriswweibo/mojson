library(rlist)
library(stringr)
library(magrittr)
source('R/flatten.r')

pb <- txtProgressBar(style=3)

descj= function(dat){
  dat =flattenj(dat)
  paths = names(dat)
  keys_summary= sapply(paths, strsplit, '\\.') %>% unlist() %>% table() %>% as.data.frame()

  idx= nrow(keys_summary)
  stream_summary=as.list(1:idx)
  names(stream_summary)=keys_summary$.
  value_summary=list()
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

    return(list(keys_summary=keys_summary,stream_summary=stream_summary))
}
