library(magrittr)
library(stringr)
validatej=function(dat, complete=FALSE){
if (!is.data.frame(dat)) stop('Please make sure that the input is a data.frame object.')
  coln = colnames(dat)
if (!all(c('values','index') %in% coln)) stop('Please make sure that the input contains values and index columns.')
  if (dat$index %>% unique() %>% sort() %>% diff() %>% unique()!=1){
    stop('Please make sure that the index value is continuous integral number, starting from one.')
  }
if(!complete){
      if (!('paths' %in% coln)) stop('Please make sure that the input contains paths column.')
      warning('Only the paths, values and index columns will be used.')
          return(TRUE)
        }

else {
  warning('Only the level1, ..., leveln, values and index columns will be used.')
  level_coln_id = coln[grepl('level',coln)] %>% sapply(., str_remove,'level') %>% as.integer()
  lag = level_coln_id %>% sort() %>% diff() %>% unique()
  if (lag !=1) stop('Please make sure that the input contains continuous level1,.. columns.')
  return(TRUE)
}
}
