library(magrittr)

nestj_one=function(dat,complete=F){
if(!complete){
dat = expanddf(dat)
}
  else{
    dat = dat
  }
  level_coln = colnames(dat) %>% .[grepl('level',.)]
  keys = lapply(level_coln, function(x) dat[[x]] %>% str_remove_all(.,'[0-9]+$') %>% unique %>% na.omit)

  nestedlist <- split(dat, level_coln, drop = TRUE)
}
