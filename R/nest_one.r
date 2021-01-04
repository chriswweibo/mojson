library(magrittr)

nestj_one=function(dat){
  keys = dat$paths %>% sapply(., function(x) str_split(x,'\\.'))
  max_level = keys %>% sapply(.,length) %>% max()
  key_level = list(rep(list(NULL),max_level))
  keys_unique = keys %>% unlist() %>% unique()
  for (i in 1:length(keys)){

  }

}
