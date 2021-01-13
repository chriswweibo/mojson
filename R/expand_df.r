library(tidyr)
expanddf=function(df, seperator='\\.'){
  max_level = df$paths %>% sapply(., function(x) str_split(x,seperator)) %>% sapply(.,length) %>% max()
  levels = paste('level', 1:max_level, sep='')
  result = df %>% separate(paths, levels, seperator)
  return(result)
}
