filterj=function(dat, path, value){
  result = flattenj(dat) %>% subset(., path == value) %>% nest()
  return(result)


}
