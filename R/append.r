appendj=function(dat, added){
 result = rbind(flattenj(dat),flattenj(added)) %>% nest()
 return(result)
}

