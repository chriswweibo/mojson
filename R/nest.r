nestj = function(dat, complete=FALSE){
  if (validatej(dat, complete)){
    dat %>% group_by(index) %>% lapply(., nest_one, complete)
  }
  else{
   stop('Please make sure the input is valid. You can use validate() function to get details')
  }

}
