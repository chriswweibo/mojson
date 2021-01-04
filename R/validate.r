library(magrittr)

validatej=function(dat){
coln=colnames(dat)
if (!is.data.frame(dat)){
 stop('Please make sure that the input is a data.frame object.')
}
else{
  if (!all(c('paths', 'values','index')%in%colnames(dat))){
    stop('Please make sure that the input contains paths, values and index columns.')
  }
  else{
    if (ncol(dat)!=3){
      warning('Please make sure that the input has three columns. Only the paths, values and index columns will be used.')

    }
    else{
      if (dat$index %>% unique() %>% sort() %>% diff() %>% unique()!=1){
        stop('Please make sure that the index value is continuous integral number, starting from one.')
      }
      else{
        return(TRUE)
      }

    }
  }}}
