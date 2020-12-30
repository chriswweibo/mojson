library(rlist)
library(stringr)
library(magrittr)

pb <- txtProgressBar(style=3)

flattenj= function(dat){
  flat= list.flatten(dat)
  expanded = list(root=0)
  for(i in 1:length(flat)){
    if(!is.null(names(flat[[i]]))){
      father_name = names(flat[i])
      children = as.list(flat[[i]])
      names(children)=paste(father_name, names(children),sep='.')
      expanded=unlist(c(expanded, children))
    }
    else{
      expanded=unlist(c(expanded,flat[i]))
    }
    setTxtProgressBar(pb,value=i/length(flat))
  }
  expanded$root=NULL
  return(expanded)
}
