library(RJSONIO)


loadj=function(file_path,encoding = 'UTF-8'){
  tryCatch(
  {content = readLines(file_path, encoding = encoding)
  dat = fromJSON(content)
  },
  finally = print('Load JSON file failed. Please check the file path and the enconding method!'))
  len =length(dat)
  len_each = sapply(dat,length)
  len_sum = summary(len_each)
  print(paste('BRIEF:','##########'))
  print(paste('Loaded ',len, ' JSON objects.'))
  print('The summary of the JSON objects is as below ')
  print(table(len_sum))
  return(dat)
}
