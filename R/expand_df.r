#' Title
#'
#' @param df
#' @param separator
#'
#' @return
#' @importFrom tidyr separate
#' @export
#'
#' @examples
expanddf=function(df, separator='\\.'){
  max_level = df$paths %>% sapply(., function(x) str_split(x,separator)) %>% sapply(.,length) %>% max()
  levels = paste('level', 1:max_level, sep='')
  result = df %>% separate(paths, levels, seperator)
  return(result)
}
