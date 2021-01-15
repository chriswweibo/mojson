#' Data frame expand
#' @description Expand a data frame by splitting one column
#' @param df data frame.
#' @param column character. The column to be splitted.
#' @param sep character. Used to split a column. Defaults to @.
#' @details This function implements the data frame expansion if you need to split one column into pieces.
#'     The new data frame will generate the new columns which are named by 'level'appended by indexing numbers, such as 'level1', 'level2'.
#'     The maximum of number indicates the deepest level. Any rows narrower than this number will be padded and corresponding cells will be filled with NAs.
#'
#' @return data frame that has been expanded by splitting one column.
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @export
#'
#' @examples
#' library(mojson)
#' # use default separator and nesting levels are identical.
#' df1 = data.frame(a=c('192.168.75.0','192.168.75.1'), b=c('T','F'))
#' expanddf(df1,'a')
#'
#' # change the separator and treat various nesting levels.
#' df2 = data.frame(a=c('1-2-0','1-2-0-3','1-2'), b=c('T','F','T'))
#' expanddf(df2,'a','-')
#'
expanddf=function(df, column, sep='@'){
  max_level = df[column] %>% sapply(., str_split,sep) %>% sapply(.,length) %>% max()
  levels = paste('level', 1:max_level, sep='')
  result = df %>% separate(column, levels, sep)
  return(result)
}
