#' Data Frame Expand
#' @description Expand a data frame by splitting one column
#' @param df \code{data frame}. The input to be expanded.
#' @param column \code{character}. The column to be splitted.
#' @param sep \code{character}. Separator for splitting a column.
#' @details This function implements the data frame expansion if you need to split one column by the specific characters.
#'     The new data frame will generate the new columns named as 'level' appended by position-indexing numbers, such as 'level1', 'level2'.
#'     The maximum of appended numbers indicates the most splitting pieces for one cell.
#'     If the splitting results of one cell are fewer than the maximum, the row will be padded and corresponding cells will be filled with NAs.
#'
#' @return \code{data frame}. The resultant data frame with new columns.
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @importFrom stringr str_split fixed
#' @export
#'
#' @examples
#' library(mojson)
#' # levels are identical.
#' df1 <- data.frame(a = c('ab@gmail.com', 'cd@gmail.com'),
#'                   b = c(TRUE, FALSE))
#' expanddf(df1, 'a', '@')
#'
#' # change the separator and treat various levels.
#' df2 <- data.frame(a = c('1-2-0', '1-2-0-3', '1-2'),
#'                   b = c(TRUE, FALSE, TRUE))
#' expanddf(df2, 'a', '-')
#'
expanddf <- function(df, column, sep) {
  max_level <- df[column] %>%
    sapply(str_split, sep) %>%
    sapply(length) %>%
    max()
  levels <- paste("level", 1:max_level, sep = "")
  result <- df %>%
    separate(column, levels, sep)
  return(result)
}
