#' JSON Description
#' @description  Provide descriptive information about the JSON list, such as the key frequency, the nesting information and the value distribution.
#'
#' @param dat \code{list}. Loaded result from a JSON file.
#' @param sep \code{character}. A character/string passed to \code{\link{flattenj}}.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#' @details The result contains three parts:
#'    \itemize{
#'    \item{`key_summary`, presents the description of keys, which contains all the keys and their respective frequencies.}
#'    \item{`value_summary`, presents the description of values, which contains all atomic values and their respective frequencies.}
#'    \item{`stream_summary`, presents the description of paths' direct upstream keys and downstream keys.
#'     The \code{up} data frame stores the upstream information about where the current key is nested.
#'     And the \code{down} data frame stores the downstream information about how the current key branches.
#'     It means no upstream or downstream if \code{.} value is empty.}
#'     }
#'    \bold{Note that the mathematical logic of frequency is based on the flattening work, which means the occurrence of one key will be considered as repeated if it has multiple downstream keys}.
#'     For example, \code{list(list(x = list(m = 1, n = 2), y = 2))}, and the frequency of \code{x} will be 2, because it has two nesting keys.
#'     It is recommended to interpret the upstream and downstream information in a relative way rather than an absolute way.
#'     Returning the absolute frequency is to preserve the raw information.
#'     Hence, it is easy to know that \code{x} will equally branches to \code{m} and \code{n}.
#'
#' @return \code{list}. The descriptive result.
#' @seealso \code{\link{flattenj}}.
#' @export
#' @importFrom stringr str_split str_remove_all str_replace_all

#' @importFrom magrittr %>%
#'
#' @examples
#' library(mojson)
#' j <- list(a = list(x = 1, y = 2),
#'           b = c(3, 4, list(z = 5, s = 6, t = list(m = 7, n = 8))))
#' j_multi <- list(j, j, j)
#' desc <- descj(j_multi)
#' desc$keys_summary
#'
descj <- function(dat, sep = "@")
{
  message("flattening the list...")
  dat <- flattenj(dat, sep = sep)
  message("generating key summary...")
  paths <- dat$paths  # all paths in the records
  path_keys <- paths %>% sapply(str_split, sep)  # all keys in the path strings
  keys_summary <- path_keys %>% unlist() %>% table() %>% as.data.frame() %>%
    .[order(-.$Freq), ]  # keys occurrence

  message("generating stream summary...")
  idx <- nrow(keys_summary)
  stream_summary <- as.list(1:idx)
  names(stream_summary) <- keys_summary$.
  for (i in 1:idx)
  {
    key <- keys_summary$.[i] %>%
      str_replace_all(fixed("["), "\\[") %>%
      str_replace_all(fixed("]"), "\\]") %>%
      str_replace_all(fixed("."), "\\.")
    key_pattern <- paste(sep, key, "$", "|", "^", key, sep, "|", sep,
                         key, sep, "|", "^", key, "$", sep = "")
    extracted <- paths[grepl(key_pattern, paths)]
    up <- str_remove_all(extracted, paste("(", sep, ")?", key, ".*", sep = "")) %>%
      sapply(function(x) str_split(x, sep)[[1]] %>% .[length(.)]) %>%
      table() %>%
      as.data.frame() %>%
      .[order(-.$Freq), ]
    down <- str_remove_all(extracted, paste(".*", key, "(", sep, ")?", sep = "")) %>%
      sapply(function(x) str_split(x, sep)[[1]] %>% .[1]) %>%
      table() %>%
      as.data.frame() %>%
      .[order(-.$Freq), ]

    stream_summary[[i]] <- list(up = up, down = down)
  }


  message("generating value summary...")
  unique_innermost <- paths %>%
    sapply(function(x) str_split(x, sep)[[1]] %>% .[length(.)]) %>%
    unique()
  value_summary <- list()
  for (j in 1:length(unique_innermost))
  {
    key <- unique_innermost[j]
    key_pattern <- paste(sep, key, "$", "|", "^", key, sep, "|", sep,
                         key, sep, "|", "^", key, "$", sep = "")
    value_set <- subset(dat, grepl(key_pattern, dat$paths))
    value_summary[[j]] <- value_set$values %>%
      table() %>%
      as.data.frame() %>%
      .[order(-.$Freq), ]
  }
  names(value_summary) <- unique_innermost
  return(list(keys_summary = keys_summary,
              stream_summary = stream_summary,
              value_summary = value_summary))
}
