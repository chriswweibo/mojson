#' Title transform a loaded json objects into a flatted data frame.
#'
#' @param dat list. loaded result from a json file.
#'
#' @return data frame. flattening result.
#' @export
#' @importFrom rlist list.flatten
#'
#' @examples
flattenj_one <- function(dat)
{
  flat <- list.flatten(dat)
  expanded <- list(root = 0)
  for (i in 1:length(flat))
  {
    if (!is.null(names(flat[[i]])))
    {
      father_name <- names(flat[i])
      children <- as.list(flat[[i]])
      names(children) <- paste(father_name, names(children), sep = '.')
      expanded <- unlist(c(expanded, children))
    } else
    {
      expanded <- unlist(c(expanded, flat[i]))
    }
  }
  expanded = expanded[-1]
  return(data.frame(paths = names(expanded), values = unlist(expanded),row.names = NULL))
}


