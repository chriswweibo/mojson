#' Multiple JSON Objects Diff
#'
#' @description Find the difference between multiple JSON objects yielded by create, delete and update operations.
#' @param json_new \code{list}. The new JSON objects.
#' @param json_old \code{list}. The old JSON objects.
#' @param sep \code{character}. A character/string used to separate keys in the nesting path.
#'     Defaults to @ to avoid the occasional overriding. Not recommended to use some risky characters like . and \.
#'     When \code{compact = FALSE}, it is unnecessary to assign \code{sep} explicitly, unless @ has been used in the keys.
#' @param primary \code{character}. The primary path(keys) for identifying a unique JSON object.
#'     The value provided should contain the \code{sep} value to specify the nesting information unless it is an outermost key.
#' @details This function finds out the difference between two JSON lists.
#'     And the difference is as follows:
#'     \itemize{
#'     \item{`create`, stores the flattened result of objects only in the `json_new`, that is some JSON objects have been created.}
#'     \item{`delete`, stores the flattened result of objects only in the `json_old`, that is some JSON objects have been deleted.}
#'     \item{`change`, stores the value update information in the common objects, reflected by '+(add)', and '-(remove)' in the `chng_type` field. }}
#'     The `change_summary` provides the general information of value change.
#' @return \code{list}. Contains the difference result, including path create, path delete and value change results.
#' @importFrom compareDF compare_df
#' @export
#'
#' @examples
#'
#' library(mojson)
#' j1 <- list(list(x = 1, y = 2, b = list(m = 1, n = 1)),
#'           list(x = 2, y = 2, b = list(m = 1, n = 1)))
#' j2 <- list(list(x = 2, y = 3, b = list(m = 1)),
#'           list(x = 3, y = 2, b = list(m = 1, n = 1)))
#' diffj(j1, j2, primary = 'x')

diffj <- function(json_new, json_old, sep = '@', primary)
{
  align_result <- alignj(json_new, json_old, primary, sep = sep)
  new <- align_result$new
  old <- align_result$old
  only_new <- align_result$new_primary
  only_old <- align_result$old_primary
  common <- align_result$common_primary

  new_index <- subset(new, paths == primary & values == only_new)$index
  new_ <- subset(new, index == new_index)

  old_index <- subset(old, paths == primary & values == only_old)$index
  old_ <- subset(old, index == old_index)

  new_common_index <- subset(new, paths == primary & values == common)$index
  new_common <- subset(new, index == new_common_index)

  old_common_index <- subset(old, paths == primary & values == common)$index
  old_common <- subset(old, index == old_common_index)

  common_result <- compare_df(new_common[, -3], old_common[, -3]) %>%
    suppressWarnings()
  return(list(create = new_,
              delete = old_,
              change = common_result$comparison_df,
              change_summary = common_result$change_summary))
}
