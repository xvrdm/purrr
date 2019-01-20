#' Grab multiple elements (including nested element) from a list
#'
#' `fish()` allow you to index deeply and flexibly into data structures.
#' `fish()` consistently returns `NULL` when an element does not
#' exist.
#'
#' @param data A vector or environment
#' @param items A list of lists of accessors for indexing into the object.
#'   Each accessors is used with `pluck()` so they need to be written
#'   accordingly
#' @param .default Value to use if target is empty or absent.
#' @param missing.rm Can be turned to TRUE to remove unfound keys
#'
#' @examples
#' # Let's create some data structures:
#' obj1 <- list("a", list(1, elt = "foo"), pear="william", 56, "hello")
#' obj2 <- list("b", list(2, elt = "bar"), pear="sage", 34, "hola")
#'
#'
#' # fish() provides a way of retrieving multiple objects from such data
#' # structures by defining a list that contains all the needed accessors
#' items_to_get <- list(
#'   list(2, "elt"),
#'   list("pear"),
#'   list(5)
#' )
#'
#' fish(obj1, items_to_get)
#'
#' #' #' # Let's create a list of data structures:
#' x <- list(obj1, obj2)
#'
#' # fish() can be used with map() to get the desired keys in each item
#' # of a list of lists
#'
#' map(x, fish, items_to_get)
#'
#' # optionally, you can use fish to rename the elements by adding new names
#' # to your items_to_get
#' named_items_to_get <- list(
#'   list(2, "elt"),
#'   fruit=list("pear"),
#'   month=list(5)
#' )
#' map(x, fish, named_items_to_get)
#'
#' @export

fish <- function(data, items, .default = NULL, missing.rm = FALSE) {
  if(!is.list(items)) stop("The keys to search should be given as a list.")

  modified_names <- names(items) %||% rep("", length(items)) %>%
    sub("^$", as.character(NA), .)
  old_names <- map_chr(items, get_very_last)

  new_names <- modified_names
  for (i in seq_along(new_names)) {
    if(is.na(new_names[i])) new_names[i] <- old_names[i]
  }

  new_list <- items %>%
    purrr::map(~purrr::pluck(data, !!!., .default = .default)) %>%
    purrr::set_names(new_names)

  if(missing.rm) return(compact(new_list))

  new_list
}

get_very_last <- function(l) {
  last_value <- l[[length(l)]]
  if(length(last_value) != 1) return(get_very_last(last_value))
  last_value
}
