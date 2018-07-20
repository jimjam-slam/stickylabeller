#' Label facets with glue.
#'
#' \code{label_glue} returns a labeller function that you can give to the
#'   `labeller` argument of a `facet_*` function.
#'
#' @param template A string to be used as the template by \code{glue}. Wrap
#'   facet columns in braces, as you would in \code{glue}. If you're using
#'   the labeller with \code{facet_wrap}, you can also use \code{.n},
#'   \code{.l} or \code{.L} to add numbers or letters to each facet.
#'   you're supplying this \code{}
#' @return A labelling function that you can give to the `labeller` argument
#'   of a `facet_*` function.
#' @examples
#' library(ggplot2)
#' library(stickylabeller)
#'
#' # wrap facet columns in braces to refer to their values in the labels
#' p1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
#' p1 + facet_wrap(
#'   ~  Species,
#'   labeller = label_glue('Sepal and petal lengths in {Species} plants'))
#'
#' # distinguish panels with .n (numbers), .l (lowercase) or .L (uppercase)
#' # (this is only available with facet_wrap presently!)
#' p1 + facet_wrap(
#'   ~  Species,
#'   labeller = label_glue('({.n}) {Species}'))
#'
#' # you can also use label_glue with facet_grid, either to label individual
#' # variables or entire margins
#' p2 <- ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point()
#' p2 + facet_grid(
#'   gear ~ cyl,
#'   labeller = labeller(
#'     .rows = label_glue('{gear} gears'),
#'     .cols = label_glue('{cyl} cylinders')))
#'
#' # you can add summary statistics in a couple of ways. the easiest (in terms
#' # of plot code) is to join a summary back into the original data and to add
#' # the new columns in the facet spec
#' library(tidyverse)
#' cyl_stats <- mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(cyl_n = n(), cyl_meanmpg = sprintf('%#.2f', mean(mpg)))
#' mtcars_joined ,- mtcars %>% inner_join(cyl_stats)
#'
#' p3 <- ggplot(mtcars_joined, aes(x = disp, y = mpg)) + geom_point()
#' p3 + facet_wrap(
#'   ~ cyl + cyl_n + cyl_meanmpg,
#'   labeller = label_glue(
#'     '({.l}) {cyl} cylinders\n(n = {cyl_n}, mean = {cyl_meanmpg})'))
#'
#'
#' @export
label_glue <- function(template) {

  # here's the inner function. label_glue returns this, but it can access the
  # template string given when the user calls label_glue
  label_glue_inner <- function(labels) {

    browser()
    # i can add extra functionality for facet_wrap
    facet_type <- attr(labels, "facet")

    if(!is.null(facet_type) && facet_type == "wrap") {
      facet_count <- nrow(labels)

      # convert incoming labels to strings and add extra columns
      labels = lapply(labels, as.character)
      labels[[".n"]] <- as.character(1:facet_count)
      labels[[".l"]] <- letters[1:facet_count]
      labels[[".L"]] <- toupper(letters[1:facet_count])
    } else {
      # convert incoming labels to strings and add extra columns
      labels = lapply(labels, as.character)
    }

    return(list(unname(glue::glue_data(labels, template))))
  }

  class(label_glue_inner) <- c("function", "labeller")
  return(label_glue_inner)
}
