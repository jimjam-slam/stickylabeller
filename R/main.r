#' Label facets with a string template.
#'
#' \code{label_glue} returns a labeller function that you can give to the
#' \code{labeller} argument of a \code{facet_*} function. If you're using
#' \code{label_glue} with \code{facet_wrap} or you're individually supplying
#' labellers to each variable, you only need one string template. If you're
#' using it with \code{facet_grid} directly, you need to give two templates:
#' \code{rows} and \code{cols}.
#'
#' If you're using
#' the labeller with \code{facet_wrap}, you can also use these variables in
#' \code{glue} strings:
#' \itemize{
#'   \item \code{.n} to add numbers to each facet;
#'   \item \code{.l} or \code{.L} to add lower- or uppercase letters (only up to 26 facets are supported);
#'   \item \code{.r} or \code{.R} to add lower or uppercase Roman numerals.
#' }
#' @param rows A string to be used as the template by \code{glue}.
#' @param cols A string to be used as the template by \code{glue}.
#'
#' @return A labelling function that you can give to the \code{labeller} argument
#'   of a \code{facet_*} function.
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
#' # distinguish panels with .n (numbers), .l (lowercase), .L (uppercase),
#' # .r or .R (Roman)
#' # (this is only available with facet_wrap presently!)
#' p1 + facet_wrap(
#'   ~  Species,
#'   labeller = label_glue('({.n}) {Species}'))
#'
#' # you can also use label_glue with facet_grid
#' p2 <- ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point()
#' p2 + facet_grid(
#'   gear ~ cyl,
#'   labeller = label_glue(
#'     rows = '{gear} gears',
#'     cols = '{cyl} cylinders'))
#'
#' # you can add summary statistics in a couple of ways. the easiest (in terms
#' # of plot code) is to join a summary back into the original data and to add
#' # the new columns in the facet spec
#' library(dplyr)
#' cyl_stats <- mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(cyl_n = n(), cyl_meanmpg = sprintf('%#.2f', mean(mpg)))
#' mtcars_joined <- mtcars %>% inner_join(cyl_stats)
#'
#' p3 <- ggplot(mtcars_joined, aes(x = disp, y = mpg)) + geom_point()
#' p3 + facet_wrap(
#'   ~ cyl + cyl_n + cyl_meanmpg,
#'   labeller = label_glue(
#'     '({.l}) {cyl} cylinders\n(n = {cyl_n}, mean = {cyl_meanmpg})'))
#'
#'
#' @export
label_glue <- function(rows, cols) {

  # here's the inner function. label_glue returns this, but it can access the
  # template string given when the user calls label_glue
  label_glue_inner <- function(labels) {

    facet_type <- attr(labels, "facet")

    if (!is.null(facet_type) & facet_type == "wrap") {

      # for facet_wrap, convert incoming labels to strings
      # and add extra columns for numbering
      facet_count <- nrow(labels)
      template <- rows
      labels <- lapply(labels, as.character)
      labels[[".n"]] <- as.character(1:facet_count)
      labels[[".l"]] <- letters[1:facet_count]
      labels[[".L"]] <- toupper(letters[1:facet_count])
      labels[[".r"]] <- tolower(as.character(utils::as.roman(1:facet_count)))
      labels[[".R"]] <- as.character(utils::as.roman(1:facet_count))

    } else if (!is.null(facet_type) & facet_type == "grid") {

      if (numbering_present(cols) | numbering_present(rows)) {
        stop(paste("Error: the column or row label contains .n, .l or .L.",
          "label_glue can currently only number the facets in facet_wrap.",
          "For more info, see",
          "https://github.com/rensa/stickylabeller/issues/1"))
      }

      facet_direction <- attr(labels, "type")
      if (facet_direction == "rows") {
        template <- rows
      } else if (facet_direction == "cols") {
        template <- cols
      } else {
        stop(paste("Error: unrecognised facet_direction in label_guide. This",
          "is probably a bug in stickylabeller. Please report it to",
          "https://github.com/rensa/stickylabeller/issues"))
      }
      labels = lapply(labels, as.character)

    } else {

      # if no facet type is specified (eg. inside labeller wrapper), just do
      # the basics

      if (numbering_present(rows)) {
        stop(paste("Error: the column or row label contains .n, .l or .L.",
          "label_glue can currently only number the facets in facet_wrap.",
          "For more info, see",
          "https://github.com/rensa/stickylabeller/issues/1"))
      }
      template <- rows
      labels = lapply(labels, as.character)
    }

    return(list(unname(glue::glue_data(labels, template))))
  }

  class(label_glue_inner) <- c("function", "labeller")
  return(label_glue_inner)
}

# helper functions

numbering_present <- function(template) {
  # TODO - need a more sophisticated regex that can catch more complex
  # expressions including numbering columns
  return(grepl("{[\\s\\W]*\\.[nlLrR](?!\\w)[\\s\\W]*}", template, perl = TRUE))
}
