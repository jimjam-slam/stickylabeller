#' Label facets with a string template.
#'
#' Returns a labeller function that you can give to the `labeller` argument of
#' a `facet_*` function.
#' 
#' If you're using [label_glue()] with [ggplot2::facet_wrap()] or you're
#' individually supplying labellers to each variable, you only need one string
#' template: `row_template`.
#' 
#' If you're using it with [ggplot2::facet_grid()], you need to supply two
#' templates: one for the rows (`row_template`) and one for the columns
#' (`col_template`).
#'
#' If you're using the labeller with [`ggplot2::facet_wrap()`], you can also
#' use these variables in the templates:
#'   * `.n` to add numbers to each facet;
#'   * `.l` or `.L` to add lower- or uppercase letters
#'   * `.r` or `.R` to add lower- or uppercase roman numerals.
#'
#' @param row_template A string to be used as the template by
#' [glue::glue_data()].
#' @param col_template A string to be used as the template by
#' [glue::glue_data()].
#' @param summary_data A data frame of additional  variables to reference in
#'   the templates. Must also include the facet grouping variables.
#' @param ... Other arguments to be passed to [`glue::glue_data()`]
#'
#' @return A labelling function that you can give to the `labeller` argument
#'   of the facetting function.
#' @examples
#' library(ggplot2)
#' library(stickylabeller)
#'
#' # wrap facet columns in braces to refer to their values in the labels
#' p1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
#' p1 + facet_wrap(
#'   ~  Species,
#'   labeller = label_glue("Sepal and petal lengths in {Species} plants"))
#'
#' # distinguish panels with .n (numbers), .l (lowercase), .L (uppercase),
#' # .r or .R (lower- or uppercase roman) if you're using facet_wrap
#' p1 + facet_wrap(
#'   ~  Species,
#'   labeller = label_glue("({.n}) {Species}"))
#'
#' # you can also use label_glue with facet_grid
#' p2 <- ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point()
#' p2 + facet_grid(
#'   gear ~ cyl,
#'   labeller = label_glue(
#'     row_template = "{gear} gears",
#'     col_template = "{cyl} cylinders"))
#'
#' # you can add summary statistics in a couple of ways. the easiest (in terms
#' # of plot code) is to join a summary back into the original data and to add
#' # the new columns in the facet spec
#' library(dplyr)
#' cyl_stats <- mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(cyl_n = n(), cyl_meanmpg = sprintf("%#.2f", mean(mpg)))
#' mtcars_joined <- mtcars %>% inner_join(cyl_stats)
#'
#' p3 <- ggplot(mtcars_joined, aes(x = disp, y = mpg)) + geom_point()
#' p3 + facet_wrap(
#'   ~ cyl + cyl_n + cyl_meanmpg,
#'   labeller = label_glue(
#'     "({.l}) {cyl} cylinders\n(n = {cyl_n}, mean = {cyl_meanmpg})"))
#'
#'
#' @export
label_glue <- function(row_template, col_template, summary_data = NULL, ...) {

  # here's the inner function. label_glue returns this, but it can access the
  # template string given when the user calls label_glue
  label_glue_inner <- function(labels) {

    # grab facet info before a possible merge with summary data nukes it
    facet_type <- attr(labels, "facet")
    facet_direction <- attr(labels, "type")

    # merge summary_data if it exists
    if (is.data.frame(summary_data)) {
      labels <- merge(labels, summary_data)
    }

    if (!is.null(facet_type) & facet_type == "wrap") {

      # for facet_wrap, convert incoming labels to strings
      # and add extra columns for numbering
      facet_count <- nrow(labels)
      template <- row_template
      labels <- lapply(labels, as.character)
      labels[[".n"]] <- as.character(seq_len(facet_count))
      labels[[".l"]] <- make_letters(seq_len(facet_count))
      labels[[".L"]] <- toupper(make_letters(seq_len(facet_count)))
      labels[[".r"]] <- tolower(as.character(utils::as.roman(1:facet_count)))
      labels[[".R"]] <- as.character(utils::as.roman(1:facet_count))

    } else if (!is.null(facet_type) & facet_type == "grid") {

      if (numbering_present(col_template) | numbering_present(row_template)) {
        stop(paste("Error: the column or row label contains .n, .l or .L.",
          "label_glue can currently only number the facets in facet_wrap.",
          "For more info, see",
          "https://github.com/jimjam-slam/stickylabeller/issues/1"))
      }

      if (facet_direction == "rows") {
        template <- row_template
      } else if (facet_direction == "cols") {
        template <- col_template
      } else {
        stop(paste("Error: unrecognised facet_direction in label_guide. This",
          "is probably a bug in stickylabeller. Please report it to",
          "https://github.com/jimjam-slam/stickylabeller/issues"))
      }
      labels <- lapply(labels, as.character)

    } else {

      # if no facet type is specified (eg. inside labeller wrapper), just do
      # the basics

      if (numbering_present(row_template)) {
        stop(paste("Error: the column or row label contains .n, .l or .L.",
          "label_glue can currently only number the facets in facet_wrap.",
          "For more info, see",
          "https://github.com/jimjam-slam/stickylabeller/issues/1"))
      }
      template <- row_template
      labels <- lapply(labels, as.character)
    }

    return(list(unname(glue::glue_data(labels, template, ...))))
  }

  class(label_glue_inner) <- c("function", "labeller")
  return(label_glue_inner)
}

# helper functions ============================================================

#' Detect the use of numbering variables
#' 
#' Numbering variables are only supported in string templates when using
#' [ggplot2::facet_wrap()].
#' @param template The string template to check
#' @return A boolean: TRUE if a numbering variable is present.
numbering_present <- function(template) {
  # TODO - need a more sophisticated regex that can catch more complex
  # expressions including numbering columns
  return(grepl("{[\\s\\W]*\\.[nlLrR](?!\\w)[\\s\\W]*}", template, perl = TRUE))
}

#' Build a sequence of letters to extend past 26 facets, in the style of
#' spreadsheet column indices (Y, Z, AA, AB, AC, ...).
#' 
#' (Adapted from cellranger::letter_to_num)
#' @param y A vector of indices to create letters for
#' @return A vector of letters potentially extending past a–z
make_letters <- function(y) {
  jfun <- function(div) {
    if (is.na(div)) return(NA_character_)
    ret <- integer()
    while (div > 0) {
      remainder <- ((div - 1) %% 26) + 1
      ret <- c(remainder, ret)
      div <- (div - remainder) %/% 26
    }
    paste(letters[ret], collapse = "")
  }
  ret <- vapply(y, jfun, character(1))
  if (length(ret) == 0) return(ret) # vapply doesn't always work
  ifelse(ret == "", NA_character_, ret)
}
