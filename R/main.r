
label_glue <- function(template) {

  # here's the inner function. label_glue returns this, but it can access the
  # template string given when the user calls label_glue
  label_glue_inner <- function(labels) {

    # TODO - I can check the `facet` attribute to see whether it's a wrap
    # or a grid, and the `type` attribute to see rows or columns (direction?)
    # facet_type <- attr(labels, "facet")
    facet_count <- nrow(labels)

    # convert incoming labels to strings and add extra columns
    labels = lapply(labels, as.character)
    labels[[".n"]] <- as.character(1:facet_count)
    labels[[".l"]] <- letters[1:facet_count]
    labels[[".L"]] <- toupper(letters[1:facet_count])

    # new_labels = glue_data(labels, template)
    return(list(unname(glue_data(labels, template))))
  }

  class(label_glue_inner) <- c("function", "labeller")
  return(label_glue_inner)
}
