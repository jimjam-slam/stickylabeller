#' Pushes out wrapped facet panels underneath the facet labels.
#'
#'
#' @importFrom ggplot2 facet_wrap
#'
#' @export
facet_wrap_overlay <- function(
  facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE,
  labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE,
  dir = "h", strip.position = 'top') {

  ggproto(NULL, FacetWrapOverlay,
    shrink = shrink,
    params = list(
      horizontal = horizontal
    )
  )


}

FacetWrapOverlay <- ggproto("FacetWrapOverlay", FacetWrap,
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
    data, theme, params) {

    # can i use ggproto_parent(parent, self)?

    # so i'm following the extending ggplot2 vignette
    # (https://ggplot2.tidyverse.org/articles/extending-ggplot2.html) and it
    # looks like this function builds a gtable (panel_table), uses gtable
    # methods to add things like spacing between panels and then returns it.

    # their example (FacetTrans) adds the strips at the end, calling
    # render_strips to build them (according to the theme) and then
    # using gtable::gtable_add_row to create space and gtable::gtable_add_grob
    # to insert the strip.

    # https://stackoverflow.com/questions/30532889/ggplot-overlay-two-plots
    # this so question about overlaying one plot on another just uses
    # gtable::gtable_add_grob on its own. so maybe i just need to copy the
    # facet_wrap implementation but not insert a row...

  })
