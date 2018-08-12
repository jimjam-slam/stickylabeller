#' Pushes out wrapped facet panels underneath the facet labels.
#'
#'
#' @importFrom ggplot2 facet_wrap ggproto
#'
#' @export
facet_wrap_overlay <- function(
  facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE,
  labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE,
  dir = "h", strip.position = 'top') {

  # basically copying from the extending ggplot2 vignette
  # https://ggplot2.tidyverse.org/articles/extending-ggplot2.html
  facet <- facet_wrap(facets, nrow = NULL, ncol = NULL, scales = "fixed",
    shrink = TRUE, labeller = "label_value", as.table = TRUE, switch = NULL,
    drop = TRUE, dir = "h", strip.position = 'top')

  ggproto(NULL, FacetWrapOverlay, shrink = shrink, params = facet$params)

}

FacetWrapOverlay <- ggplot2::ggproto("FacetWrapOverlay", FacetWrap,

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


  # debug version: simpyl call the FacetWrap draw fn, then inspect the gtable
  # (can i modify the gtable myself, rather than reimplementing the parent fn?)
  # draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord,
  #   data, theme, params) {
  #
  #   # call FacetWrap's draw_panels, then inspect the gtable
  #   panel_table = ggplot2::ggproto_parent(FacetWrap, self)$draw_panels(
  #     panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
  #   plot(panel_table)
  #   browser()
  #   gtable::gtable_show_layout(panel_table)
  #   browser()
  #
  #   panel_table
  # },

  # this is a version of FacetWrap's draw_panels, modified to not create extra
  # space for the strips. it ignores theme.strip.placement
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord,
    data, theme, params) {

    if ((params$free$x || params$free$y) && !coord$is_free()) {
      stop(snake_class(coord), " doesn't support free scales", call. = FALSE)
    }

    if (inherits(coord, "CoordFlip")) {
      if (params$free$x) {
        layout$SCALE_X <- seq_len(nrow(layout))
      } else {
        layout$SCALE_X <- 1L
      }
      if (params$free$y) {
        layout$SCALE_Y <- seq_len(nrow(layout))
      } else {
        layout$SCALE_Y <- 1L
      }
    }

    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    n <- nrow(layout)

    panel_order <- order(layout$ROW, layout$COL)
    layout <- layout[panel_order, ]
    panels <- panels[panel_order]
    panel_pos <- convertInd(layout$ROW, layout$COL, nrow)
    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

    labels_df <- layout[names(params$facets)]
    attr(labels_df, "facet") <- "wrap"
    strips <- render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme)

    # If user hasn't set aspect ratio, and we have fixed scales, then
    # ask the coordinate system if it wants to specify one
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }

    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }

    empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
    panel_table <- empty_table
    panel_table[panel_pos] <- panels
    empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
    panel_table <- gtable_matrix("layout", panel_table,
     widths = unit(rep(1, ncol), "null"),
     heights = unit(rep(aspect_ratio, nrow), "null"), respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

    panel_table <- gtable_add_col_space(panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)

    # Add axes
    axis_mat_x_top <- empty_table
    axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
    axis_mat_x_bottom <- empty_table
    axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
    axis_mat_y_left <- empty_table
    axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
    axis_mat_y_right <- empty_table
    axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]
    if (!params$free$x) {
      axis_mat_x_top[-1,]<- list(zeroGrob())
      axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
    }
    if (!params$free$y) {
      axis_mat_y_left[, -1] <- list(zeroGrob())
      axis_mat_y_right[, -ncol] <- list(zeroGrob())
    }
    axis_height_top <- unit(apply(axis_mat_x_top, 1, max_height), "cm")
    axis_height_bottom <- unit(apply(axis_mat_x_bottom, 1, max_height), "cm")
    axis_width_left <- unit(apply(axis_mat_y_left, 2, max_width), "cm")
    axis_width_right <- unit(apply(axis_mat_y_right, 2, max_width), "cm")

    # Add back missing axes
    if (any(empties)) {
      first_row <- which(apply(empties, 1, any))[1] - 1
      first_col <- which(apply(empties, 2, any))[1] - 1
      row_panels <- which(layout$ROW == first_row & layout$COL > first_col)
      row_pos <- convertInd(layout$ROW[row_panels], layout$COL[row_panels], nrow)
      row_axes <- axes$x$bottom[layout$SCALE_X[row_panels]]
      col_panels <- which(layout$ROW > first_row & layout$COL == first_col)
      col_pos <- convertInd(layout$ROW[col_panels], layout$COL[col_panels], nrow)
      col_axes <- axes$y$right[layout$SCALE_Y[col_panels]]
      if (params$strip.position == "bottom" &&
          theme$strip.placement != "inside" &&
          any(!vapply(row_axes, is.zero, logical(1))) &&
          !params$free$x) {
        warning("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'", call. = FALSE)
      } else {
        axis_mat_x_bottom[row_pos] <- row_axes
      }
      if (params$strip.position == "right" &&
          theme$strip.placement != "inside" &&
          any(!vapply(col_axes, is.zero, logical(1))) &&
          !params$free$y) {
        warning("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'", call. = FALSE)
      } else {
        axis_mat_y_right[col_pos] <- col_axes
      }
    }
    panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
    panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)

    strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty_table
    strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
    if (params$strip.position %in% c("top", "bottom")) {
      inside <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "top") {
        placement <- if (inside) -1 else -2
        strip_pad <- axis_height_top
      } else {
        placement <- if (inside) 0 else 1
        strip_pad <- axis_height_bottom
      }
      strip_height <- unit(apply(strip_mat, 1, max_height), "cm")
      panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip)
      if (!inside) {
        strip_pad[unclass(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
      }
    } else {
      inside <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "left") {
        placement <- if (inside) -1 else -2
        strip_pad <- axis_width_left
      } else {
        placement <- if (inside) 0 else 1
        strip_pad <- axis_width_right
      }
      strip_pad[unclass(strip_pad) != 0] <- strip_padding
      strip_width <- unit(apply(strip_mat, 2, max_width), "cm")
      panel_table <- weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
      if (!inside) {
        strip_pad[unclass(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
      }
    }

    panel_table

  }
)
