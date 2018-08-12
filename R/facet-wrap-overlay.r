#' Pushes out wrapped facet panels underneath the facet labels.
#'
#' \code{facet_wrap_overlay} works just like \code{facet_wrap}, except that
#' the facet labels are overlaid on the panels. you might prefer this if you're
#' looking to save space or if you just want to number the panels with
#' \code{label_glue}.
#'
#' @param facets A set of variables or expressions quoted by [vars()]
#'   and defining faceting groups on the rows or columns dimension.
#'   The variables can be named (the names are passed to `labeller`).
#'
#'   For compatibility with the classic interface, can also be a
#'   formula or character vector. Use either a one sided formula, `~a
#'   + b`, or a character vector, `c("a", "b")`.
#' @param nrow,ncol Number of rows and columns.
#' @param scales Should scales be fixed (`"fixed"`, the default),
#'   free (`"free"`), or free in one dimension (`"free_x"`,
#'   `"free_y"`)?
#' @param shrink If `TRUE`, will shrink scales to fit output of
#'   statistics, not raw data. If `FALSE`, will be range of raw data
#'   before statistical summary.
#' @param labeller A function that takes one data frame of labels and
#'   returns a list or data frame of character vectors. Each input
#'   column corresponds to one factor. Thus there will be more than
#'   one with formulae of the type `~cyl + am`. Each output
#'   column gets displayed as one separate line in the strip
#'   label. This function should inherit from the "labeller" S3 class
#'   for compatibility with [labeller()]. See
#'   [label_value()] for more details and pointers to other
#'   options.
#' @param as.table If `TRUE`, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If `FALSE`, the
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param switch By default, the labels are displayed on the top and
#'   right of the plot. If `"x"`, the top labels will be
#'   displayed to the bottom. If `"y"`, the right-hand side
#'   labels will be displayed to the left. Can also be set to
#'   `"both"`.
#' @param drop If `TRUE`, the default, all factor levels not used in the
#'   data will automatically be dropped. If `FALSE`, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @param strip.position By default, the labels are displayed on the top of
#'   the plot. Using `strip.position` it is possible to place the labels on
#'   either of the four sides by setting \code{strip.position = c("top",
#'   "bottom", "left", "right")}
#' @param dir Direction: either `"h"` for horizontal, the default, or `"v"`,
#'   for vertical.
#'
#' @importFrom ggplot2 facet_wrap ggproto FacetWrap panel_cols panel_rows
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols gtable_matrix gtable_add_col_space gtable_add_row_space
#' @importFrom grid unit convertUnit
#' @importFrom rlang "%||%"
#'
#' @examples
#' library(ggplot2)
#' library(stickylabeller)
#' p = ggplot(mtcars) + geom_point(aes(x = mpg, y = gear))

#' # by default, labels have opaque backgrounds, so this isn't very useful
#' p + facet_wrap_overlay(~ cyl)
#'
#' # you'll probably want a blank facet strip background
#' p + facet_wrap_overlay(~ cyl) +
#'   theme(strip.background = element_blank())

#' # you can also do overlay effects
#' p + facet_wrap_overlay(~ cyl) +
#'   theme(
#'     strip.background = element_rect(fill = alpha('black', 0.5)),
#'     strip.text = element_text(colour = 'white'))
#'
#' # facet_wrap_overlay works with label_glue
#' p +
#'   facet_wrap_overlay(~ cyl,
#'     labeller = label_glue("({.l})")) +
#'   theme(strip.background = element_blank())
#'
#' @export
facet_wrap_overlay <- function(
  facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE,
  labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE,
  dir = "h", strip.position = 'top') {

  # basically copying from the extending ggplot2 vignette
  # https://ggplot2.tidyverse.org/articles/extending-ggplot2.html
  facet <- facet_wrap(facets, nrow = nrow, ncol = ncol, scales = scales,
    shrink = shrink, labeller = labeller, as.table = as.table, switch = switch,
    drop = drop, dir = dir, strip.position = strip.position)

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


  # debug version: simply call the FacetWrap draw fn, then inspect the gtable
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
    # OVERLAY - strips rendered here
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

      # OVERLAY - this is where the action starts for facet_wrap_overlay!
      # i want to ignore theme$strip.placement == 'outside'
      # so i *think* this can basically go?
      # if (params$strip.position == "bottom" &&
      #     theme$strip.placement != "inside" &&
      #     any(!vapply(row_axes, is.zero, logical(1))) &&
      #     !params$free$x) {
        warning("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'", call. = FALSE)
      # } else {
      #   axis_mat_x_bottom[row_pos] <- row_axes
      # }
      # if (params$strip.position == "right" &&
      #     theme$strip.placement != "inside" &&
      #     any(!vapply(col_axes, is.zero, logical(1))) &&
      #     !params$free$y) {
      #   warning("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'", call. = FALSE)
      # } else {
      #   axis_mat_y_right[col_pos] <- col_axes
      # }
      if (theme$strip.placement != "inside") {
        warning("Theme option strip.placement == 'outside' ignored when using facet_wrap_overlay", call. = FALSE)
      }
      axis_mat_x_bottom[row_pos] <- row_axes
      axis_mat_y_right[col_pos] <- col_axes
    }

    panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
    panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)

    # rendered strips being prepped here i think
    strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty_table
    strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]

    # looks like i can simplify this section by assuming theme$strip.placement (and .x, .y) are 'inside'
    # weave_tables_col and weave_tables_row are used to do the insertion; they call

    # if strips are on top/bottom...
    if (params$strip.position %in% c("top", "bottom")) {
      # inside == TRUE for facet_wrap_overlay
      inside <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "top") {
        # placement <- if (inside) -1 else -2
        placement <- -1
        strip_pad <- axis_height_top
      } else {
        # placement <- if (inside) 0 else 1
        placement <- 0
        strip_pad <- axis_height_bottom
      }
      strip_height <- unit(apply(strip_mat, 1, max_height), "cm")
      panel_table <- weave_strips_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip, params$strip.position)
      if (!inside) {
        # strip_pad[unclass(strip_pad) != 0] <- strip_padding
        # panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
        warning("Theme option strip.placement == 'outside' ignored when using facet_wrap_overlay", call. = FALSE)
      }

    # or if strips are on left/right...
    } else {
      # inside == TRUE for facet_wrap_overlay
      inside <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "left") {
        # placement <- if (inside) -1 else -2
        placement <- -1
        strip_pad <- axis_width_left
      } else {
        # placement <- if (inside) 0 else 1
        placement <- 0
        strip_pad <- axis_width_right
      }
      strip_pad[unclass(strip_pad) != 0] <- strip_padding
      strip_width <- unit(apply(strip_mat, 2, max_width), "cm")
      panel_table <- weave_strips_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip, params$strip.position)
      if (!inside) {
        # strip_pad[unclass(strip_pad) != 0] <- strip_padding
        # panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
        warning("Theme option strip.placement == 'outside' ignored when using facet_wrap_overlay", call. = FALSE)
      }
    }

    panel_table

  }
)

# Helpers (maybe modified versions of weave_tables_row and weave_tables_col?) ---

weave_strips_col <- function(table, table2, col_shift, col_width, name, z = 1, clip = "off", just = "left") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t

  # rearrange strip width/position/justifications to overlay on panel
  for (i in 1:length(table2)) {
    strip_width = table2[i][[1]]$widths
    table2[i][[1]]$widths = unit(1, 'npc')

    # is the strip and text grobs
    strip_child = grep(
      'strip.background',
      names(table2[1][[1]]$grobs[[1]]$children))
    title_child = grep('GRID.titleGrob',
      names(table2[i][[1]]$grobs[[1]]$children))

    # modify them
    table2[i][[1]]$grobs[[1]]$children[[strip_child]]$width = strip_width
    table2[i][[1]]$grobs[[1]]$children[[strip_child]]$x =
      switch(just, 'left' = unit(0, 'npc'), 'right' = unit(1, 'npc'),
        unit(1, 'npc'))
    table2[i][[1]]$grobs[[1]]$children[[strip_child]]$just = just
    table2[i][[1]]$grobs[[1]]$children[[title_child]]$vp$parent$x = switch(just,
      'left' = unit(0, 'npc') + (0.5 * strip_width),
      'right' = unit(1, 'npc') - (0.5 * strip_width),
      unit(1, 'npc') - (0.5 * strip_width))
  }

  for (i in rev(seq_along(panel_col))) {
    col_ind <- panel_col[i] + col_shift
    # table <- gtable_add_cols(table, col_width[i], pos = col_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[, i], t = panel_row, l = col_ind + 1, clip = clip, name = paste0(name, "-", seq_along(panel_row), "-", i), z = z)
    }
  }
  table
}

weave_strips_row <- function(table, table2, row_shift, row_height, name, z = 1, clip = "off", just = "top") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t

  # rearrange strip height/position/justifications to overlay on panel
  for (i in 1:length(table2)) {
    strip_height = table2[i][[1]]$heights
    table2[i][[1]]$heights = unit(1, 'npc')

    # find the strip grob and modify it
    strip_child = grep(
      'strip.background',
      names(table2[1][[1]]$grobs[[1]]$children))
    title_child = grep('GRID.titleGrob',
      names(table2[i][[1]]$grobs[[1]]$children))

    # modify them
    table2[i][[1]]$grobs[[1]]$children[[strip_child]]$height = strip_height
    table2[i][[1]]$grobs[[1]]$children[[strip_child]]$y =
      switch(just, 'bottom' = unit(0, 'npc'), 'top' = unit(1, 'npc'),
        unit(1, 'npc'))
    table2[i][[1]]$grobs[[1]]$children[[strip_child]]$just = just
    table2[i][[1]]$grobs[[1]]$children[[title_child]]$vp$parent$y = switch(just,
      'bottom' = unit(0, 'npc') + (0.5 * strip_height),
      'top' = unit(1, 'npc') - (0.5 * strip_height),
      unit(1, 'npc') - (0.5 * strip_height))
  }

  for (i in rev(seq_along(panel_row))) {
    row_ind <- panel_row[i] + row_shift
    # table <- gtable_add_rows(table, row_height[i], pos = row_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[i, ], t = row_ind + 1, l = panel_col, clip = clip, name = paste0(name, "-", seq_along(panel_col), "-", i), z = z)
    }
  }
  table

}

# copied straight from ggplot2 because i don't know how to (or if i can) import them

convertInd <- function(row, col, nrow) {
  (col - 1) * nrow + row
}

is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")

weave_tables_col <- function(table, table2, col_shift, col_width, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_col))) {
    col_ind <- panel_col[i] + col_shift
    table <- gtable_add_cols(table, col_width[i], pos = col_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[, i], t = panel_row, l = col_ind + 1, clip = clip, name = paste0(name, "-", seq_along(panel_row), "-", i), z = z)
    }
  }
  table
}

weave_tables_row <- function(table, table2, row_shift, row_height, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_row))) {
    row_ind <- panel_row[i] + row_shift
    table <- gtable_add_rows(table, row_height[i], pos = row_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[i, ], t = row_ind + 1, l = panel_col, clip = clip, name = paste0(name, "-", seq_along(panel_col), "-", i), z = z)
    }
  }
  table
}

