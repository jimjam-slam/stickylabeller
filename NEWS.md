# stickylabeller 1.0.0

* **Breaking change:** the required arguments for [label_glue()] are now `row_template` and `col_template`, not `rows` and `cols`.
  - (This may affect users who used the package with [ggplot2::facet_grid()] and passed the templates as named arguments.)
* [label_glue()] now accepts a `summary_data` argument for passing additional summary data to the plot without joining it to the original data.
* [label_glue()] now passes dots onto [glue::glue_data()], allowing its other arguments to be used.

# stickylabeller 0.0.0.9100

* Initial release
