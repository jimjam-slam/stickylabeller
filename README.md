

<!-- README.md is generated from README.qmd. Please edit that file -->

# stickylabeller <img src="man/figures/logo.svg" align="right" width="180px" style="padding-left: 1rem;" />

[![](https://jimjam-slam.r-universe.dev/stickylabeller/badges/version.png)](https://jimjam-slam.r-universe.dev/stickylabeller)

The `stickylabeller` package helps you label the facets in your ggplot2
plots using
[`glue`](https://cran.r-project.org/web/packages/glue/index.html).

## Installation

Install `stickylabeller` from my R-Universe:

``` r
install.packages("stickylabeller", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
```

## Use

The package has just one function: `label_glue`. Give it a string
template to be processed by `glue`, and it’ll return a labelling
function that you can pass to `facet_*`:

``` r
library(ggplot2)
library(stickylabeller)

# here's some example data: some random points in groups
mydf <- data.frame(
  x = 1:90,
  y = rnorm(90),
  red = rep(letters[1:3], 30),
  blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))

# and here's a labelled plot!
ggplot(mydf) +
  geom_point(aes(x = x, y = y)) +
  facet_wrap(
    vars(red, blue),
    labeller = label_glue("Red is {red}\nand blue is {blue}"))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" style="width:100.0%"
alt="facet_wrap labelled with two facet column values" />

``` r
ggplot(mydf) +
  geom_point(aes(x = x, y = y)) +
  facet_grid(
    vars(red), vars(blue),
    labeller = label_glue(
      "Red is {red}",
      "Blue is {blue}"))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" style="width:100.0%"
alt="facet_grid labelled with one facet column value on each margin" />

Your `label_glue` labeller can refer to any of the data frame columns
included in the facetting formula. It can also use those columns in
expressions, like:

``` r
my_labeller <- label_glue("Red is {toupper(red)}\nand blue is {blue}")
```

### Numbering sequential facets

As well as the columns you include in the facetting specification,
`stickylabeller` includes a few helper columns:

- `.n` numbers the facets numerically: `1`, `2`, `3`…
- `.l` numbers the facets using lowercase letters: `a`, `b`, `c`…
- `.L` numbers the facets using uppercase letters: `A`, `B`, `C`…
- `.r` numbers the facets using lowercase Roman numerals: `i`, `ii`,
  `iii`…
- `.R` numbers the facets using uppercase Roman numerals: `I`, `II`,
  `III`…

So you can automatically number your facets like:

``` r
ggplot(mydf) +
  geom_point(aes(x = x, y = y)) +
  facet_wrap(
    vars(red, blue),
    labeller = label_glue("({.l}) Red is {toupper(red)}\nand blue is {blue}"))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="width:100.0%"
alt="facet_wrap labelled with sequential lowercase letters: (a), (b), (c) and so on" />

#### Limitations

- `.n`, `.l` and `.L` only work with `facet_wrap` for now. See [Issue
  \#1](https://github.com/jimjam-slam/stickylabeller/issues/1).

### Including summary statistics in facet labels

As of stickylabeller 1.0, `label_glue` now accepts a `summary_data`
argument. This accepts a data frame of pre-computed summary data for the
facets; it should include the facet grouping columns, plus any other
variables you’d like to reference in the templates.

For example:

``` r
library(magrittr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# summarise the data
mydf_stats = mydf |>
  group_by(red, blue) |>
  summarise(
    mean_y = sprintf("%#.2f", mean(y)),
    sd_y = sprintf("%#.2f", sd(y))) |>
  ungroup()
#> `summarise()` has grouped output by 'red'. You can override using the `.groups`
#> argument.

# pass the summary data onto label_glue
ggplot(mydf) +
  geom_point(aes(x = x, y = y)) +
  facet_wrap(
    vars(red, blue),
    labeller = label_glue(
      "({.L}) Red = {red}, blue = {blue}\n(mean = {mean_y}, SD = {sd_y})",
      summary_data = mydf_stats))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" style="width:100.0%"
alt="facet_wrap labelled with two facet column values and two summary statistics, each numbered (a), (b), (c) and so on" />

Have fun! If you hit any snags, please feel free to [file an issue
here](https://github.com/jimjam-slam/stickylabeller/issues) so that I
can get on it! \<3
