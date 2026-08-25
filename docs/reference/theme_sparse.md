# ggheatmap additional themes

These themes are provided as helpers to build panels with `ggheatmap`.
They are all built on top of
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Usage

``` r
theme_sparse(...)

theme_sparse2(...)

theme_sparse3(...)

theme_quant(...)

theme_scatter(...)

theme_clean(...)
```

## Arguments

- ...:

  all arguments passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Details

`theme_sparse()` is the baseline simple and spartan theme to build plots
that align with columns of `ggheatmap`s. `theme_sparse2()` and
`theme_sparse3()` are designed for plots that will align to rows of the
`ggheatmap`. `theme_quant()` adds horizontal lines to `theme_sparse()`
and can be a useful upgrade for displaying numeric values in the y-axis.
`theme_scatter()` and `theme_clean()` are all-purpose themes for
building ggplots.
