# Get and update heatmap from ggheatmap

Functions to get and modify heatmap data

## Usage

``` r
get_hmPlot(gghm)

update_hmPlot(gghm, new_hm)
```

## Arguments

- gghm:

  an object of class ggheatmap

- new_hm:

  an object of class ggplot, which corresponds to the heatmap portion of
  the ggheatmap to overwrite the current heatmap portion.

## Details

Allows the user to update graphical parameters of the heatmap by adding
onto it with ggplot syntax as they like. You first perform a
`new_hm <- get_hmPlot(gghm)` to get the object, which can then be
modified and re-written by setting `gghm <- update_hmPlot(gghm, new_hm)`
