# Get data from ggheatmap

These three functions help get data that may be useful for further
plotting from the ggheatmap object.

## Usage

``` r
get_data(gghm)

gghmData(gghm)

get_rowLevels(gghm)

get_colLevels(gghm)
```

## Arguments

- gghm:

  An object of class `ggheatmap`

## Details

`get_data()` recovers the table with an ID column named `observations`,
which if a factor that uses the same ordered levels as the heatmap.
Those levels are also provided by calling `get_colLevels()`. For plots
where you'd like to align the rows to the rows of the heatmap,
`get_rowLevels()` provides the order of the rows in the heatmap.
