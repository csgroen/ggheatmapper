#' Get data from ggheatmap
#' 
#' These three functions help get data that may be useful for further plotting
#' from the ggheatmap object.
#' 
#' `gghmData()` recovers the table with an ID column named `observations`, which
#' if a factor that uses the same ordered levels as the heatmap. Those levels are
#' also provided by calling `gghm_colLevels()`. For plots where you'd like to
#' align the rows to the rows of the heatmap, `gghm_rowLevels()` provides the
#' order of the rows in the heatmap.
#' 
#' @param gghm An object of class `ggheatmap`
#' 
#' @export
#' 
#' 
gghmData <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$data
}
#' @export
#' @rdname gghmData
gghm_rowLevels <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$gghm$row_levels
}
#' @export
#' @rdname gghmData
gghm_colLevels <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$gghm$col_levels
}