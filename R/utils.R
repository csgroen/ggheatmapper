#' Get data from ggheatmap
#'
#' These three functions help get data that may be useful for further plotting
#' from the ggheatmap object.
#'
#' `gghm_Data()` recovers the table with an ID column named `observations`, which
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
gghm_Data <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$data
}

#' @export
#' @rdname gghm_Data
gghmData <- function(gghm) {
   gghm_Data(gghm)
}

#' @export
#' @rdname gghm_Data
gghm_rowLevels <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$gghm$row_levels
}
#' @export
#' @rdname gghm_Data
gghm_colLevels <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$gghm$col_levels
}

#' Get and update heatmap from ggheatmap
#'
#' Functions to get and modify heatmap data
#'
#' Allows the user to update graphical parameters of the heatmap by adding onto
#' it with ggplot syntax as they like. You first perform a `new_hm <- get_gghm(gghm)`
#' to get the object, which can then be modified and re-written by setting
#' `gghm <- update_gghm(gghm, new_hm)`
#'
#' @param gghm an object of class ggheatmap
#' @param new_hm an object of class ggplot, which corresponds to the heatmap
#' portion of the ggheatmap to overwrite the current heatmap portion.
#'
#' @export
gghm_getHMplot <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) return(gghm$gghm$plots$hm)
}

#' @export
#' @rdname gghm_getHMplot
gghm_updateHMplot <- function(gghm, new_hm) {
    if("ggheatmap" %in% class(gghm)) {
        gghm_info <- gghm$gghm
        gghm_info$plots$hm <- new_hm
        new_gghm <- wrap_plots(gghm_info$plots,
                              design = gghm_info$design,
                              widths = gghm_info$params$widths,
                              heights = gghm_info$params$heights)
        gghm$gghm <- gghm_info
        gghm$data <- gghm$data
        return(gghm)

    }

}
