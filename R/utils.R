#' Get data from ggheatmap
#'
#' These three functions help get data that may be useful for further plotting
#' from the ggheatmap object.
#'
#' `get_data()` recovers the table with an ID column named `observations`, which
#' if a factor that uses the same ordered levels as the heatmap. Those levels are
#' also provided by calling `get_colLevels()`. For plots where you'd like to
#' align the rows to the rows of the heatmap, `get_rowLevels()` provides the
#' order of the rows in the heatmap.
#'
#' @param gghm An object of class `ggheatmap`
#'
#' @export
#'
#'
get_data <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$data
}

#' @export
#' @rdname get_data
gghmData <- function(gghm) {
    get_data(gghm)
}

#' @export
#' @rdname get_data
get_rowLevels <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$gghm$row_levels
}
#' @export
#' @rdname get_data
get_colLevels <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) gghm$gghm$col_levels
}

#' Get and update heatmap from ggheatmap
#'
#' Functions to get and modify heatmap data
#'
#' Allows the user to update graphical parameters of the heatmap by adding onto
#' it with ggplot syntax as they like. You first perform a `new_hm <- get_hmPlot(gghm)`
#' to get the object, which can then be modified and re-written by setting
#' `gghm <- update_hmPlot(gghm, new_hm)`
#'
#' @param gghm an object of class ggheatmap
#' @param new_hm an object of class ggplot, which corresponds to the heatmap
#' portion of the ggheatmap to overwrite the current heatmap portion.
#'
#' @export
get_hmPlot <- function(gghm) {
    if("ggheatmap" %in% class(gghm)) return(gghm$gghm$plots$hm)
}

#' @export
#' @rdname get_hmPlot
#' @importFrom patchwork wrap_plots
update_hmPlot <- function(gghm, new_hm) {
    if("ggheatmap" %in% class(gghm)) {
        gghm_info <- gghm$gghm
        gghm_info$plots$hm <- new_hm
        new_gghm <- wrap_plots(gghm_info$plots,
                              design = gghm_info$design,
                              widths = gghm_info$params$widths,
                              heights = gghm_info$params$heights)
        new_gghm$gghm <- gghm_info
        new_gghm$gghm <- gghm_info
        new_gghm$data <- gghm$data
        class(new_gghm) <- append(class(new_gghm), "ggheatmap")
        return(new_gghm)
    }
}

#' A list with data extracted from the TCGA-BLCA cohort of patients with
#' muscle-invasive bladder cancer
#'
#' @format A list comprised of:
#' 1. `gexp`: A matrix of gene expression of 60 genes for 100 patients
#' 2. `sample_annot`: A data.frame with clinical and molecular annotations
#' 3. `gene_annotation`: A data.frame with annotation for the 60 genes
#'
#' @source 10.1016/j.cell.2017.09.007
"tcgaBLCA_ex"
