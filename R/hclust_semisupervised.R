#' Semi-supervised hierarchical clustering
#'
#' Semi-supervised hierarchical clustering by chosen groups with hclust.
#'
#' @param data a data.frame to be clustered by rows
#' @param groups a list of vectors. If we unlist(groups), all elements must be
#'   present in the rownames of data. Each vector in the list will be treated as
#'   a separate group for the hierarchical clustering, and rejoined in order at
#'   the end.
#' @param dist_method a distance computation method. Must be one of "euclidean",
#' "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman"
#' @param dist_p the power of the Minkowski distance, if chosen dist_method is "minkowski"
#' @param hclust_method an agglomeration method. Should be a method supported by
#' hclust, one of:  "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA),
#' "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#' @param cor_use If using correlation as distance, chooses the method for computing
#' covariances in the presence of missing values. See [stats::cor].
#' @param merge_height If provided, dendrogramws will be merged at that height.
#'
#' @importFrom stats dist hclust cor as.dendrogram
#'
#' @return hclust_semisupervised returns a list. The first element of the list
#' is the data, reordered so that the merged hclust object will work. The second
#' element is the result of the semi-supervised hierarchical clustering.
#' @export

hclust_semisupervised <- function(data, groups, dist_method = "euclidean",
                      dist_p = 2, hclust_method = "complete", cor_use = "everything",
                      merge_height = NA) {
    #-- Group checks
    if (!all(unlist(groups) %in% rownames(data))) {
        stop("vectors in `groups` must contain rownames of `data`")
    }
    if(anyDuplicated(unlist(groups))){
        stop("`groups` can't have elements duplicated within or in different
             groups")
    }

    #-- dist_method checks
    alldists <- c("euclidean", "maximum", "manhattan", "canberra", "binary",
                  "minkowski", "pearson", "spearman", "kendall")
    if(!(dist_method %in% alldists)) {
        stop("`dist_method` must be one of: `euclidean`, `maximum`, `manhattan`,
             `canberra`, `binary`, `minkowski`, `pearson`, `spearman` or `kendall`.")
    }

    # #-- Use check
    # if(dist_method %in% c("pearson", "spearman", "kendall")) {
    #     alluse <- c("everything", "all.obs", "complete.obs", "na.or.complete",
    #                 "pairwise.complete.obs")
    #     if(!(cor_use %in% alluse)) {
    #         stop('`cor_use` must be one of "everything", "all.obs", "complete.obs",
    #              "na.or.complete", or "pairwise.complete.obs".')
    #     }
    # }

    #-- Get groups with 1 member
    g_size <- sapply(groups, length)
    if (any(g_size == 1)) {
        # s_groups <- groups[g_size == 1]
        groups <- groups[g_size != 1]

    }

    #-- Make distance matrices
    if (dist_method %in% c("pearson", "spearman", "kendall")) {
        distlist <- lapply(groups, function(group) {
            as.dist(1 - cor(t(data[group,]), method = dist_method, use = cor_use))
        })
    } else {
        distlist <- lapply(groups, function (group) {
            dist(data[group,], method = dist_method, p = dist_p)
        })
    }
    #-- Use hclust
    hclist <- lapply(distlist, hclust, method = hclust_method)


    hc <- .merge_hclust(hclist, height = merge_height)

    #-- Join groups with one element
    if(exists("s_groups")) {
        # s_groups <- unlist(s_groups)
        # if (s_groups > 1) s_hc <- hclust(dist(data[s_groups,]))
        # else {
        #     s_hc <- list(merge = matrix(c(1), ncol = 1), height = max(hc$height + sd(hc$height)),
        #          order = 1, labels = s_groups, call = NA, method = NA,
        #          dist.method = NA)
        #     class(s_hc) <- "hclust"
        # }
        # hc <- .merge_hclust(list(hc, s_hc), height = merge_height)
        # data_reordered <- data[c(unlist(groups), s_groups),]
    } else {
        data_reordered <- data[unlist(groups),]
    }

    data_reordered <- data[unlist(groups),]

    return(list(data = data_reordered,
                hclust = hc))
}

#' @importFrom stats as.hclust
.merge_hclust <- function(hclist, height) {
    #-- Check
    if(!is.list(hclist)) {
        stop("`hclist` must be a list.")
    }
    if(!all(sapply(hclist, class) == "hclust")){
        stop("All objects in `hclist` must be `hclust-class`")
    }

    #-- Merge
    d <- as.dendrogram(hclist[[1]])
    for (i in 2:length(hclist)) {
        if(is.na(height)) {
            d <- merge(d, as.dendrogram(hclist[[i]]), adjust = "add.max")
            } else {
            d <- merge(d, as.dendrogram(hclist[[i]]), adjust = "add.max", height = height)
            }
    }
    as.hclust(d)
}
