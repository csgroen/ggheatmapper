# Semi-supervised hierarchical clustering

Semi-supervised hierarchical clustering by chosen groups with hclust.

## Usage

``` r
hclust_semisupervised(
  data,
  groups,
  dist_method = "euclidean",
  dist_p = 2,
  hclust_method = "complete",
  cor_use = "everything",
  merge_height = NA
)
```

## Arguments

- data:

  a data.frame to be clustered by rows

- groups:

  a list of vectors. If we unlist(groups), all elements must be present
  in the rownames of data. Each vector in the list will be treated as a
  separate group for the hierarchical clustering, and rejoined in order
  at the end.

- dist_method:

  a distance computation method. Must be one of "euclidean", "maximum",
  "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman"

- dist_p:

  the power of the Minkowski distance, if chosen dist_method is
  "minkowski"

- hclust_method:

  an agglomeration method. Should be a method supported by hclust, one
  of: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA),
  "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

- cor_use:

  If using correlation as distance, chooses the method for computing
  covariances in the presence of missing values. See
  [stats::cor](https://rdrr.io/r/stats/cor.html).

- merge_height:

  If provided, dendrogramws will be merged at that height.

## Value

hclust_semisupervised returns a list. The first element of the list is
the data, reordered so that the merged hclust object will work. The
second element is the result of the semi-supervised hierarchical
clustering.
