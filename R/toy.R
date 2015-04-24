#' Get toy rpkm filename
#'
#' @return The name of a toy rpkm file
#'
#' @examples
#' toy_rpkm <- get_toy_rpkm()
#'
#' @export
get_toy_rpkm <- function() {
  system.file("extdata/toy.rpkm", package = "heatmapHIV")
}

#' Get toy gene list filename
#'
#' @return The name of a toy gene list file
#'
#' @examples
#' toy_gene_list <- get_toy_gene_list()
#'
#' @export
get_toy_gene_list <- function() {
  system.file("extdata/toy.tsv", package = "heatmapHIV")
}
