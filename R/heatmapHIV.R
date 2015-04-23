#' Produce a heatmap
#'
#' @param counts The filename of the RPKM or raw count matrix.
#' @param gene_list The filename of the gene list to subset the matrix before
#'                  producing the heatmap.
#' @return Invisibly return the \code{matrix} used to produce the heatmap.
#'
#' @examples
#' \dontrun{
#' heatmapHIV("RPKM.txt", "gene_list.txt")
#' }
#'
#' @export
#' @import gplots
heatmapHIV <- function(counts, gene_list) {
  counts <- read.table(counts, header = TRUE, stringsAsFactors = FALSE)
  gene_list <- read.table(gene_list, header = FALSE,
			  stringsAsFactors = FALSE)
  gene_list <- gene_list[,1]
  i <- rownames(counts) %in% gene_list
  counts <- counts[i,]
  counts <- as.matric(counts)
  heatmap.2(counts,
	    col = redgreen(75),
	    scale = "row",
	    key = TRUE,
	    keysize = 1.5,
	    density.info = "none",
	    cexCol = 0.4,
	    cexRow = 0.2,
	    trace = "none")
  invisible(counts)
}
