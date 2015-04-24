#' Produce a heatmap
#'
#' @param counts The filename of the RPKM or raw count matrix.
#' @param gene_list The filename of the gene list to subset the matrix before
#'                  producing the heatmap.
#' @param filter Order on a subset of column matching value of \code{filter}.
#'               When \code{NULL}, a clustering of the columns will be done
#'               instead. Default: \code{NULL}.
#' @param heatmap Should the function produce an heatmap. Default: TRUE
#' @return Invisibly return the \code{matrix} used to produce the heatmap.
#'
#' @examples
#' \dontrun{
#' heatmapHIV("RPKM.txt", "gene_list.txt")
#' }
#'
#' @export
#' @import gplots
heatmapHIV <- function(counts, gene_list, filter = NULL, heatmap = TRUE) {
  # Check param
  if (!file.exists(counts)) {
    stop(paste("counts file not found:", counts))
  }
  if (!file.exists(gene_list)) {
    stop(paste("gene_list file not found:", gene_list))
  }
  if (!is.null(filter) & class(filter) != "character") {
    stop("filter param must be NULL or of class character")
  }
  if (!is.null(filter) & filter == "") {
    stop("filter param must be at least one character long")
  }
  if (!is.logical(heatmap)) {
    stop("heatmap param must be TRUE of FALSE")
  }

  # Import the datasets
  counts <- read.table(counts, header = TRUE, stringsAsFactors = FALSE)
  gene_list <- read.table(gene_list, header = FALSE,
			  stringsAsFactors = FALSE)
  gene_list <- gene_list[,1]

  # Subset rpkm based on gene_list
  i <- rownames(counts) %in% gene_list
  counts <- counts[i,]

  # Set the correct value for dendrogram
  dendrogram = "column" # Default
  ## If there is a value for the filter param, we won't be doing
  ## any clustering (i.e.: the value will be sorted based on the value
  ## of filter
  if (!is.null(filter)) {
    dendrogram = "none"
    i <- grepl(filter, colnames(counts), ignore.case = TRUE)
    if (sum(i) == 0) {
      warning("filter value did not match any column name.")
    } else {
      # Group the columns that will be used to order the data.frame
      counts <- counts[,c(which(i), which(!i))]
      # Order the data.frame
      if (sum(i) > 1) {
        i <- order(rowMeans(counts[,i]), decreasing = TRUE)
      } else {
        i <- order(counts[,i], decreasing = TRUE)
      }
      counts <- counts[i,]
    }
  }

  # Produce the matrix and the heatmap
  counts <- as.matrix(counts)
  if (heatmap == TRUE) {
    heatmap.2(counts,
              col = redgreen(75),
              scale = "row",
              key = TRUE,
              keysize = 1.5,
              density.info = "none",
              cexCol = 0.4,
              cexRow = 0.2,
              trace = "none",
              dendrogram = dendrogram)
  }
  invisible(counts)
}
