test_that("heatmapHIV - RPKM file does not exists", {
  gene_list <- get_toy_gene_list()
  msg <- "counts file not found: invalid_file"
  expect_error(heatmapHIV("invalid_file", gene_list), msg)
})

test_that("heatmapHIV - gene_list file does not exists", {
  rpkm <- get_toy_rpkm()
  msg <- "gene_list file not found: invalid_file"
  expect_error(heatmapHIV(rpkm, "invalid_file"), msg)
})

test_that("heatmapHIV - filter - invalid pattern", {
  rpkm <- get_toy_rpkm()
  gene_list <- get_toy_gene_list()
  msg <- "filter value did not match any column name."
  expect_warning(heatmapHIV(rpkm, gene_list, filter = "invalid_filter", heatmap = FALSE), msg)
})

test_that("heatmapHIV - filter - invalid class", {
  rpkm <- get_toy_rpkm()
  gene_list <- get_toy_gene_list()
  msg <- "filter param must be NULL or of class character"
  expect_error(heatmapHIV(rpkm, gene_list, filter = 2), msg)
})

test_that("heatmapHIV - filter - invalid length", {
  rpkm <- get_toy_rpkm()
  gene_list <- get_toy_gene_list()
  msg <- "filter param must be at least one character long"
  expect_error(heatmapHIV(rpkm, gene_list, filter = ""), msg)
})

test_that("heatmapHIV - filter 1 column", {
  rpkm <- get_toy_rpkm()
  gene_list <- get_toy_gene_list()
  res <- heatmapHIV(rpkm, gene_list, filter = "Exp1", heatmap = FALSE)
  expect_identical(class(res), "matrix")
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 3)
  expect_equal(as.vector(res), c(10,5,5,15,5,25,5,30,5))
})

test_that("heatmapHIV - filter 2 column", {
  rpkm <- get_toy_rpkm()
  gene_list <- get_toy_gene_list()
  res <- heatmapHIV(rpkm, gene_list, filter = "Exp", heatmap = FALSE)
  expect_identical(class(res), "matrix")
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 3)
  expect_equal(as.vector(res), c(5,10,5,25,15,5,5,5,30))
})

test_that("heatmapHIV - heatmap - invalid class", {
  rpkm <- get_toy_rpkm()
  gene_list <- get_toy_gene_list()
  msg <- "heatmap param must be TRUE of FALSE"
  expect_error(heatmapHIV(rpkm, gene_list, heatmap = 3))
})
