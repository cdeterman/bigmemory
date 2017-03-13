################################################################################

library("bigmemory")
opt.save <- options(bigmemory.allow.dimnames=TRUE)
context("rownames with read.big.matrix")

tmp <- tempfile()
tmp.dir <- dirname(tmp)
tmp.file <- basename(tmp)
descfile <- paste0(tmp.file, ".desc")
rownames(iris) = paste0("row_", 1:nrow(iris))
txtfile <- paste0(tmp, ".txt")
write.table(iris[,-ncol(iris)], file = txtfile, 
            col.names=TRUE, row.names = TRUE, 
            sep = ' ', quote = FALSE)

################################################################################

bim <- read.big.matrix(txtfile, sep = ' ', header = TRUE, 
                       has.row.names = TRUE, type = "double")

test_that("Shared big.matrix has rownames", {
  expect_equal(rownames(bim), rownames(iris))
})

################################################################################

bim2 <- read.big.matrix(txtfile, sep = ' ', header = TRUE,
                        has.row.names = TRUE, backingfile = tmp.file,
                        backingpath = tmp.dir,
                        descriptorfile = descfile, type = "double")

test_that("Filebacked big.matrix has rownames", {
  expect_equal(rownames(bim2), rownames(iris))
})

################################################################################

bim3 <- attach.big.matrix(file.path(tmp.dir, descfile))

test_that("Filebacked big.matrix (from descfile) has rownames", {
  expect_equal(rownames(bim3), rownames(iris))
})

################################################################################

options(opt.save)

################################################################################
