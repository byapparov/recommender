context("Similarity functions.")

test.sim.matrix <- c(1.00, 0.52, 0.41, 0.52,
                     0.60, 0.60, 0.23, 0.26,
                     0.40, 0.30, 0.83, 0.26,
                     0.62, 0.80, 0.25, 0.16 )

test.sim.matrix <- matrix(test.sim.matrix, nrow = 4, ncol = 4)
colnames(test.sim.matrix) <- rownames(test.sim.matrix) <- c("a", "b", "c", "d")

test_that("Similarity matrix is calculated correctly.", {
   m <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), nrow = 3, ncol = 3)
   res <- cosineCpp(m)
   expect_identical(res[1, 3], 0, "Products are ortoganal, cosine is zero.")
   expect_identical(res[2, 2], 1, "Product self cosine is one.")
   expect_identical(res[2, 3], 0.5, "Product have overlapping values.")

   # test that colnames and rownames are not changed
   colnames(m) <- rownames(m) <- c("a", "b", "c")

   res <- cosineMatrix(m)

   expect_equal(colnames(res), colnames(m), "Column names in result are same as original matrix.")
   expect_equal(rownames(res), rownames(m), "Row names in result are same as original matrix.")
})

test_that("User hits table produces correct sparce matrix", {
  user.hits <- data.table(users = c("u1", "u2", "u1"), products = c("p1", "p2", "p3"))
  res <- userProductHitsToMatrix(user.hits)
  expect_identical(res["p1", "p3"], as.integer(1), "Products p1 & p3 were hit by one user(u1)")
  expect_true(!("p2" %in% colnames(res)), "Product p2 was not hit by any users with any other product.")

})

test_that("Recommendations work with group", {

  m <- test.sim.matrix

  # vector of skus that we want to base recommendations on.
  viewed.skus <- c("a", "b")

  # group is the named vector of groups that sku belongs to.
  # It can be product type, collection or any other way of categorising the skus.
  sku.groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")

  # we want to get 2 recommendations.
  values <- 2

  res <- getSimilarProducts(m, viewed.skus, values, exclude.same = F, sku.groups)

  expect_identical(length(res), as.integer(2), "We got two recommedations")

  # we are expecting a and d to be recommended as they have high proximity to a and b .
  expect_equal(as.character(res), c("a", "b"), "We got products from diffrent groups")

  res <- getSimilarProducts(m, viewed.skus, values, exclude.same = F)
  expect_equal(as.character(res), c("a", "d"), "We got products from the same group")

  res <- getSimilarProducts(m, viewed.skus, values, exclude.same = T)
  expect_equal(as.character(res), c("d", "c"), "We got products where viewed skus are excluded")

  # Set group and exclude same SKUs
  res <- getSimilarProducts(m, viewed.skus, values, exclude.same = T, sku.groups)
  expect_equal(as.character(res), c("d", "c"), "We get both skus as groups for c & d are different")

  # Setting same group for c & d
  sku.groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p3")
  res <- getSimilarProducts(m, viewed.skus, values, exclude.same = T, sku.groups)
  expect_equal(as.character(res), c("d"), "We get only one skus as group for c & d are the same")

  viewed.skus <- c("a")
  res <- getSimilarProducts(m, viewed.skus, 1, exclude.same = T)
  expect_equal(as.character(res), c("d"), "We get only one skus as requested")

  viewed.skus <- c("z") # sku does not exist in the matrix.
  res <- getSimilarProducts(m, viewed.skus, 5, exclude.same = T)
  expect_identical(length(res), as.integer(0), "Result is empty for sku that is not in the similarity matrix")
  expect_warning(getSimilarProducts(m, viewed.skus, 5, exclude.same = T))
})

test_that("Weighting a similarity matrix", {
    
  m <- test.sim.matrix
  w <- c("a" = 1, "e" = 0.22, "b" = 0.75, "c" = 0.5, "d" = 0.25)
  res <- abjustSimMatrix(m, w)

  expect_identical(m[, 1], res[, 1], "First column returned correctly with no changes")
  expect_identical(m[, 2]*0.75, res[, 2], "Order is kept and calculation is correct")

  w <- c("a" = 1)
  res <- abjustSimMatrix(m, w)
  expect_identical(m[, 3] * 0, res[, 3], "Defaulting is working as expected")

  w <- c("b" = 0.2, "a" = 1)
  res <- abjustSimMatrix(m, w)
  expect_identical(m[, 2] * 0.2, res[, 2], "Ordering is kept intact")
  
})
