context("Similarity functions.")

test_that("Similarity matrix is calculated correctly.", {
   m <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), nrow = 3, ncol = 3)
   res <- cosineMatrix(m)
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

  m <- testSimilarityModel()
  m <- m@sim
  # vector of skus that we want to base recommendations on.
  viewed.skus <- c("a", "b")

  # group is the named vector of groups that sku belongs to.
  # It can be product type, collection or any other way of categorising the skus.
  sku.groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")

  # we want to get 2 recommendations.
  values <- 2

  res <- recommendSimilar(m, viewed.skus, values, exclude.same = F, sku.groups)

  expect_identical(length(res), as.integer(2), "We got two recommedations")

  # we are expecting a and d to be recommended as they have high proximity to a and b .
  expect_equal(as.character(res), c("a", "b"), "We got products from diffrent groups")

  res <- recommendSimilar(m, viewed.skus, values, exclude.same = F)
  expect_equal(as.character(res), c("a", "b"), "We got products from the same group")

  res <- recommendSimilar(m, viewed.skus, values, exclude.same = T)
  expect_equal(as.character(res), c("d", "c"), "We got products where viewed skus are excluded")

  # Set group and exclude same SKUs
  res <- recommendSimilar(m, viewed.skus, values, exclude.same = T, sku.groups)
  expect_equal(as.character(res), c("d", "c"), "We get both skus as groups for c & d are different")

  # Setting same group for c & d
  sku.groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p3")
  res <- recommendSimilar(m, viewed.skus, values, exclude.same = T, sku.groups)
  expect_equal(as.character(res), c("d"), "We get only one skus as group for c & d are the same")

  viewed.skus <- c("a")
  res <- recommendSimilar(m, viewed.skus, 1, exclude.same = T)
  expect_equal(as.character(res), c("d"), "We get only one skus as requested")

  viewed.skus <- c("z") # sku does not exist in the matrix.
  res <- recommendSimilar(m, viewed.skus, 5, exclude.same = T)
  expect_identical(length(res), as.integer(0), "Result is empty for sku that is not in the similarity matrix")
  expect_warning(recommendSimilar(m, viewed.skus, 5, exclude.same = T), regexp = "skus are missing.*z$")
})

test_that("Filter for exclusion of same products", {
  res <- notInWhich(c("a", "b"), "b", TRUE)
  expect_identical(res, -2L, "Second item is removed")

  res <- notInWhich(c("a", "b"), "b", FALSE)
  expect_identical(res, 1:2, "Second item is not removed")

  res <- notInWhich(c("a", "b"), "c", FALSE)
  expect_identical(res, 1:2, "All items are kept")

  res <- notInWhich(c("a", "b"), c("a", "b"), TRUE)
  expect_identical(res, c(-1L, -2L), "All items are removed")

})

test_that("One per group filter", {
  groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")
  dt <- data.table(sku = c("a", "b", "d"), sim = c(0.1, 0.4, 0.9), key = "sku")
  res <- keepOnePerGroup(dt, groups)
  res <- res$sku
  expect_equal(sort(res), c("b", "d"), "Products with highest sim per group are selected")

  res <- keepOnePerGroup(dt, NULL)
  res <- res$sku
  expect_equal(sort(res), c("a", "b", "d"), "All products are kept if groupign is not provided")
})
