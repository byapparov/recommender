context("Similarity Class")

test.sim.model <- testSimilarityModel()

test_that("Test simlarity model and predict", {
  user.hits <- data.table(users =    c("u1", "u2", "u1", "u3", "u2", "u1"),
                          products = c("p1", "p2", "p3", "p2", "p3", "p4"))
  model <- similarityRecommender(user.hits)

  new.hits <- data.table(users = c("u5", "u5", "u6", "u7", "u6"),
                         sku =   c("p3", "p4", "p1", "p1", "p2"))

  new.hits.expanded <- expandHits(model, new.hits)

  new.hits.expanded$sim <- predict(model, new.hits.expanded)
  expect_equal(new.hits.expanded[sku == "p1" & sku.rec == "p3", sim], rep(0.408, 2), tolerance = 1e-2)
})

test_that("Test expanding of hits to new dataset", {
  groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")
  page.views <- data.table(visitor.id = c("u1", "u1"), sku = c("a", "b"))
  newdata <- expandHits(test.sim.model, page.views)
  newdata$sim <- predict(test.sim.model, newdata)

  # Hit to a product that is missing from the model
  page.views <-  data.table(visitor.id = c("u1"), sku = c("z"))
  newdata <- expect_warning(expandHits(test.sim.model, page.views), regexp = "skus are missing.*z$")

})

test_that("Test predict for similarity model", {

  groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")
  page.views <- data.table(visitor.id = c("u1", "u1"), sku = c("a", "b"))
  newdata <- expandHits(test.sim.model, page.views)
  filter <- makeRecommendationsFilter(groups, values = 2)
  res <- recommendSimilarProducts(test.sim.model, page.views, exclude.same = T, filter = filter)

  expect_equal(sort(res$sku), c("c", "d"), "As c and d are in different groups we get both")

  # Setting same group for c & d
  groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p3")
  filter <- makeRecommendationsFilter(groups, values = 2)
  res <- recommendSimilarProducts(test.sim.model, page.views, exclude.same = T, filter = filter)
  expect_equal(res$sku, c("d"), "As c and d are in the same group we get one")

  # Recommend single sku:
  page.views <- data.table(visitor.id = c("u1"), sku = c("a"))
  newdata <- expandHits(test.sim.model, page.views)
  filter <- makeRecommendationsFilter(groups, values = 1)
  res <- recommendSimilarProducts(test.sim.model, page.views, TRUE, filter)
  expect_identical(nrow(res), 1L, "One row retured as requested")

  # SKU does not exist in the matrix:
  page.views <-  data.table(visitor.id = c("u1"), sku = c("z"))
  res <- recommendSimilarProducts(test.sim.model, page.views)
  expect_identical(nrow(res), 0L, "Result is empty for the SKU that is not in the similarity model")

  # Multiple users
  groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")
  page.views <- data.table(
    user = c("u1", "u1", "u2", "u3", "u3", "u3"),
    sku = c("a", "b",   "c",  "a",  "a", "d")
  )
  filter <- makeRecommendationsFilter(groups, values = 1)
  res <- recommendSimilarProducts(
    test.sim.model,
    page.views,
    exclude.same = T,
    filter = filter
  )
  expect_identical(nrow(res), 3L, "One result per user is returned")

})

context("Complimentary products")

test_that("Test recommendations for products", {
  products <- data.table(sku = c("a", "b", "c", "d"),
                         type = c("p1", "p2", "p3", "p1"))

  affinity <- recommendComplimentaryProducts(test.sim.model, products, "type", limit = 2)
  expect_identical(nrow(affinity), 8L,
                   label = "Two recommendations returned per product")
  expect_equal(affinity[sku == "b", sku.rec], c("d", "c"),
               label = "a is not recommended for b as d has higher similarity and the same type")

  expect_equal(affinity[sku == "a", sku.rec], c("b", "c"),
               label = "a is not recommended for b as d has higher similarity and the same type")

  affinity <- recommendComplimentaryProducts(test.sim.model, products, limit = 2)
  expect_equal(affinity[sku == "b", sku.rec], c("d", "a"),
               label = "a is recommended for b as when type filter is not provided")

  affinity <- recommendComplimentaryProducts(test.sim.model, products, "type", limit = 1)
  expect_identical(nrow(affinity), 4L,
                   label = "One recommendation returned per product")

})
