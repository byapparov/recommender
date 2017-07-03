context("Similarity Class")

test_that("Test simlarity model and predict", {
  user.hits <- data.table(users =    c("u1", "u2", "u1", "u3", "u2", "u1"),
                          products = c("p1", "p2", "p3", "p2", "p3", "p4"))
  model <- similarityRecommender(user.hits)

  new.hits <- data.table(users = c("u5", "u5", "u6", "u7", "u6"),
                         sku =   c("p3", "p4", "p1", "p1", "p2"))

  new.hits.expanded <- expandVisits(model, new.hits)

  scores <- predict(model, new.hits.expanded)
  expect_equal(scores[sku == "p1" & sku.rec == "p3", sim], rep(0.408, 2), tolerance = 1e-2)
})
