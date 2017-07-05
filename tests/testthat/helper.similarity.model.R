testSimilarityModel <- function() {
  test.sim.matrix <- c(1.00, 0.60, 0.40, 0.62,
                       0.60, 1.00, 0.23, 0.80,
                       0.40, 0.23, 1.00, 0.26,
                       0.62, 0.80, 0.26, 1.00 )

  test.sim.matrix <- matrix(test.sim.matrix, nrow = 4, ncol = 4)
  colnames(test.sim.matrix) <- rownames(test.sim.matrix) <- c("a", "b", "c", "d")
  model <- new("similarity.recommender", sim = test.sim.matrix)
  return(model)
}
