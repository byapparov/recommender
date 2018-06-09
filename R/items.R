#' Turns interactions data table into a matrix
#'
#' @export
#' @param product.hits data.table with columns:  visitor.id, product attribute
#' @return Returns sparse matrix for interactions between products within user sessions,
#'   meaning number of visitors that viewed/bought products together.
userProductHitsToMatrix <- function(product.hits) {
  sku.x <- sku.y <- NULL

  colnames(product.hits) <- c("visitor.id", "sku")

  product.matches <- merge(product.hits, product.hits, by = "visitor.id", allow.cartesian = TRUE)
  product.matches <- subset(product.matches, sku.x != sku.y)

  product.matrix <- dcast(product.matches, sku.x ~ sku.y, value.var = "visitor.id", fun.aggregate = length)
  product.matrix <- data.frame(product.matrix, check.names = F)
  rownames(product.matrix) <- product.matrix[, 1]
  product.matrix <- product.matrix[, -1]
  as.matrix(product.matrix)
}

#' Cosine similarity transformation for product hits
#' @export
#' @useDynLib recommender
#' @description Tranforms product hits matrix to product similarity matrix
#' @param m matrix of product hit counts that happened within a single user
#' @return product similarity matrix
cosineMatrix <- function(m) {
  res <- cosineCpp(m)
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  res
}
