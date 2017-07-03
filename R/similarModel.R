

similarityRecommender <- function(data, filter = NULL) {
  m <- userProductHitsToMatrix(data)
  m <- cosineMatrix(m)
  if(is.null(filter)) filter <- colnames(m)
  m <- m[, filter]
  class(m) <- append(class(m), "similarity.recommender")
  return(m)
}

predict.similarity.recommender <- function(object, newdata) {
  similarity <- melt(object[, , drop=FALSE], na.rm = T)
  colnames(similarity) <- c("sku", "sku.rec", "sim")
  similarity <- data.table(similarity, key = c("sku", "sku.rec"))
  colnames(newdata) <- c("user", "sku", "sku.rec")
  setkeyv(newdata, cols <- c("sku", "sku.rec"))
  scores <- similarity[newdata]
  return(scores)
}

expandVisits <- function(object, data) {
  recommend <- data.table(sku.rec = colnames(object), key = "sku.rec")
  recommend[, dummy := 0L]
  data[, dummy := 0L]
  newdata <- merge(data, recommend, by = "dummy", allow.cartesian = T)
  newdata[, dummy := NULL]
  return(newdata)
}
