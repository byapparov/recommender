library(parallel)

#' @export
similarityRecommender <- function(data, filter = NULL) {
  m <- userProductHitsToMatrix(data)
  m <- cosineMatrix(m)
  if(is.null(filter)) filter <- colnames(m)
  m <- m[, filter]
  class(m) <- append(class(m), "similarity.recommender")
  return(m)
}

#' Predicts similarity score for new product hits data
#'
#' @importFrom stats predict
#' @export
#' @param object similarity model object
#' @param newdata product hits data
predict.similarity.recommender <- function(object, newdata) {
  score <- NULL
  colnames(newdata) <- c("visitor.id", "sku", "sku.rec")

  target.skus <- intersect(unique(newdata[, sku]), colnames(object))
  similarity <- melt(object[target.skus, , drop=FALSE], na.rm = T)
  if(nrow(similarity) == 0L) return (as.numeric(NULL))
  colnames(similarity) <- c("sku", "sku.rec", "score")
  similarity <- data.table(similarity, key = c("sku", "sku.rec"))
  scores <- similarity[newdata][, score]
  return(scores)
}

#' @export
expandHits <- function(object, data) {
  sku <- dummy <- NULL

  missing.skus <- setdiff(unique(data[, sku]), rownames(object))
  if(length(missing.skus) > 0) {
    warning("Following skus are missing from the similarity model: ", paste(missing.skus, collapse = ", "))
  }

  recommend <- data.table(sku.rec = colnames(object), key = "sku.rec")
  recommend[, dummy := 0L]
  data[, dummy := 0L]
  newdata <- merge(data, recommend, by = "dummy", allow.cartesian = T)
  newdata[, dummy := NULL]
  setkeyv(newdata, cols = c("sku", "sku.rec"))
  return(newdata)
}

#' @export
recommendSimilarProducts <- function(model, hits, exclude.same = TRUE,
                                     filter = makeRecommendationsFilter(NULL)) {
  visitor.id <- sku <- sku.rec <- sim <- group <- same <- NULL

  hits.l <- split(hits, by = "visitor.id")
  res <- mclapply(hits.l, function(visitor.hits) {
    newdata <- expandHits(model, visitor.hits)
    if(exclude.same) { # exclude seen products from recommendations
      newdata[, same := sku.rec %in% sku, visitor.id]
      newdata <- newdata[!same == TRUE][, same := NULL]
    }
    newdata$sim <- predict(model, newdata)

    # Only keep skus that are in the similarity matrix
    newdata <- newdata[!is.na(sim)]
    newdata <- newdata[, list(sim = mean(sim)), by = list(visitor.id, sku.rec)]
    setnames(newdata, "sku.rec", "sku")
    setkey(newdata, sku)
    newdata <- filter(newdata)
  })
  newdata <- rbindlist(res)
  setkey(newdata, sku)

  return (newdata)
}

#' @export
makeRecommendationsFilter <- function(groups, values = 20) {
  function(data) {
    visitor.id <- sku <- sim <- NULL

    res <- keepOnePerGroup(data, groups)
    # Limit results to the requested number of skus
    res <- res[order(sim, decreasing = T), head(.SD[, list(sku, sim)], values), visitor.id]
  }
}
