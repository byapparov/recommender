library(methods)
library(parallel)

#' S4 class that represents similar products recommendation model
#' @export
#' @param sim cosine matrix of product similarity
setClass("similarity.recommender", representation(sim = "matrix"))

#' Factory for the similarity recommendation model
#' @export
#' @param data product hit stream with visitor.id and sku
#' @param filter allows to reduce recommendations to a given set
similarityRecommender <- function(data, filter = NULL) {
  m <- userProductHitsToMatrix(data)
  m <- cosineMatrix(m)
  if(is.null(filter)) filter <- colnames(m)
  m <- m[, filter]
  model <- new("similarity.recommender", sim = m)
  return(model)
}


#' Makes similarity score predictions based on the similarity.recommender model
#' @param object a fitted similarity model object.
#' @param newdata use product hits.
#' @return \code{predict} returns a vector with predicted values.
#' @rdname similarity
#' @aliases predict,similarity-recommender
#' @export
setMethod("predict", signature(object = "similarity.recommender"),
          function(object, newdata) {
            similarity.predictor(object, newdata)
          })


#' Predicts similarity score for new product hits data
#'
#' @importFrom stats predict
#' @export
#' @param object similarity model object
#' @param newdata product hits data
similarity.predictor <- function(object, newdata) {
  sku <- score <- NULL
  colnames(newdata) <- c("visitor.id", "sku", "sku.rec")

  target.skus <- intersect(unique(newdata[, sku]), colnames(object@sim))
  similarity <- melt(object@sim[target.skus, , drop=FALSE], na.rm = T)
  if(nrow(similarity) == 0L) return (as.numeric(NULL))
  colnames(similarity) <- c("sku", "sku.rec", "score")
  similarity <- data.table(similarity, key = c("sku", "sku.rec"))
  scores <- similarity[newdata][, score]
  return(scores)
}

#' Expand visitor product hits data to dataset for prediction
#' @export
#' @param object similarity model
#' @param data visitor page hits
expandHits <- function(object, data) {
  sku <- dummy <- NULL

  missing.skus <- setdiff(unique(data[, sku]), rownames(object@sim))
  if(length(missing.skus) > 0) {
    warning("Following skus are missing from the similarity model: ", paste(missing.skus, collapse = ", "))
  }

  recommend <- data.table(sku.rec = colnames(object@sim), key = "sku.rec")
  recommend[, dummy := 0L]
  data[, dummy := 0L]
  newdata <- merge(data, recommend, by = "dummy", allow.cartesian = T)
  newdata[, dummy := NULL]
  setkeyv(newdata, cols = c("sku", "sku.rec"))
  return(newdata)
}

#' Recommend similar products to visitors based on product interraction
#' @export
#' @importFrom parallel mclapply
#' @param model similarity model object
#' @param hits visitor product hits data to be used for recommendations
#' @param exclude.same exludes products in the hits data per user if set to TRUE
#' @param filter function generated with makeRecommendationsFilter()
recommendSimilarProducts <- function(model, hits, exclude.same = TRUE,
                                     filter = makeRecommendationsFilter()) {
  visitor.id <- sku <- sku.rec <- sim <- group <- same <- NULL

  hits.l <- split(hits, f = substr(hits$visitor.id, 1, 3))
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
  setkeyv(newdata, c("visitor.id", "sku"))

  return (newdata)
}

#' Create filter function to reduce number of recommendations
#' to a relevant subset
#' @export
#' @param groups named vector of product types (or other level of product hierarchy)
#' @param values number of recommendations to return per visitor
makeRecommendationsFilter <- function(groups = NULL, values = 20) {
  function(data) {
    visitor.id <- sku <- sim <- NULL

    res <- keepOnePerGroup(data, groups)
    # Limit results to the requested number of skus
    res <- res[order(sim, decreasing = T), head(.SD[, list(sku, sim)], values), visitor.id]
  }
}
