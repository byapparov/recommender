library(methods)
library(parallel)

#' S4 class that represents similar products recommendation model
#' @export
#' @param sim cosine matrix of product similarity
setClass("similarity.recommender", slots = c(sim = "matrix"))

#' Factory for the similarity recommendation model
#' @export
#' @param data product hit stream with visitor.id and sku
#' @param filter allows to reduce recommendations to a given set
#' @param weights named vector of weights to adjust similarity score
similarityRecommender <- function(data, filter = NULL, weights = NULL) {
  m <- userProductHitsToMatrix(data)
  m <- cosineMatrix(m)
  m <- abjustSimMatrix(m, weights)
  if (is.null(filter)) filter <- colnames(m) # all products will be used
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

  # only include products that are in the model
  target.skus <- intersect(unique(newdata[, sku]), rownames(object@sim))
  similarity <- melt(object@sim[target.skus, , drop = FALSE], na.rm = T)
  if (nrow(similarity) == 0L) return(as.numeric(NULL))
  colnames(similarity) <- c("sku", "sku.rec", "score")
  similarity <- data.table(similarity, key = c("sku", "sku.rec"))
  scores <- similarity[newdata][, score]
  return(scores)
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
  visitor.id <- sku <- sku.rec <- sim <- NULL

  hits.l <- split(hits, f = substr(hits$visitor.id, 1, 3))
  res <- mclapply(hits.l, function(visitor.hits) {
    newdata <- expandHits(model, visitor.hits)
    if (exclude.same) { # exclude seen products from recommendations
      newdata <- excludeSame(newdata)
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

  return(newdata)
}

#' Recommend products in item-to-item scenario
#'
#' @export
#'
#' @param model similarity model object
#' @param skus data.table of product details with sku field as unique identifier.
#' @param group.column name of the column that will be used for grouping
#' @param limit number of records to return per product
recommendComplimentaryProducts <- function(model, skus,
                                           group.column = "sku",
                                           limit = 20L) {
  sku <- sku.rec <- sim <- group.rec <- NULL

  similarity <- melt(model@sim, na.rm = T, variable.factor = FALSE)
  similarity <- data.table(similarity)
  colnames(similarity) <- c("sku", "sku.rec", "sim")
  similarity[, sku := as.character(sku)]
  similarity[, sku.rec := as.character(sku.rec)]
  dt <- merge(skus, similarity, by = "sku")
  dt <- dt[sku != sku.rec]

  # At this point direction of recommendation is not important as model is simetrical.
  # This will be used here to keep only one value per group in the sku column,
  # while columns will be renamed later to achieve correct result.
  groups <- skus[, .(sku.rec = sku, group.rec = get(group.column))]
  dt <- merge(dt, groups, by = "sku.rec")
  dt <- dt[get(group.column) != group.rec] # exlude products within the same group

  # Get best record per each available group for a given sku
  dt <- dt[dt[, .I[sim == max(sim)], by = .(sku, group.rec)]$V1]

  # Count results and limit the records
  dt <- dt[order(sim, decreasing = TRUE), head(.SD, limit), by = sku]
  dt <- dt[order(sku, -sim), .(sku, sku.rec, sim)]
  invisible(dt)
}

#' Expand visitor product hits data to dataset for prediction
#' @export
#' @param object similarity model
#' @param data visitor page hits
expandHits <- function(object, data) {
  sku <- dummy <- NULL

  missing.skus <- setdiff(unique(data[, sku]), rownames(object@sim))
  if (length(missing.skus) > 0) {
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

#' Filter out products that were seen by visitor
#'
#' @param data sku combinations for prediction with sku.rec and sku fields
#' @return sku combinations where sku.rec does not have seen sku within visitor.id
excludeSame <- function(data) {
  sku.rec <- sku <- same <- visitor.id <- NULL
  data[, same := sku.rec %in% sku, visitor.id]
  data <- data[!same == TRUE][, same := NULL]
  return(data)
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
