#' Recommends products similar to given products
#'
#' Based on the similarity matix and given product recommends top products
#' @export
#'
#' @importFrom utils head
#' @param sim.matrix similarity matrix based on implicit interractions
#' @param skus identifiers of the implicit interraction products (source should match names in sim.matrix)
#' @param values required number of recommendations
#' @param exclude.same excludes values in `skus` from recommendations
#' @param groups - named vector of sku categories
recommendSimilar <- function(sim.matrix, skus, values, exclude.same, groups = NULL) {
  sku.rec <- sim <- group <- NULL

  missing.skus <- setdiff(skus, rownames(sim.matrix))
  if(length(missing.skus) > 0) {
    warning("Following skus are missing from the sim.matrix: ", paste(missing.skus, collapse = ", "))
  }
  # only keep skus that are in the similarity matrix
  skus <- setdiff(skus, missing.skus)

  similarity.scores <- combineSimilarity(sim.matrix, skus, exclude.same)
  similarity.scores <- keepOnePerGroup(similarity.scores, groups)

  # Limit results to the requested number of skus
  res <- head(similarity.scores[order(sim, decreasing = T)]$sku, values)

  return (res)
}

#' Combined similarity to given products
#'
#' Turns recommendations matrix into a normalised data table
#' with mean similarity score for each recommended product
#' @inherit recommendSimilar
combineSimilarity <- function(sim.matrix, skus, exclude.same) {
  sim <- sku.rec <- NULL
  # filter to the list of relevant skus
  filter <- notInWhich(colnames(sim.matrix), skus, exclude.same)
  product.affinity <- melt(sim.matrix[skus, filter, drop=FALSE], na.rm = T)
  colnames(product.affinity) <- c("sku", "sku.rec", "sim")
  product.affinity <- data.table(product.affinity, key = c("sku", "sku.rec"))

  # Group similiarity score by recommended sku
  combined.scores <- product.affinity[, list(sim = mean(sim)), by = sku.rec]
  setnames(combined.scores, "sku.rec", "sku")
  setkey(combined.scores, "sku")
  return(combined.scores)
}

#' Creates permutation index for exclusion of values
#' @param x vector to filter
#' @param y vector to match
#' @param filter flag to indicate whether filter should be applied
notInWhich <- function(x, y, filter) {
  no.filter <- 1:length(x)
  if(filter) {
    index = - which(x %in% y)
    if(length(index) == 0L) index <- no.filter
    return(index)
  }
  else {
    return(no.filter)
  }
}

#' Gets top value per group
#' @param dt data.table with similarity score
#' @param groups named vector of product groups
keepOnePerGroup <- function(dt, groups) {
  sim <- group <- NULL

  if(is.null(groups)) return(dt)

  # Append group data to affinity table
  groups.table <- data.table(sku = names(groups), group = groups, key = "sku")
  dt <- dt[groups.table, nomatch = 0]

  # Get the best performing sku per group which
  # can be a combindation of several columns e.g.: c("visitor.id", "group")
  by.cols <- setdiff(colnames(dt), c("sku", "sim"))
  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  dt <- dt[dt[, .I[sim == max(sim)], by = by.cols]$V1]
}
