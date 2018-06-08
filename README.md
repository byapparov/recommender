# recommender

[![Build Status](https://travis-ci.org/madedotcom/recommender.svg?branch=master)](https://travis-ci.org/madedotcom/recommender)
[![codecov.io](https://codecov.io/github/madedotcom/recommender/coverage.svg?branch=master)](https://codecov.io/github/madedotcom/recommender?branch=master)

The recommender package provides collaborative filter based product recommendations. 

## Installation

The newest development release can be installed from GitHub:

```R
# install.packages('devtools')
devtools::install_github("byapparov/recommender")
```

## Similar products model

To create a model for similar products recommender you will need a history of 
user-to-product interactions in a data.frame where first collumn identifies the user and
second column identifies a product, e.g.:

```R
  user.hits <- data.table(
    users =    c("u1", "u2", "u1", "u3", "u2", "u1"),
    products = c("p1", "p2", "p3", "p2", "p3", "p4")
  )
  model <- similarityRecommender(user.hits)
```

Product interaction history columns are matched according to order:

1. User Identifier (any name)
2. Product Identifier (any name)

Any other columns in the table will be ignored.

### Item-to-item recommendations

`recommendComplimentaryProducts()` function provides "others also viewed" type of recommendations.

```R
 # table of products which will be linked to recommendations
 products <- data.table(sku = c("a", "b", "c", "d"),
                         type = c("p1", "p2", "p3", "p1"))

 # Get 5 most similar products for each product in the `products` table
 product.affinity <- recommendComplimentaryProducts(model, products, limit = 5)
```

### User-to-item recommendations

`recommendSimilarProducts()` function provides "similar products" recommendations based on the user-item 
interactions data.

```R
  # product interactions stream of new users
  page.views <- data.table(
    user = c("u1", "u1", "u2", "u3", "u3", "u3"),
    sku = c("a", "b",   "c",  "a",  "a", "d")
  )
  
  # `groups` in the filter limit number of recommended products
  # from the same group to one. this can be useful in cases where 
  # distance between recommended items should be increased
  # 
  # `values` in the filter limit number of items returned per user
  groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")
  filter <- makeRecommendationsFilter(groups, values = 1)
  
  # make user-to-item recommendations table
  res <- recommendSimilarProducts(  
    test.sim.model, 
    page.views, 
    exclude.same = T, 
    filter = filter
  )
```
