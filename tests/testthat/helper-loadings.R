library(recipes)
library(parsnip)
library(workflows)
library(ggplot2)

data("Chicago", package = "modeldata")

theme_set(theme_minimal())

## -----------------------------------------------------------------------------

train_pca <-
    recipe(ridership ~ ., data = Chicago %>% dplyr::select(1:21)) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_pca(all_predictors())

pca_workflow <-
    workflow() %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    add_recipe(train_pca)

train_pca <- prep(train_pca)
pca_workflow <- pca_workflow %>% fit(data = Chicago)

## -----------------------------------------------------------------------------

train_pls <-
    recipe(ridership ~ ., data = Chicago %>% dplyr::select(1:21)) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_pls(all_predictors(), outcome = vars(ridership))

pls_workflow <-
    workflow() %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    add_recipe(train_pls)

train_pls <- prep(train_pls)
pls_workflow <- pls_workflow %>% fit(data = Chicago)
