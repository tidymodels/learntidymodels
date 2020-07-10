#' Plot PCA component loadings from a recipe or workflow
#'
#' A plot of the component loadings for each original column of the data is
#' produced.
#' @inheritParams get_component_data
#' @return A `ggplot` object.
#' @examples
#' library(recipes)
#' library(parsnip)
#' library(workflows)
#' library(ggplot2)
#'
#' data("Chicago", package = "modeldata")
#'
#' theme_set(theme_minimal())
#'
#' ## -----------------------------------------------------------------------------
#'
#' train_pca <-
#'   recipe(ridership ~ ., data = Chicago %>% dplyr::select(1:21)) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_pca(all_predictors())
#'
#' # or when used in a workflow
#' lm_workflow <-
#'   workflow() %>%
#'   add_model(linear_reg() %>% set_engine("lm")) %>%
#'   add_recipe(train_pca)
#'
#' ## -----------------------------------------------------------------------------
#'
#' train_pca <- prep(train_pca)
#'
#' plot_components(train_pca, component_number <= 3)
#'
#' plot_components(train_pca, component_number <= 3, value > 0)
#'
#' ## -----------------------------------------------------------------------------
#'
#' lm_workflow <- lm_workflow %>% fit(data = Chicago)
#'
#' plot_components(lm_workflow, component_number <= 3)
#'
#' @export
plot_components <- function(x, ...) {
    UseMethod("plot_components")
}

#' @export
#' @rdname plot_components
plot_components.recipe <- function(x, ..., id = NULL, type = "pca") {
    comp_vals <- get_component_data(x, ..., id = id, type = type)
    # Reorder component labels
    comp_vals$component <- forcats::fct_inorder(comp_vals$component)
    pca_rng <- max(abs(comp_vals$value))
    pca_rng <- c(-pca_rng, pca_rng)
    comp_vals %>%
        dplyr::mutate(component = component) %>%
        ggplot2::ggplot(ggplot2::aes(value, terms, fill = terms)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::facet_wrap( ~ component) +
        ggplot2::labs(y = NULL, x = "Coefficient Value") +
        ggplot2::xlim(pca_rng)
}

#' @export
#' @rdname plot_components
plot_components.workflow <- function(x, ..., id = NULL) {
    x <- workflows::pull_workflow_prepped_recipe(x)
    plot_components(x, ..., id = id)
}
