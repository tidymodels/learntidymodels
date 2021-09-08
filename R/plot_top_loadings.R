#' Plot the largest PCA component loadings from a recipe or workflow
#'
#' A plot of the `n` largest component loadings is produced.
#' @inheritParams get_loading_data
#' @param n The number of columns to plot (per component).
#' @return A `ggplot` object.
#' @examples
#' library(recipes)
#' library(parsnip)
#' library(workflows)
#' library(ggplot2)
#'
#' data("cells", package = "modeldata")
#'
#' theme_set(theme_minimal())
#'
#' ## -----------------------------------------------------------------------------
#'
#' cell_pca <-
#'   recipe(class ~ ., data = cells %>% dplyr::select(-case)) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_pca(all_predictors())
#'
#' # or when used in a workflow
#' lr_workflow <-
#'   workflow() %>%
#'   add_model(logistic_reg() %>% set_engine("glm")) %>%
#'   add_recipe(cell_pca)
#'
#' ## -----------------------------------------------------------------------------
#'
#' cell_pca <- prep(cell_pca)
#'
#' # What were the top 10 channel 1 columns in the first two components?
#' plot_top_loadings(cell_pca, grepl("ch_1", terms) & component_number <= 2, n = 10)
#'
#' ## -----------------------------------------------------------------------------
#'
#' lr_workflow <- lr_workflow %>% fit(data = cells)
#'
#' plot_top_loadings(lr_workflow, component_number <= 4)
#'
#' @export
plot_top_loadings <- function(x, ...) {
    UseMethod("plot_top_loadings")
}

#' @export
#' @rdname plot_top_loadings
plot_top_loadings.recipe <- function(x, ..., n = 4, id = NULL, type = "pca") {
    comp_vals <- get_loading_data(x, ..., id = id, type = type)

    comp_vals <-
        comp_vals %>%
        dplyr::mutate(
            `Positive?` = value > 0,
            abs_value = abs(value)
        )%>%
        dplyr::group_by(component) %>%
        dplyr::slice_max(abs_value, n = n) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(component, abs_value) %>%
        dplyr::mutate(order = dplyr::row_number())

    # Tactics based on
    # https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets
    ggplot2::ggplot(comp_vals, ggplot2::aes(x = order, y = abs_value, fill = `Positive?`)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap( vars(component), scales = "free_y") +
        ggplot2::scale_x_continuous(
            breaks = comp_vals$order,
            labels = comp_vals$terms,
            expand = c(0,0)
        )  +
        ggplot2::labs(x = NULL, y = "Abs. Coefficient Value")
}

#' @export
#' @rdname plot_top_loadings
plot_top_loadings.workflow <- function(x, ..., n = 4, id = NULL, type = "pca") {
    x <- workflows::pull_workflow_prepped_recipe(x)
    plot_top_loadings(x, ..., id = id, n = n, type = type)
}
