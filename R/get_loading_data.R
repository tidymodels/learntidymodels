#' Obtain and format PCA component data from a recipe or workflow
#'
#' @param x A prepped recipe or fitted workflow that uses a recipe. The recipe
#' must have used at least one [recipes::step_pca()] (or [recipes::step_pls()]).
#' @param id A single numeric or character value that is used to pick the step
#' with the PCA results. If a single [recipes::step_pca()] (or
#' [recipes::step_pls()]) was used, this argument is ignored. *Note*: if used,
#' `id` must be named.
#' @param ... An optional series of conditional statements used to filter the
#' PCA data before plotting. See Details below.
#' @param type A character value ("pca" or "pls") for the type of step to use.
#' @return A tibble that mirrors the `tidy()` method for those steps. The data
#'  also includes a numeric `component_number` and may have been changed due to
#'  any filters supplied to `...`.
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
#' get_loading_data(train_pca, component_number <= 3)
#'
#' get_loading_data(train_pca, component_number <= 3, value > 0)
#'
#' ## -----------------------------------------------------------------------------
#'
#' lm_workflow <- lm_workflow %>% fit(data = Chicago)
#'
#' get_loading_data(lm_workflow, component_number <= 3)
#'
#'@export
get_loading_data <- function(x, ...) {
    UseMethod("get_loading_data")
}

#' @export
#' @rdname get_loading_data
get_loading_data.recipe <- function(x, ..., id, type = "pca") {
    step_id <- check_recipe_for_step(x, id, type)
    comp_vals <- recipes::tidy(x, id = step_id)
    # Convert component label to number
    comp_vals$component_number <- as.numeric(gsub("[[:alpha:]]", "", comp_vals$component))
    # Optional filtering
    filter_pca_data(comp_vals, ...)
}

#' @export
#' @rdname get_loading_data
get_loading_data.workflow <- function(x, ..., id = NULL, type = "pca") {
    x <- workflows::extract_recipe(x)
    get_loading_data(x, ..., id = id, type = type)
}

## -----------------------------------------------------------------------------

check_recipe_for_step <- function(x, id, type = "pca") {
    rec_steps <- recipes::tidy(x)
    has_step <- rec_steps$type == type
    if (sum(has_step) == 0) {
        rlang::abort(
            paste0("The recipe does not appear to use step_", type, "(). Use the ",
                   "`tidy()` method for more details.")
        )
    }
    if (sum(has_step) > 1) {
        if (is.null(id)) {
            rlang::abort(
                paste("The recipe appears to have multiple", type, "steps. Use the 'id'",
                      "argument to pick one. The `tidy()` can list the availible",
                      "steps.")
            )
        } else {
            if (length(id) != 1 | (all(!is.numeric(id)) & all(!is.character(id)))) {
                rlang::abort(
                    paste("'id' should be either a single character string or single",
                          "numeric value.")
                )
            }
            if (is.numeric(id)) {
                step_id <- rec_steps$id[rec_steps$number == id]
                if (rec_steps$type[rec_steps$number == id] != "pca") {
                    rlang::abort(
                        paste0("'id' value ", id, " does not appear to correspond to a ",
                               type, " step. The `tidy()` can list the availible steps.")
                    )
                }
            } else {
                step_id <- rec_steps$id[rec_steps$id == id]
                if (rec_steps$type[rec_steps$id == id] != "pca") {
                    rlang::abort(
                        paste0("'id' value ", id, " does not appear to correspond to a ",
                               type, " step. The `tidy()` can list the availible steps.")
                    )
                }
            }
        }
    } else {
        step_id <- rec_steps$id[rec_steps$type == type]
    }

    if (!rec_steps$trained[rec_steps$id == step_id]) {
        rlang::abort("Please `prep()` the recipe.")
    }

    step_id
}

filter_pca_data <- function(x, ...) {
    filters <- rlang::enquos(...)
    if (!rlang::is_empty(filters)) {
        x <- dplyr::filter(x, !!!filters)
    }
    x
}

