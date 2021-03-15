#' @importFrom utils globalVariables
#' @importFrom dplyr %>%
#' @importFrom embed step_umap
#'
## -----------------------------------------------------------------------------

# token imports to stop R CMD check from complaining

#' @importFrom tidyverse tidyverse_conflicts
#' @importFrom tidymodels tag_attach

## -----------------------------------------------------------------------------

utils::globalVariables(
    c(
        "Positive?", "abs_value", "component", "terms", "value"
    )
)
