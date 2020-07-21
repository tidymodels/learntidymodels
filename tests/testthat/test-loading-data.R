context("Extract PCA data")

source(test_path("helper-loadings.R"))

## -----------------------------------------------------------------------------

test_that('PCA recipe with no options', {
    raw_pca <- tibble::as_tibble(train_pca$steps[[3]]$res$rotation, rownames = "terms")

    step_data <- get_loading_data(train_pca)
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pca[[i]]
        )
    }
})

test_that('PCA workflow with no options', {
    raw_pca <-
        pca_workflow %>%
        pull_workflow_prepped_recipe() %>%
        pluck("steps") %>%
        pluck(3) %>%
        pluck("res") %>%
        pluck("rotation") %>%
        tibble::as_tibble(rownames = "terms")

    step_data <- get_loading_data(pca_workflow)
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pca[[i]]
        )
    }
})

## -----------------------------------------------------------------------------

test_that('PCA recipe with filters', {
    raw_pca <- tibble::as_tibble(train_pca$steps[[3]]$res$rotation, rownames = "terms")

    step_data <- get_loading_data(train_pca, value > .3)
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pca[[i]][raw_pca[[i]] > .3]
        )
    }
})


test_that('PCA workflow with filters', {
    raw_pca <-
        pca_workflow %>%
        pull_workflow_prepped_recipe() %>%
        pluck("steps") %>%
        pluck(3) %>%
        pluck("res") %>%
        pluck("rotation") %>%
        tibble::as_tibble(rownames = "terms")

    step_data <- get_loading_data(pca_workflow, component_number <= 3)
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pca[[i]]
        )
    }
})


## -----------------------------------------------------------------------------

test_that('PLS recipe with no options', {
    raw_pls <- tidy(train_pls, 3)
    step_data <- get_loading_data(train_pls, type = "pls")
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pls %>% filter(component == i) %>% pull(value)
        )
    }
})

test_that('PLS workflow with no options', {
    raw_pls <-
        pls_workflow %>%
        pull_workflow_prepped_recipe() %>%
        tidy(3)

    step_data <- get_loading_data(pls_workflow, type = "pls")
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pls %>% filter(component == i) %>% pull(value)
        )
    }
})

## -----------------------------------------------------------------------------

test_that('PLS recipe with filters', {
    raw_pls <- tidy(train_pls, 3)

    step_data <- get_loading_data(train_pls, value > .3, type = "pls")
    comps <- unique(step_data$component)
    for(i in comps) {
        expect_equal(
            step_data %>% filter(component == i) %>% pull(value),
            raw_pls %>% filter(component == i & value > .3) %>% pull(value)
        )
    }
})


test_that('PLS workflow with filters', {
    raw_pls <-
        pls_workflow %>%
        pull_workflow_prepped_recipe() %>%
        tidy(3)

    step_data <- get_loading_data(pls_workflow, component_number <= 1, type = "pls")
    expect_equal(
        step_data %>% pull(value),
        raw_pls %>% filter(component == "PLS1") %>% pull(value)
    )
})

## -----------------------------------------------------------------------------

test_that('bad inputs', {
    expect_error(
        get_loading_data(train_pls),
        "The recipe does not appear to use step_pca()"
    )
    expect_error(
        get_loading_data(pls_workflow),
        "The recipe does not appear to use step_pca()"
    )

    pca_twice <-
        recipe(ridership ~ ., data = Chicago %>% dplyr::select(1:21)) %>%
        step_center(all_predictors()) %>%
        step_scale(all_predictors(), id = "scale") %>%
        step_pca(Austin, Quincy_Wells, Belmont,  id = "1st") %>%
        step_pca(Archer_35th, Oak_Park, Western, id = "2nd")

    expect_error(
        get_loading_data(pca_twice),
        'argument "id" is missing, with no default'
    )

    expect_error(
        get_loading_data(pca_twice, id = "1st"),
        'prep\\(\\)'
    )

    expect_error(
        get_loading_data(pca_twice, id = TRUE),
        'should be either a single character string or single numeric value'
    )

    expect_error(
        get_loading_data(pca_twice, id = 1),
        'does not appear to correspond to a pca step'
    )

    expect_error(
        get_loading_data(pca_twice, id = "scale"),
        'does not appear to correspond to a pca step'
    )
})
