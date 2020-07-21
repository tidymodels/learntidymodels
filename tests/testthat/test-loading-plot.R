context("Extract PCA pls")

source(test_path("helper-loadings.R"))

## -----------------------------------------------------------------------------

test_that('PCA recipe', {
    p <- plot_loadings(train_pca)
    expect_is(p, "ggplot")
    expect_equal(names(p$data), c('terms', 'value', 'component', 'id', 'component_number'))
    expect_equal(rlang::get_expr(p$mapping$x), expr(value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(terms))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Coefficient Value")
    expect_equal(p$labels$fill, "terms")
})


## -----------------------------------------------------------------------------

test_that('PCA workflow', {
    p <- plot_loadings(pca_workflow)
    expect_is(p, "ggplot")
    expect_equal(names(p$data), c('terms', 'value', 'component', 'id', 'component_number'))
    expect_equal(rlang::get_expr(p$mapping$x), expr(value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(terms))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Coefficient Value")
    expect_equal(p$labels$fill, "terms")
})


## -----------------------------------------------------------------------------

test_that('PLS recipe', {
    p <- plot_loadings(train_pls, type = "pls")
    expect_is(p, "ggplot")
    expect_equal(names(p$data), c('terms', 'value', 'component', 'id', 'component_number'))
    expect_equal(rlang::get_expr(p$mapping$x), expr(value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(terms))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Coefficient Value")
    expect_equal(p$labels$fill, "terms")
})


## -----------------------------------------------------------------------------

test_that('PLS workflow', {
    p <- plot_loadings(pls_workflow, type = "pls")
    expect_is(p, "ggplot")
    expect_equal(names(p$data), c('terms', 'value', 'component', 'id', 'component_number'))
    expect_equal(rlang::get_expr(p$mapping$x), expr(value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(terms))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Coefficient Value")
    expect_equal(p$labels$fill, "terms")
})


## -----------------------------------------------------------------------------

test_that('PCA recipe - top loadings', {
    p <- plot_top_loadings(train_pca)
    expect_is(p, "ggplot")
    expect_equal(
        names(p$data),
        c('terms', 'value', 'component', 'id', 'component_number', 'Positive?', 'abs_value', 'order')
    )
    expect_equal(rlang::get_expr(p$mapping$x), expr(abs_value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(`Positive?`))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Abs. Coefficient Value")
    expect_equal(p$labels$fill, "Positive?")
})


## -----------------------------------------------------------------------------

test_that('PCA workflow - top loadings', {
    p <- plot_top_loadings(pca_workflow)
    expect_is(p, "ggplot")
    expect_equal(
        names(p$data),
        c('terms', 'value', 'component', 'id', 'component_number', 'Positive?', 'abs_value', 'order')
    )
    expect_equal(rlang::get_expr(p$mapping$x), expr(abs_value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(`Positive?`))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Abs. Coefficient Value")
    expect_equal(p$labels$fill, "Positive?")
})


## -----------------------------------------------------------------------------

test_that('PLS recipe - top loadings', {
    p <- plot_top_loadings(train_pls, type = "pls")
    expect_is(p, "ggplot")
    expect_equal(
        names(p$data),
        c('terms', 'value', 'component', 'id', 'component_number', 'Positive?', 'abs_value', 'order')
    )
    expect_equal(rlang::get_expr(p$mapping$x), expr(abs_value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(`Positive?`))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Abs. Coefficient Value")
    expect_equal(p$labels$fill, "Positive?")
})


## -----------------------------------------------------------------------------

test_that('PLS workflow - top loadings', {
    p <- plot_top_loadings(pls_workflow, type = "pls")
    expect_is(p, "ggplot")
    expect_equal(
        names(p$data),
        c('terms', 'value', 'component', 'id', 'component_number', 'Positive?', 'abs_value', 'order')
    )
    expect_equal(rlang::get_expr(p$mapping$x), expr(abs_value))
    expect_equal(rlang::get_expr(p$mapping$y), expr(terms))
    expect_equal(rlang::get_expr(p$mapping$fill), expr(`Positive?`))
    expect_null(p$labels$y)
    expect_equal(p$labels$x, "Abs. Coefficient Value")
    expect_equal(p$labels$fill, "Positive?")
})
