context("golem tests")

library(golem)

testthat::test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
})

testthat::test_that("app server", {
  server <- app_server
  testthat::expect_is(server, "function")
})










