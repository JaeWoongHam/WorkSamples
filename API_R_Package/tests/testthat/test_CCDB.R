library(CCDB)
context("Check for Valid input & Output for every function")

test_that("test if default argument parameters are working for product_function",{
  a<- product_function(product="Debt collection",subproduct_option = TRUE,state_option=TRUE)
  b<- product_function()
  expect_equal(a,b)}
)

test_that("test if input argument parameters are working for product_function",{
  a<- product_function(product="Debt collection",subproduct_option = FALSE,state_option=TRUE)
  b<- product_function("Debt collection",FALSE)
  expect_equal(a,b)}
)

test_that("test if the output is a list for product_function", {
  a<- typeof(product_function())
  b<- "list"
  expect_equal(a,b)
})

test_that("test if default argument parameters are working for general_function",{
  a<- general_function(company = "BANK OF AMERICA, NATIONAL ASSOCIATION",product = "Checking or savings account",issue="Managing an account", limit=10)
  b<- general_function()
  expect_equal(a,b)}
)

test_that("test if input argument parameters are working for general_function",{
  a<- general_function(company = "BANK OF AMERICA, NATIONAL ASSOCIATION",product = "Checking or savings account",issue="Managing an account", limit=5)
  b<- general_function(limit=5)
  expect_equal(a,b)}
)

test_that("test if the output for general_function is a list", {
  a<- typeof(general_function())
  b<- "list"
  expect_equal(a,b)
})
