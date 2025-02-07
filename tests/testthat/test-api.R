test_that("connection string can be defined", {

  skip_on_ci()

  con <- ws4sql_con("http://ducky:duckz@localhost:8000/duckserve")
  is_valid <- con$user == "ducky" & con$pass == "duckz"
  expect_true(is_valid)
  
})

test_that("connection string can be defined", {

  skip_on_ci()

  con <- ws4sql_con("http://ducky:duckz@localhost:8000/duckserve")

  res <- con |> ws4sql_read("#Q1")

  is_valid <- res$result[[1]]$resultSet[[1]]$b == "one-to-ten"

  expect_true(is_valid)
  
})
