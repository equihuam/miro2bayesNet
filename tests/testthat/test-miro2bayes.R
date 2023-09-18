test_that("miroBoards", {
  service <- readline("  Service > ")
  user <- readline("  User > ")
  test_boards <- miroBoards(service, user)
  expect_false(is_empty(test_boards))
  return()})

