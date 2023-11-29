test_that("storing messages works as expected", {

  # errors -------
  expect_error(store_msg("blah",
                         ok = cli::symbol$tick, "  ",
                         pale = "another try"))

  expect_error(store_msg("info"))

  # output --------
  rlang::env_unbind(.joynenv, "joyn_msgs")
  expect_false(rlang::env_has(.joynenv, "joyn_msgs"))
  dt <- store_msg("info",
                  ok = cli::symbol$tick, "  ",
                  pale = "first try")

  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c("type", "msg"))

  dt <- store_msg("info",
                  ok = cli::symbol$tick, "  ",
                  pale = "second try")
  expect_equal(nrow(dt), 2)

  # if env is emptied
  rlang::env_unbind(.joynenv, "joyn_msgs")
  dt <- store_msg("info",
                  ok = cli::symbol$tick, "  ",
                  pale = "first try")

  expect_equal(nrow(dt), 1)

})


test_that("display messages works", {
  store_msg("info",
            ok = cli::symbol$tick, "  ",
            pale = "first try")
  # errors --------
  joyn_msg("blah") |>
    expect_error()

  rlang::env_unbind(.joynenv, "joyn_msgs")
  expect_false(rlang::env_has(.joynenv, "joyn_msgs"))

  joyn_msg("all") |>
    expect_error()

  # output ----------
  rlang::env_unbind(.joynenv, "joyn_msgs")
  expect_false(rlang::env_has(.joynenv, "joyn_msgs"))
  store_msg("info",
            ok = cli::symbol$tick, "  ",
            pale = "first try")
  dt <- joyn_msg()
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c("type", "msg"))

  rlang::env_unbind(.joynenv, "joyn_msgs")
  expect_false(rlang::env_has(.joynenv, "joyn_msgs"))

  store_msg("info",
            ok = cli::symbol$tick, "  ",
            pale = "first try info")
  store_msg("warn",
            ok = cli::symbol$tick, "  ",
            pale = "second try warn")
  store_msg("note",
            ok = cli::symbol$tick, "  ",
            pale = "third try note")

  ## filtering works -------
  dt <- joyn_msg(c("info", "warn"))
  expect_equal(nrow(dt), 2)

  dt <- joyn_msg()
  expect_equal(nrow(dt), 3)


})
