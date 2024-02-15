# Testing functions displaying messages' info

withr::local_options(joyn.verbose = FALSE)
df1 <- data.frame(
  id1 = c(1, 1, 2, 3),
  id2 = c("a", "b", "b", "c"),
  name = c("John", "Jane", "Bob", "Carl"),
  age = c(35, 28, 42, 50)
)
df2 <- data.frame(
  id1 = c(1, 2, 3, 3),
  id2 = c("a", "b", "c", "e"),
  salary = c(60000, 55000, 70000, 80000),
  dept = c("IT", "Marketing", "Sales", "IT")
)

df3 <- df2[, c("id1", "id2")]

collapse::join(df1,df3, on = c("id1", "id2"), how = "inner")

x <- c("id1", "id2")

df1 |>
  fselect(x)

# Test that joyn_msg works as expected ####
test_that("joyn_msg works as expected", {
  types <- c("info", "note", "warn", "timing", "err")
  # store some msg
  store_msg("info",
    ok = cli::symbol$tick, "  ",
    pale = "This is an info message")

  store_msg("warn",
     err = cli::symbol$cross, "  ",
     note = "This is a warning message")

  out_all <- joyn_msg("all")
  out_warn <- joyn_msg("warn")

  class(out_all) |>
    expect_equal("data.frame")

  class(out_warn) |>
    expect_equal(class(out_warn))

  #print(out_warn)$type |>
  #  expect_equal("warn")

  type = "err"

  if (type %in% types) {
      expect_no_error(joyn_msg(type))
    } else expect_error()

})


# Test that joyn store_msg works as expected ####

test_that("storing messages works as expected", {

  # errors -------
  ## wrong type ------------
  expect_error(store_msg("blah",
                         ok = cli::symbol$tick, "  ",
                         pale = "another try"))

  ## no message provided ------------
  expect_error(store_msg("info"))

  ## wrong style  ----------
  store_msg("info",
            timing = cli::symbol$star, "   ",
            blah = "Joyn's report available in variable {.val {x}}") |>
    expect_error()

  # output --------
  clear_joynenv()
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
  clear_joynenv()
  dt <- store_msg("info",
                  ok = cli::symbol$tick, "  ",
                  pale = "first try")

  expect_equal(nrow(dt), 1)


  ## duplicated obs deleted -----------
  clear_joynenv()
  store_msg("info", "simple message")
  store_msg("info", "simple message")
  dt <- store_msg("info", "simple message")

  nrow(dt) |>
    expect_equal(1)

})


test_that("display messages works", {
  store_msg("info",
            ok = cli::symbol$tick, "  ",
            pale = "first try")
  # errors --------
  joyn_msg("blah") |>
    expect_error()

  clear_joynenv()
  expect_false(rlang::env_has(.joynenv, "joyn_msgs"))

  joyn_msg("all") |>
    expect_error()


  # output ----------
  clear_joynenv()
  expect_false(rlang::env_has(.joynenv, "joyn_msgs"))
  store_msg("info",
            ok = cli::symbol$tick, "  ",
            pale = "first try")
  dt <- joyn_msg()
  expect_equal(nrow(dt), 1)
  expect_equal(names(dt), c("type", "msg"))

  clear_joynenv()
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

# Testing joyn_msgs_exist function ####
test_that("joyn_msgs_exist works as expected", {
  clear_joynenv()

  joyn_msgs_exist() |>
    expect_error()

  store_msg("info", "simple message")

  joyn_msgs_exist() |>
    expect_equal(TRUE)
})

#
