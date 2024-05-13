withr::local_options(joyn.verbose = FALSE)

#-------------------------------------------------------------------------------
# TEST DATA --------------------------------------------------------------------
#-------------------------------------------------------------------------------
# options(joyn.verbose = FALSE)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))

x2 = data.table(id = c(1, 4, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))

y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))

y3 <- data.table(id = c("c","b", "c", "a"),
                 y  = c(11L, 15L, 18L, 20L))
x3 <- data.table(id  = c("c","b", "d"),
                 v   = 8:10,
                 foo = c(4,2, 7))
x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))

y4 = data.table(id  = c(1, 2, 5, 6, 3),
                id2 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                x   = c(16:20))
x5 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_, 4L),
                x  = 11:16)
y5 = data.table(id = c(1,2, 4, NA_integer_, NA_integer_),
                y  = c(11L, 15L, 16, 17L, 18L))
reportvar = getOption("joyn.reportvar")
#-------------------------------------------------------------------------------
# TEST LEFT JOINS --------------------------------------------------------------
#-------------------------------------------------------------------------------


test_that("LEFT JOIN - Conducts left join", {

  # One way
  jn_joyn <- left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )
  setorder(jn_joyn, id, na.last = TRUE)

  jn_joyn2 <- left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    unmatched = "drop"
  )
  setorder(jn_joyn2, id, na.last = TRUE)
  jn_dplyr <- dplyr::left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )

  attr( jn_dplyr, "sorted") <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = "row.names" # data.table::serorderv convert row.names to characters.
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )

  # Second set of tables ----------------------
  jn_joyn <- left_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )
  setorder(jn_joyn, id, na.last = TRUE)
  jn_dplyr <- dplyr::left_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )
  setorder(jn_dplyr, id, na.last = TRUE)
  attr(jn_dplyr, "sorted") <- "id"
  expect_equal(
    jn_joyn |>
      fselect(-get(reportvar)), # `jvar` should be `.joyn` in principle
    jn_dplyr,
    ignore_attr = "row.names"
  )

  jn <- left_join(
    x4,
    y4,
    by = c("id1 = id2"),
    relationship = "many-to-many"
  )
  jn_dplyr <- dplyr::left_join(x4, y4, by = dplyr::join_by(id1 == id2), relationship = "many-to-many")
  # jn_dplyr <- data.frame(id1 = c(1, 1, 1, 1, 2, 3, 3),
  #                        id2 = c(1, 1, 1, 1, 2, 3, 4),
  #                        t   = c(1, 1, 2, 2, 1, 2, NA),
  #                        x.x = c(16, 16, 12, 12, NA, NA, 15),
  #                        id  = c(1, 2, 1, 2, 5, 6, 6),
  #                        y   = c(11, 15, 11, 15, 20, 13, 13),
  #                        x.y = c(16, 17, 16, 17, 18, 19, 19))
  attr(jn_dplyr, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = ".internal.selfref"
  )

  # With "many-to-one" relationship
  jn <- left_join(
    x1,
    y2,
    by = "id",
    relationship = "many-to-one"
  )

  jn_dplyr <- dplyr::left_join(
    x1,
    y2,
    by = "id",
    relationship = "many-to-one"
  )

  attr(jn_dplyr, "sorted") <- "id"
  attr(jn, "sorted") <- "id"
  jn_dplyr <- roworder(jn_dplyr, "id", na.last = FALSE)

  rownames(jn) <- c(1:length(x1$id))

  expect_equal(
    jn |> fselect(-get(reportvar)) |> as.data.frame(),
    jn_dplyr |> as.data.frame(),
    ignore_attr = ".internal.selfref"
  )


})


test_that("LEFT JOIN - no id given", {
  jn1 <- left_join(
    x2,
    y2
  )
  jn2 <- left_join(
    x2,
    y2,
    by = c("id", "x")
  )
  expect_equal(jn1, jn2)
})


test_that("LEFT JOIN - copy given", {
  jn1 <- joyn::left_join(
    x2,
    y2,
    copy = TRUE
  )

  (length(joyn_msg("warn")) > 0) |>
    expect_equal(TRUE)

})


test_that("LEFT JOIN - incorrectly specified arguments give errors", {
  expect_error(
    left_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = NULL
    )
  )
  expect_error(
    left_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    left_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c(1, 2)
    )
  )

  expect_error(
    left_join(
      x = y1,
      y = x1,
      relationship = "one-to-many",
      multiple = "any"
    )
  )

  expect_error(
    left_join(
      x = x4,
      y = y4,
      relationship = "many-to-many",
      by = "id2",
      multiple = "any"
    )
  )

  expect_no_error(
    left_join(
      x = x4,
      y = y4,
      relationship = "many-to-many",
      by = "id2",
      multiple = "all"
    )
  )

  expect_error(
    left_join(
      x = x1,
      y = y1,
      by = "id",
      relationship = "many-to-one",
      unmatched = "error"
    )
  )
  expect_error(
    left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = "invalid KEEP",
    by = "id")
  )
  expect_no_error(
    left_join(
      x = x2,
      y = y2,
      relationship = NULL,
      keep = T,
      by = "id"
    )
  )

})


test_that("LEFT JOIN - argument `keep` preserves keys in output", {
  jn <- left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = T,
    by = "id"
  )

  expect_true(
    "id.y" %in% names(jn)
  )

  expect_equal(
    jn |>
      fselect(id.y) |>
      na.omit() |>
      unique() |>
      reg_elem(),
    y1 |>
      fsubset(id %in% x1$id) |>
      fselect(id) |>
      reg_elem()
  )

  joyn::left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = NULL,
    by = "id"
  )

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
      expect_contains("warn")

  joyn::left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = "invalid keep",
    by = "id"
  ) |>
    expect_error()

})


test_that("LEFT JOIN - update values works", {
  x2a <- x2
  x2a$x <- 1:5
  jn <- left_join(
    x = x2a,
    y = y2,
    relationship = "one-to-one",
    update_values = TRUE,
    by = "id"
  )
  vupdated <- jn |>
    fsubset(get(reportvar) == "value updated") |>
    fselect(x.x) |>
    reg_elem()
  expect_true(
    all(vupdated %in% y2$x)
  )
  expect_equal(
    jn |>
      fsubset(get(reportvar) == "value updated") |>
      fnrow(),
    x2 |>
      fsubset(id %in% y2$id) |>
      fnrow()
  )

})


test_that("LEFT JOIN - reportvar works", {
  jn <- left_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = "report"
  )

  expect_true(
    "report" %in% names(jn)
  )

  expect_no_error(
    left_join(
      x1,
      y1,
      relationship = "many-to-one",
      by = "id",
      reportvar = FALSE)
    )

  expect_no_error(
    left_join(
      x1,
      y1,
      relationship = "many-to-one",
      by = "id",
      reportvar = NULL)
  )

})




test_that("LEFT JOIN - unmatched throws error", {

  left_join(x              = x4,
              y            = y4,
              relationship = "many-to-many",
              by           = "id2",
              unmatched    = "error") |>
    expect_no_error()


  left_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id1=id2",
            unmatched    = "error") |>
    expect_error()


  left_join(x            = x1,
            y            = y1,
            relationship = "many-to-one",
            by           = "id",
            unmatched    = "error") |>
    expect_error()


  left_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = c("id2", "x"),
            unmatched    = "error") |>
    expect_error()
################################################################################################################################################
  left_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = c("id2=id"),
            unmatched    = "error") |>
    expect_warning()

  left_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = c("id1=id2", "id2=id"),
            unmatched    = "error") |>
    expect_warning()



})



test_that("LEFT JOIN - NA matches", {

  jn <- left_join(
    x5,
    y5,
    relationship = "many-to-many"
  )
  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      nrow(),
    4
  )

})

# TEST RIGHT JOINS ------------------------------------------------------

test_that("RIGHT JOIN - Conducts right join", {
  # One way
  jn_joyn <- right_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )
  jn_joyn2 <- right_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    unmatched = "drop"
  )
  jn_dplyr <- dplyr::right_join(
    x1, y1, by = "id",
    relationship = "many-to-one"
  )
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )

  # Second set of tables ----------------------
  jn_joyn <- right_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- dplyr::right_join(
    x2,
    y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- jn_dplyr[order(jn_dplyr$id, na.last = T),]
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )

  jn <- right_join(
    x4,
    y4,
    by = c("id1 = id2"),
    relationship = "many-to-many"
  )
  #dplyr::right_join(x4, y4, by = dplyr::join_by(id1 == id2), relationship = "many-to-many")
  jn_dplyr <- dplyr::right_join(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2),
    relationship = "many-to-many"
  )
  attr(jn_dplyr, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = '.internal.selfref'
  )
})

test_that("RIGHT JOIN - no id given", {
  jn1 <- right_join(
    x2,
    y2
  )
  jn2 <- right_join(
    x2,
    y2,
    by = c("id", "x")
  )
  expect_equal(jn1, jn2)
  expect_no_error(
    right_join(
      x2,
      y2,
      by = NULL
    ))
})

test_that ("RIGHT JOIN - when copy TRUE get warning message", {
  clear_joynenv()
  joyn::right_join(
    x = x2,
    y = y2,
    by = "id",
    copy = TRUE,
    keep = FALSE
  )
  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")
})

test_that("RIGHT JOIN - incorrectly specified arguments give errors", {
  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = NULL
    )
  )
  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c(1, 2)
    )
  )

  expect_error(
    right_join(
      x = y1,
      y = x1,
      relationship = "one-to-many",
      multiple = "any"
    )
  )
  expect_error(
    right_join(
      x = y1,
      y = x1,
      relationship = "many-to-many",
      multiple = "any"
    )
  )
  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      unmatched = "error"
    )
  )
  expect_no_error(
    right_join(
      x = x2,
      y = y2,
      relationship = NULL,
      keep = T,
      by = "id"
    )
  )

})

test_that("RIGHT JOIN - argument `keep` preserves keys in output", {
  jn <- right_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = T,
    by = "id"
  )
  expect_true(
    "id.x" %in% names(jn)
  )
  expect_equal(
    jn |>
      fselect(id.x) |>
      na.omit() |>
      unique() |>
      reg_elem(),
    x1 |>
      fsubset(id %in% y1$id) |>
      fselect(id) |>
      unique() |>
      reg_elem()
  )
})


test_that("RIGHT JOIN - update values works", {
  x2a <- x2
  x2a$x <- 1:5
  jn <- right_join(
    x = x2a,
    y = y2,
    relationship = "one-to-one",
    update_values = TRUE,
    by = "id"
  )

  vupdated <- jn |>
    fsubset(get(reportvar) == "value updated") |>
    fselect(x.x) |>
    reg_elem()
  expect_true(
    all(vupdated %in% y2$x)
  )
  expect_equal(
    jn |>
      fsubset(get(reportvar) == "value updated") |>
      fnrow(),
    x2 |>
      fsubset(id %in% y2$id) |>
      fnrow()
  )
})

test_that("RIGHT JOIN - reportvar works", {
  jn <- right_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = "report"
  )
  expect_true(
    "report" %in% names(jn)
  )

  expect_no_error(
    right_join(
      x1,
      y1,
      relationship = "many-to-one",
      by = "id",
      reportvar = NULL))


  expect_no_error(
    right_join(
      x1,
      y1,
      relationship = "many-to-one",
      by = "id",
      reportvar = FALSE))

})

test_that("RIGHT JOIN - NA matches", {

  jn <- right_join(
    x5,
    y5,
    relationship = "many-to-many"
  )
  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      nrow(),
    4
  )

  # checking when na_matches is never warning msg is stored
  joyn::right_join(
    x = x5,
    y = y5,
    relationship = "many-to-many",
    na_matches = "never"
  )
  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")
})



### ERROR 1 --> change in y4$id2
test_that("RIGHT JOIN - unmatched error", {

 right_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id2",
            unmatched    = "error") |>
    expect_no_error()

 right_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id1=id2",
            unmatched    = "error") |>
    expect_no_error()

 right_join(x            = x1,
            y            = y1,
            relationship = "many-to-one",
            by           = "id",
            unmatched    = "error") |>
    expect_error()

 right_join(x           = x4,
           y            = y4,
           relationship = "one-to-one",
           by           = c("id2", "x"),
           unmatched    = "error") |>
   expect_error()

 right_join(x           = x4,
           y            = y4,
           relationship = "many-to-one",
           by           = c("id1=id2", "id2=id"),
           unmatched    = "error") |>
   expect_error()


})

#-------------------------------------------------------------------------------
# TEST FULL JOINS --------------------------------------------------------------
#-------------------------------------------------------------------------------


test_that("FULL JOIN - Conducts full join", {
  # One way
  jn_joyn <- full_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )
  jn_joyn2 <- full_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    unmatched = "drop"
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )
  jn_dplyr <- dplyr::full_join(
    x1, y1, by = "id", relationship = "many-to-one"
  )
  setorder(jn_dplyr, id, na.last = T)
  setorder(jn_joyn, id, na.last = T)
  attr(jn_dplyr,
       "sorted") <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )
  expect_true(
    all(c("x", "y", "x & y") %in% jn_joyn$.joyn)
  )
  expect_true(
    all(
      c(jn_joyn$id) %in% c(y1$id, x2$id)
    )
  )

  # With one-to-many relationship
  jn_joyn <- full_join(
    x = x2,
    y = y5,
    relationship = "one-to-many",
    by = "id"
  )

  jn_dplyr <- dplyr::full_join(
    x2,
    y5,
    relationship = "one-to-many",
    by = "id"
  )

  setorder(jn_dplyr, id, na.last = T)
  setorder(jn_joyn, id, na.last = T)
  attr(jn_dplyr,
       "sorted") <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = 'row.names'
  )


  # Second set of tables ----------------------
  jn_joyn <- full_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- dplyr::full_join(
    x2,
    y2,
    relationship = "one-to-one",
    by = "id"
  )
  setorder(jn_dplyr, id, na.last = T)
  setorder(jn_joyn, id, na.last = T)
  attr(jn_dplyr,
       "sorted") <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = 'row.names'
  )

  jn <- full_join(
    x4,
    y4,
    by = c("id1 = id2"),
    relationship = "many-to-many"
  )
  #dplyr::full_join(x4, y4, by = dplyr::join_by(id1 == id2), relationship = "many-to-many")
  jn_dplyr <- dplyr::full_join(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2),
    relationship = "many-to-many"
  )
  attr(jn_dplyr, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = '.internal.selfref'
  )

})


test_that("FULL JOIN - no id given", {
  jn <- full_join(
    x2,
    y2,
    by = NULL
  )

  jn1 <- full_join(
    x2,
    y2
  )
  jn2 <- full_join(
    x2,
    y2,
    by = c("id", "x")
  )

  expect_equal(jn, jn2)
  expect_equal(jn1, jn2)
})

test_that("FULL JOIN - copy arg", {
  expect_no_error(
    joyn::full_join(
      x            = x1,
      y            = y1,
      by           = "id",
      relationship = "many-to-one",
      copy         = TRUE
    )
  )

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")


})

test_that("FULL JOIN - (correctly) incorrectly specified arguments give (no) errors", {
  expect_error(
    full_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = NULL
    )
  )
  expect_error(
    full_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    full_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c(1, 2)
    )
  )

  expect_error(
    full_join(
      x = y1,
      y = x1,
      relationship = "one-to-many",
      multiple = "any"
    )
  )

  expect_no_error(
    full_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      unmatched = "error"
    )
  )

  expect_no_error(
    full_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      unmatched = "drop"
    )
  )

  expect_error(
    full_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      keep = "invalid keep",
      by = "id"
    )
  )


  joyn::full_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    keep = NULL
  ) |>
    expect_no_error()

  joyn::full_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    keep = NULL)

    rlang::env_get(.joynenv, "joyn_msgs")$type |>
      expect_contains("warn")

  # Error when relationship 1:m or m:m and multiple is not "all"
    expect_error(
      full_join(
        x = x1,
        y = y5,
        relationship = "many-to-many",
        by = "id",
        multiple = "any"
      )
    )

    expect_error(
      full_join(
        x = x1,
        y = y5,
        relationship = "many-to-many",
        by = "id",
        multiple = "first"
      )
    )

    expect_error(
      full_join(
        x = x1,
        y = y5,
        relationship = "many-to-many",
        by = "id",
        multiple = "last"
      )
    )


    expect_error(
      full_join(
        x = x2,
        y = y5,
        relationship = "one-to-many",
        by = "id",
        multiple = "any"
      )
    )

    expect_error(
      full_join(
        x = x2,
        y = y5,
        relationship = "one-to-many",
        by = "id",
        multiple = "first"
      )
    )

    expect_error(
      full_join(
        x = x2,
        y = y5,
        relationship = "one-to-many",
        by = "id",
        multiple = "last"
      )
    )

    expect_no_error(
      full_join(
        x            = x2,
        y            = y5,
        relationship = "one-to-many",
        by           = "id",
        unmatched    = "error"
      )
    )


})


test_that("FULL JOIN - argument `keep` preserves keys in output", {
  jn <- full_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = T,
    by = "id"
  )
  expect_true(
    "id.y" %in% names(jn)
  )
  expect_equal(
    jn |>
      fselect(id.y) |>
      na.omit() |>
      unique() |>
      reg_elem(),
    y1$id |>
      unique()
  )
})


test_that("FULL JOIN - update values works", {
  x2a <- x2
  x2a$x <- 1:5
  jn <- full_join(
    x = x2a,
    y = y2,
    relationship = "one-to-one",
    update_values = TRUE,
    by = "id"
  )
  vupdated <- jn |>
    fsubset(get(reportvar) == "value updated") |>
    fselect(x.x) |>
    reg_elem()
  expect_true(
    all(vupdated %in% y2$x)
  )
  expect_equal(
    jn |>
      fsubset(get(reportvar) == "value updated") |>
      fnrow(),
    x2 |>
      fsubset(id %in% y2$id) |>
      fnrow()
  )
})

test_that("FULL JOIN - reportvar works", {
  jn <- full_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = "report"
  )

  expect_true(
    "report" %in% names(jn)
  )

  expect_no_error(
    full_join(
      x1,
      y1,
      relationship = "many-to-one",
      by = "id",
      reportvar = NULL
    )
  )

  expect_no_error(
    full_join(
      x1,
      y1,
      relationship = "many-to-one",
      by = "id",
      reportvar = FALSE
    ))

})


test_that("FULL JOIN - NA matches", {

  jn <- full_join(
    x5,
    y5,
    relationship = "many-to-many"
  )
  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      fnrow(),
    4
  )

  # Warning when na_matches is never
  joyn::full_join(x            = x5,
                  y            = y5,
                  relationship = "many-to-many",
                  keep         = TRUE,
                  na_matches   = "never")

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")
})

test_that("FULL JOIN -unmatched error", {

  full_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id2",
            unmatched    = "error") |>
    expect_no_error()

  full_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id1=id2",
            unmatched    = "error") |>
    expect_no_error()

  full_join(x            = x1,
            y            = y1,
            relationship = "many-to-one",
            by           = "id",
            unmatched    = "error") |>
    expect_no_error()

})


#-------------------------------------------------------------------------------
# TEST INNER JOINS -------------------------------------------------------------
#-------------------------------------------------------------------------------


test_that("INNER JOIN - Conducts inner join", {
  # One way
  jn_joyn <- inner_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )
  jn_joyn2 <- inner_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    unmatched = "drop"
  )
  jn_dplyr <- dplyr::inner_join(
    x1, y1, by = "id", relationship = "many-to-one"
  )
  setorder(jn_dplyr, na.last = T)
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )
  expect_true(
    all(c("x & y") %in% jn_joyn$.joyn)
  )
  expect_true(
    all(
      c(jn_joyn$id) %in% intersect(y1$id, x2$id)
    )
  )

  # One to many relationship
  jn_joyn <- inner_join(
    x = x2,
    y = y5,
    relationship = "one-to-many",
    by = "id"
  )

  jn_dplyr <- dplyr::inner_join(
    x2,
    y5,
    relationship = "one-to-many",
    by = "id"
  )

  setorder(jn_dplyr, id, na.last = T)
  setorder(jn_joyn, id, na.last = T)
  attr(jn_dplyr,
       "sorted") <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = 'row.names'
  )

  # Second set of tables ----------------------
  jn_joyn <- inner_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- dplyr::inner_join(
    x2,
    y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- jn_dplyr[order(jn_dplyr$id, na.last = T),]
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )

  jn <- inner_join(
    x4,
    y4,
    by = c("id1 = id2"),
    relationship = "many-to-many"
  )
  #dplyr::inner_join(x4, y4, by = dplyr::join_by(id1 == id2), relationship = "many-to-many")
  jn_dplyr <- dplyr::inner_join(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2),
    relationship = "many-to-many"
  )
  attr(jn_dplyr, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = '.internal.selfref'
  )
})

test_that("INNER JOIN - no id given", {
  jn <- inner_join(
    x2,
    y2,
    by = NULL
  )

  jn1 <- inner_join(
    x2,
    y2
  )
  jn2 <- inner_join(
    x2,
    y2,
    by = c("id", "x")
  )

  expect_equal(jn, jn2)
  expect_equal(jn1, jn2)
})

test_that("INNER JOIN - incorrectly(correctly) specified arguments give (no)errors", {
  expect_error(
    inner_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = NULL
    )
  )
  expect_error(
    inner_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    inner_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c(1, 2)
    )
  )

  expect_error(
    inner_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      unmatched = "error"
    )
  )

  expect_error(
    inner_join(
      x = y1,
      y = x1,
      relationship = "one-to-many",
      multiple = "any"
    )
  )

  inner_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
    expect_error()

  expect_no_error(
    joyn::inner_join(
      x2,
      y2,
      by = c("id", "x"),
      copy = TRUE,
      keep = TRUE)
    )

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")


  expect_error(
    joyn::inner_join(
      x2,
      y2,
      by = c("id", "x"),
      keep = "invalid keep")
  )

  expect_no_error(
    inner_join(
      x = x2,
      y = y1,
      by = "id"
    )
  )

  # Error when relationship 1:m or m:m and multiple is not "all"
  expect_error(
    inner_join(
      x = x1,
      y = y5,
      relationship = "many-to-many",
      by = "id",
      multiple = "any"
    )
  )

  expect_error(
    inner_join(
      x = x1,
      y = y5,
      relationship = "many-to-many",
      by = "id",
      multiple = "first"
    )
  )

  expect_error(
    inner_join(
      x = x1,
      y = y5,
      relationship = "many-to-many",
      by = "id",
      multiple = "last"
    )
  )


  expect_error(
    inner_join(
      x = x2,
      y = y5,
      relationship = "one-to-many",
      by = "id",
      multiple = "any"
    )
  )

  expect_error(
    inner_join(
      x = x2,
      y = y5,
      relationship = "one-to-many",
      by = "id",
      multiple = "first"
    )
  )

  expect_error(
    inner_join(
      x = x2,
      y = y5,
      relationship = "one-to-many",
      by = "id",
      multiple = "last"
    )
  )

})

test_that("INNER JOIN - argument `keep` preserves keys in output", {
  jn <- inner_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = T,
    by = "id"
  )
  expect_true(
    "id.y" %in% names(jn)
  )
  expect_equal(
    jn |>
      fselect(id.y) |>
      na.omit() |>
      unique() |>
      reg_elem(),
    y1 |>
      fsubset(id %in% x1$id) |>
      fselect(id) |>
      unique() |>
      reg_elem()
  )

  # When keep is NULL
  joyn::inner_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = NULL,
    by = "id"
  )

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")

})


test_that("INNER JOIN - update values works", {
  x2a <- x2
  x2a$x <- 1:5
  jn <- inner_join(
    x = x2a,
    y = y2,
    relationship = "one-to-one",
    update_values = TRUE,
    by = "id"
  )
  vupdated <- jn |>
    fsubset(get(reportvar) == "value updated") |>
    fselect(x.x) |>
    reg_elem()
  expect_true(
    all(vupdated %in% y2$x)
  )
  expect_equal(
    jn |>
      fsubset(get(reportvar) == "value updated") |>
      fnrow(),
    x2 |>
      fsubset(id %in% y2$id) |>
      fnrow()
  )


})

test_that("INNER JOIN - reportvar works", {
  jn <- inner_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = "report"
  )
  expect_true(
    "report" %in% names(jn)
  )

  inner_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = FALSE
  ) |>
    expect_no_error()

  inner_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = NULL
  ) |>
    expect_no_error()



})
test_that("INNER JOIN - NA matches", {

  jn <- inner_join(
    x5,
    y5,
    relationship = "many-to-many"
  )
  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      fnrow(),
    4
  )

  # Warning when na_matches is never
  joyn::inner_join(x            = x5,
                  y            = y5,
                  relationship = "many-to-many",
                  keep         = TRUE,
                  na_matches   = "never")

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")
})

test_that("INNER JOIN - unmatched error", {
  # HERE, the of y4 have gotten switched, so id1 --> id2.
  #     problem must be in the ordering of the setnames() somewhere
  inner_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id2",
            unmatched    = "error") |>
    expect_no_error()

  inner_join(x            = x4,
            y            = y4,
            relationship = "many-to-many",
            by           = "id1=id2",
            unmatched    = "error") |>
    expect_error()

  inner_join(x            = x1,
            y            = y1,
            relationship = "many-to-one",
            by           = "id",
            unmatched    = "error") |>
    expect_error()

  inner_join(x            = x4,
             y            = y4,
             relationship = "one-to-one",
             by           = c("id2", "x"),
             unmatched    = "error") |>
    expect_error()

  inner_join(x            = x4,
             y            = y4,
             relationship = "many-to-one",
             by           = c("id1=id2", "id2=id"),
             unmatched    = "error") |>
    expect_error() |>
    expect_warning()

})




#-------------------------------------------------------------------------------
# TEST ANTI JOINS -------------------------------------------------------------
#-------------------------------------------------------------------------------


test_that("ANTI JOIN - Conducts ANTI join", {
  # One way
  jn_joyn_m1 <- anti_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )
  jn_joyn <- anti_join(
    x = x1,
    y = y1,
    by = "id"
  )

  jn_dplyr <- dplyr::anti_join(
    x1, y1, by = "id"
  )
  setorder(jn_dplyr, na.last = F)
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )
  expect_equal(
    jn_joyn,
    jn_joyn_m1
  )
  expect_true(
    all(c("x") %in% jn_joyn$.joyn)
  )
  expect_true(
    all(
      c(jn_joyn$id) %in% x1$id))

  # One to many relationship
  jn_joyn <- anti_join(
    x = x2,
    y = y5,
    relationship = "one-to-many",
    by = "id"
  )

  jn_dplyr <- dplyr::anti_join(
    x2,
    y5,
    by = "id"
  )

  setorder(jn_dplyr, id, na.last = T)
  setorder(jn_joyn, id, na.last = T)
  attr(jn_dplyr,
       "sorted") <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr,
    ignore_attr = 'row.names'
  )

  # Second set of tables ----------------------
  jn_joyn <- anti_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- dplyr::anti_join(
    x2,
    y2,
    by = "id"
  )
  setorder(jn_dplyr, id, na.last = T)
  setorder(jn_joyn, id, na.last = T)
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"
  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dplyr
  )

  jn <- anti_join(
    x4,
    y4,
    by = c("id1 = id2"),
    relationship = "many-to-many"
  )
  #dplyr::anti_join(x4, y4, by = dplyr::join_by(id1 == id2), relationship = "many-to-many")
  jn_dplyr <- dplyr::anti_join(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2)
  )
  attr(jn_dplyr, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)) |> dim(),
    c(0, 6)
  )
})

test_that("ANTI JOIN - no id given", {
  jn <- anti_join(
    x2,
    y2,
    by = NULL
  )

  jn1 <- anti_join(
    x2,
    y2
  )
  jn2 <- anti_join(
    x2,
    y2,
    by = c("id", "x")
  )

  expect_equal(jn, jn2)
  expect_equal(jn1, jn2)
})

test_that("ANTI JOIN - incorrectly(correctly) specified arguments give (no)errors", {
  expect_error(
    anti_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = NULL
    )
  )
  expect_error(
    anti_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    anti_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c(1, 2)
    )
  )

  expect_error(
    anti_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      unmatched = "error"
    )
  )

  expect_error(
    anti_join(
      x = y1,
      y = x1,
      relationship = "one-to-many",
      multiple = "any"
    )
  )

  anti_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
    expect_error()

  expect_no_error(
    anti_join(
      x2,
      y2,
      by = c("id", "x"),
      copy = TRUE,
      keep = TRUE)
  )

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")


  expect_error(
    anti_join(
      x2,
      y2,
      by = c("id", "x"),
      keep = "invalid keep")
  )

  expect_no_error(
    anti_join(
      x = x2,
      y = y1,
      by = "id"
    )
  )

  # Error when relationship 1:m or m:m and multiple is not "all"
  expect_error(
    anti_join(
      x = x1,
      y = y5,
      relationship = "many-to-many",
      by = "id",
      multiple = "any"
    )
  )

  expect_error(
    anti_join(
      x = x1,
      y = y5,
      relationship = "many-to-many",
      by = "id",
      multiple = "first"
    )
  )

  expect_error(
    anti_join(
      x = x1,
      y = y5,
      relationship = "many-to-many",
      by = "id",
      multiple = "last"
    )
  )


  expect_error(
    anti_join(
      x = x2,
      y = y5,
      relationship = "one-to-many",
      by = "id",
      multiple = "any"
    )
  )

  expect_error(
    anti_join(
      x = x2,
      y = y5,
      relationship = "one-to-many",
      by = "id",
      multiple = "first"
    )
  )

  expect_error(
    anti_join(
      x = x2,
      y = y5,
      relationship = "one-to-many",
      by = "id",
      multiple = "last"
    )
  )

})

test_that("ANTI JOIN - argument `keep` preserves keys in output", {
  jn <- anti_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = T,
    by = "id",
    y_vars_to_keep = TRUE
  )
  expect_true(
    "id.y" %in% names(jn)
  )
  expect_equal(
    jn$id.y |>
      funique(),
    NA_real_
  )

  # When keep is NULL
  joyn::anti_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = NULL,
    by = "id"
  )

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")

})



test_that("ANTI JOIN - reportvar works", {
  jn <- anti_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = "report"
  )
  expect_true(
    "report" %in% names(jn)
  )

  anti_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = FALSE
  ) |>
    expect_no_error()

  anti_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = NULL
  ) |>
    expect_no_error()



})

test_that("ANTI JOIN - NA matches", {

  jn <- anti_join(
    x5,
    y5,
    relationship = "many-to-many"
  )
  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      fnrow(),
    0
  )

  # Warning when na_matches is never
  joyn::anti_join(x            = x5,
                   y            = y5,
                   relationship = "many-to-many",
                   keep         = TRUE,
                   na_matches   = "never")

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_contains("warn")
})


