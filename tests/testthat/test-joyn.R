withr::local_options(joyn.verbose = FALSE)
library(data.table)
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

x5 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:18, NA, NA))

#-------------------------------------------------------------------------------
# TESTS ------------------------------------------------------------------------
#-------------------------------------------------------------------------------


test_that(
  "select `by` vars when non specified", {
    expect_equal(
      joyn(
        x = x1,
        y = y1,
        match_type = "m:1"
      ),
      joyn(
        x  = x1,
        y  = y1,
        by = "id",
        match_type = "m:1"
      )
    )
  }

)

test_that("all types of by argument raise no error", {

  joyn(x          = x4,
       y          = y4,
       by         = "id1=id2",
       match_type = "m:m") |>
    expect_no_error()


  # THIS ONE
  joyn(x          = x4,
       y          = y4,
       by         = c("id1 = id", "id2"),
       match_type = "m:1") |>
    expect_no_error()

  joyn(x          = x4,
       y          = y4,
       by         = c("id1 = id2", "id2 = id"),
       match_type = "m:1") |>
    expect_no_error()

  joyn(x          = x4,
       y          = y4,
       by         = c("id2", "x"),
       match_type = "1:1") |>
    expect_no_error()

})

test_that("Errors if no common variables", {
  xf <- copy(x1)
  xf[, id := NULL]
  expect_error(
    joyn(
      xf,
      y1
    )
  )
})

test_that("m:m and 1:1 gives the same if data is correct", {
  expect_equal(
    joyn(
      x2,
      y2,
      by = "id",
      update_values = TRUE,
      match_type = "1:1"
    ),
    joyn(
      x2,
      y2,
      by = "id",
      update_values = TRUE,
      verbose = FALSE,
      match_type = "m:m"
    )
  )

  expect_equal(
    joyn(
      x2,
      y2,
      by = "id",
      update_NAs = TRUE,
      match_type = "1:1"
    ),
    joyn(
      x2,
      y2,
      by = "id",
      update_NAs = TRUE
    )
  )

  expect_equal(joyn(x2, y2, by = "id", match_type = "1:1"),
               joyn(x2, y2, by = "id"))

})


test_that("left joyn is correct", {
  x <- joyn(
    x1,
    y1,
    by = "id",
    keep = "left",
    match_type = "m:1"
  )
  expect_equal(nrow(x), nrow(x1))


  w <- joyn(x2,
             y2,
             by = "id",
             keep = "left",
             match_type = "1:1")
  expect_equal(nrow(w), nrow(x2))


})

test_that("inverse joyn works", {
  ll <-
    joyn(
      y3,
      x3,
      keep = "left",
      by = "id",
      match_type = "m:1",
      reportvar = FALSE,
      sort      = TRUE
    )
  rr <-
    joyn(
      x3,
      y3,
      keep = "right",
      by = "id",
      match_type = "1:m",
      reportvar = FALSE,
      sort      = TRUE
    )

  lnames <- names(ll)
  setcolorder(rr, lnames)

  expect_equal(ll, rr)

  lt <-
    joyn(
      y3,
      x3,
      by = "id",
      match_type = "m:1",
      reportvar = FALSE,
      keep = "left",
      sort = TRUE
    )
  rt <-
    joyn(
      x3,
      y3,
      by = "id",
      match_type = "1:m",
      reportvar = FALSE,
      keep = "right",
      sort = TRUE
    )

  lnamest <- names(lt)
  setcolorder(rt, lnamest)

  expect_equal(lt, rt)


})


test_that("FULL- Compare with base::merge", {
  jn <- joyn(
    x1,
    y1,
    by         = "id",
    reportvar  = FALSE,
    match_type = "m:1",
    sort = TRUE
  )

  br <- base::merge(x1, y1, by = "id", all = TRUE)

  setorderv(br, "id", na.last = TRUE)
  setorderv(jn, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")

  expect_equal(jn, br, ignore_attr = 'row.names')

  jn <- joyn(x2,
             y2,
             by= "id",
              reportvar = FALSE,
              keep_common_vars = TRUE,
             sort = TRUE)

  br <- base::merge(x2, y2, by = "id", all = TRUE)

  setorderv(br, "id", na.last = TRUE)
  setorderv(jn, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br, ignore_attr = 'row.names')

})


test_that("LEFT- Compare with base::merge", {
  jn <-
    joyn(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "left",
      match_type = "m:1",
      sort = TRUE
    )
  br <- base::merge(x1, y1, by = "id", all.x = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setorderv(jn, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")
  expect_equal(jn, br, ignore_attr = "row.names")


  jn <- joyn(
      x2,
      y2,
      by = "id",
      reportvar = FALSE,
      keep = "left",
      match_type = "1:1",
      keep_common_vars = TRUE,
      sort = TRUE
    )

  br <- base::merge(x2, y2, by = "id", all.x = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setorderv(jn, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br, ignore_attr = "row.names")

})



test_that("RIGHT - Compare with base::merge", {
  jn <-
    joyn(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "right",
      match_type = "m:1",
      sort = TRUE
    )
  br <- base::merge(x1, y1, by = "id", all.y = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    joyn(
      x2,
      y2,
      by               = "id",
      reportvar        = FALSE,
      keep             = "right",
      match_type       = "1:1",
      keep_common_vars = TRUE,
      sort = TRUE
    )

  br <- base::merge(x2, y2, by = "id", all.y = TRUE)

  setorderv(br, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})


test_that("INNER - Compare with base::merge", {
  jn <-
    joyn(
      x1,
      y1,
      by         = "id",
      reportvar  = FALSE,
      keep       = "inner",
      match_type = "m:1",
      sort = TRUE
    )
  br <- base::merge(x1, y1, by = "id")
  setorderv(br, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    joyn(
      x2,
      y2,
      by               = "id",
      reportvar        = FALSE,
      keep             = "inner",
      match_type       = "1:1",
      keep_common_vars = TRUE,
      sort = TRUE
    )
  br <- base::merge(x2, y2, by = "id")

  setorderv(br, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})


test_that("match types work", {
  expect_error(joyn(
    x3,
    y3,
    by = "id",
    match_type = "1:1"
  ))
  expect_error(joyn(
    y3,
    x3,
    by = "id",
    match_type = "1:1"
  ))
  expect_error(joyn(
    x3,
    y3,
    by = "id",
    match_type = "m:1"
  ))

  x <-
    structure(
      list(
        id = c(1, 1, 2, 3, 4, 7),
        t = c(1L, 2L, 1L, 2L, NA, 2L),
        x = c(16, 12, NA, NA, 15, 12)
      ),
      row.names = c(NA, -6L),
      class = c("data.table", "data.frame")
    )

  y <-
    structure(
      list(
        id  =  c(1, 1, 2, 3, 4, 6),
        y = c(11L, 15L, 20L, 13L, 10L, 7L),
        x = 16:21
      ),
      row.names = c(NA, -6L),
      class = c("data.table", "data.frame")
    )

  by <- "id"
  jn <- joyn(x, y, by = by, match_type = "m:m")

  njn <- nrow(jn)

  ux <- x[, .N, by = by]

  uy <- y[, .N, by = by]

  dd <- merge.data.table(ux, uy, by = "id", all = TRUE)
  setnafill(dd, fill = 1)
  cN <- dd[,
           N := N.x * N.y][, sum(N)]

  expect_equal(njn, cN)

})

###########################################################################################
test_that("Update NAs", {
  # update NAs in x variable form x
  jn <- joyn(x2,
             y2,
             by               = "id",
             update_NAs       = TRUE,
             keep_common_vars = TRUE,
             sort = TRUE)

  idx <- x2[is.na(x), "id"]

  expect_equal(jn[idx, on = "id"][, x.x], y2[idx, on = "id"][, x])

  jn_1 <- joyn(x2,
               y2,
               by               = "id = yd",
               update_NAs       = TRUE,
               keep_common_vars = TRUE,
               sort = TRUE)


  expect_equal(jn_1[idx, on = "id"][, x.x], y2[idx, on = "id"][, x])

  jn_2 <- joyn(x5,
               y4,
               by               = c("id", "y"),
               update_NAs       = TRUE,
               keep_common_vars = TRUE,
               sort = TRUE)

  to_replace <- x5[is.na(x), "id"]

  expect_equal(jn_2[to_replace, on = "id"][, x.x], y4[to_replace, on = "id"][, x])

  out <- joyn(x5,
               y4,
               by               = c("id = id2", "yd = id"),
               update_NAs       = FALSE,
               keep_common_vars = TRUE,
              sort = TRUE)

  to_replace <- out[(is.na(x.x) & !is.na(x.y)) | (is.na(y.x) & !is.na(y.y) ), c("id", "yd")]

  jn_3 <- joyn(x5,
               y4,
               by               = c("id = id2", "yd = id"),
               update_NAs       = TRUE,
               keep_common_vars = TRUE,
               sort = TRUE)

  any(jn_3[to_replace, on = c("id", "yd")][, .joyn] != "NA updated", with = FALSE) |>
    expect_equal(FALSE)

})


test_that("Update actual values", {

  jn <-joyn(x                = x2,
            y                = y2,
            by               = "id",
            update_values    = TRUE,
            update_NAs       = TRUE,
            keep_common_vars = TRUE,
            sort = TRUE)

  br <- base::merge(x2, y2, by = "id", all = TRUE)

  br <- br |>
    ftransform( x.x = fifelse(!is.na(x.x) & is.na(x.y),
                              x.x, x.y))


  setorderv(br, "id", na.last = TRUE)
  setorderv(jn, "id", na.last = TRUE)
  setattr(jn, 'sorted', "id")

  expect_equal(jn |>
                 fselect(x.x),
               br |>
                 fselect(x.x),
               ignore_attr = 'row.names')

  joyn(x2,
       y2,
       by               = "id = yd",
       update_values    = TRUE,
       update_NAs       = FALSE,
       keep_common_vars = TRUE) |>
    expect_no_error()

  joyn(x5,
       y4,
       by               = c("id", "y"),
       update_values    = TRUE,
       keep_common_vars = TRUE) |>
    expect_no_error()


  joyn(x5,
       y4,
       by               = c("id = id2", "yd = id"),
       update_values    = TRUE,
       keep_common_vars = TRUE) |>
    expect_no_error()

})


test_that("y vars are extracted correctly", {
  yvars <- "y"
  jn <- joyn(x2,
              y2,
              by = "id",
              y_vars_to_keep = yvars)

  expect_equal(names(jn), c(names(x2), yvars, ".joyn"))

  jn <-
    joyn(
      x2,
      y2,
      by = "id",
      y_vars_to_keep = yvars,
      reportvar = FALSE
    )

  expect_equal(names(jn), c(names(x2), yvars))

  jn <- joyn(x2,
              y2,
              by = "id",
              y_vars_to_keep = F)

  expect_equal(names(jn), c(names(x2), ".joyn"))

  jn <- joyn(x2,
             y2,
             by = "id",
             y_vars_to_keep = F,
             reportvar = "report_test")

  expect_equal(names(jn), c(names(x2), "report_test"))



  yvars <- "reuiou"
  expect_error(joyn(x2,y2,by = "id", y_vars_to_keep = yvars))

})

test_that("selection of reportvar", {
  reportvar <- "wijf"
  jn <-
    joyn(x2,
          y2,
          by = "id",
          reportvar = reportvar)

  expect_true(reportvar %in% names(jn))

  jn <- joyn(x2,
              y2,
              by = "id",
              reportvar = FALSE,
              y_vars_to_keep = c("yd", "y"))


  expect_false(".joyn" %in% names(jn))

  expect_equal(unique(c(names(x2), names(y2))), names(jn))


  jn <- joyn(x         = x2,
             y         = y2,
             by        = "id",
             reportvar = "t")

  allnames <- unique(c(names(x2), names(y2)))

  newname <- setdiff(names(jn), allnames)

  expect_true(length(newname) > 0)

})

test_that("reporttype works", {
  jn <- joyn(x          = x2,
             y          = y2,
             by         = "id",
             reporttype = "numeric")

  class(jn$.joyn) |>
    expect_equal("numeric")

})


test_that("Keep Y vars works", {
  jn <- joyn(x2,
              y2,
              by = "id",
              keep_common_vars = TRUE)

  inames <- intersect(names(x2), names(y2))
  inames <- inames[!(inames %in% "id")]

  inames <- paste0(inames, ".y")

  expect_true(all(inames  %in% names(jn)))

})


test_that("error when there is not natural join", {
  xx1 <- copy(x1)
  setnames(xx1, "id", "foo")
  expect_error(joyn(xx1, y1))
})


test_that("different names in key vars are working fine", {

  df <- joyn(x4, y4, by = c("id1 = id", "id2"), match_type = "m:1", y_vars_to_keep = c("y"), sort = TRUE)

  dd <- data.table(id1 = c(1, 1, 2, 2, 3, 3, 5, 6),
                   id2 = c(1, 1, 2, 1, 3, 4, 2, 3),
                   t = c(1L, 2L, 1L, NA, 2L, NA, NA, NA),
                   x = c(16, 12, NA, NA, NA, 15, NA, NA),
                   y = c(11L, 11L, NA, 15L, NA, 10L, 20L, 13L),
                   ".joyn" = c("x & y", "x & y", "x", "y", "x", "x & y", "y", "y")
                   )

  setorderv(dd, c("id1", "id2"), na.last = TRUE)
  setattr(df, 'sorted', c("id1", "id2"))
  setattr(dd, 'sorted', c("id1", "id2"))


  expect_equal(df, dd)

})


test_that("invalid names are changed", {

  dd <- joyn(x1, y1, reportvar = "_report", match_type = "m:1")
  expect_true("X_report"  %in% names(dd))

})


test_that("convert to data.table when dataframe", {

  yy1 <- as.data.frame(y1)

  expect_equal(joyn(x1, yy1, by = "id", match_type = "m:1"), joyn(x1, y1, match_type = "m:1"))

})


test_that("do not convert to data.table", {
  xx1 <- as.data.frame(x1)
  expect_equal(joyn(xx1, y1, match_type = "m:1") |> class(), xx1 |> class())
})

# Check return table is of the same class as x
test_that("output table class", {
  out <- joyn(x2, y2)

  class(out) |>
    expect_equal(class(x2))
})

# Test anti-join
#_________________________________
test_that("joyn's how = anti works as expected", {

  r <- joyn(x          = x1,
            y          = y1,
            match_type = "m:1",
            by         = "id",
            keep       = "anti",
            sort       = TRUE)

  expect_true(funique(r$`.joyn`) == "x")
  expect_equal(names(r),
               c(names(x1), ".joyn"))
  expect_equal(r$id,
               c(3, NA_real_))
  rn <- names(joyn(x              = x1,
                   y              = y1,
                   match_type     = "m:1",
                   by             = "id",
                   keep           = "anti",
                   y_vars_to_keep = TRUE,
                   sort           = TRUE))
  expect_false(all(names(r) == rn[1:length(names(r))]))

  # m:m anti joins
  r <- joyn(x              = x4,
            y              = y4[!id2 == 3,],
            match_type     = "m:m",
            by             = c("id1 = id2"),
            keep           = "anti",
            y_vars_to_keep = TRUE,
            sort = TRUE)

  expect_true(allNA(r$y))
  expect_true(all(r$id1 == 3))
  expect_true(all(r$.joyn == "x"))


})

test_that("anti join warning for update values", {

  expect_message(
    r <- joyn(x          = x1,
              y          = y1,
              match_type = "m:1",
              by         = "id",
              keep       = "anti",
              update_values = TRUE,
              verbose = TRUE))

  r2 <- joyn(x          = x1,
             y          = y1,
             match_type = "m:1",
             by         = "id",
             keep       = "anti",
             update_values = FALSE)

  expect_equal(r,
               r2)


})


# Test all input data is unchanged


test_that("joyn() - input data unchanged", {

  expect_equal(x1,
               data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                          t  = c(1L, 2L, 1L, 2L, NA_integer_),
                          x  = 11:15))

  expect_equal(y1,
               data.table(id = c(1,2, 4),
                          y  = c(11L, 15L, 16)))

  expect_equal(x2,
               data.table(id = c(1, 4, 2, 3, NA),
                          t  = c(1L, 2L, 1L, 2L, NA_integer_),
                          x  = c(16, 12, NA, NA, 15)))

  expect_equal(y2,
               data.table(id = c(1, 2, 5, 6, 3),
                          yd = c(1, 2, 5, 6, 3),
                          y  = c(11L, 15L, 20L, 13L, 10L),
                          x  = c(16:20)))

  expect_equal(x3,
               data.table(id  = c("c","b", "d"),
                          v   = 8:10,
                          foo = c(4,2, 7)))

  expect_equal(y3,
               data.table(id = c("c","b", "c", "a"),
                          y  = c(11L, 15L, 18L, 20L)))

  expect_equal(x4,
               data.table(id1 = c(1, 1, 2, 3, 3),
                          id2 = c(1, 1, 2, 3, 4),
                          t   = c(1L, 2L, 1L, 2L, NA_integer_),
                          x   = c(16, 12, NA, NA, 15)))

  expect_equal(y4,
               data.table(id  = c(1, 2, 5, 6, 3),
                          id2 = c(1, 1, 2, 3, 4),
                          y   = c(11L, 15L, 20L, 13L, 10L),
                          x   = c(16:20)))

  expect_equal(x5,
               data.table(id = c(1, 2, 5, 6, 3),
                          yd = c(1, 2, 5, 6, 3),
                          y  = c(11L, 15L, 20L, 13L, 10L),
                          x  = c(16:18, NA, NA)))

})


