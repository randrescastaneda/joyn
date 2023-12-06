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

test_that("Errors if no common variables", {
  xf <- copy(x1)
  xf[, id := NULL]
  expect_error(joyn(xf, y1))
})

test_that("m:m and 1:1 gives the same if data is correct", {
  expect_equal(
    joyn( # ZP: THIS GIVES ERROR
      x2,
      y2,
      by = "id",
      update_values = TRUE,
      match_type = "1:1"
    ),
    joyn( # ZP: THIS GIVES ERROR
      x2,
      y2,
      by = "id",
      update_values = TRUE,
      verbose = FALSE,
      match_type = "m:m"
    )
  )

  expect_equal(
    joyn( # ZP: THIS GIVES ERROR
      x2,
      y2,
      by = "id",
      update_NAs = TRUE,
      match_type = "1:1"
    ),
    joyn( # ZP: THIS GIVES ERROR
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
      reportvar = FALSE
    )
  rr <-
    joyn(
      x3,
      y3,
      keep = "right",
      by = "id",
      match_type = "1:m",
      reportvar = FALSE
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
      keep = "left"
    )
  rt <-
    joyn(
      x3,
      y3,
      by = "id",
      match_type = "1:m",
      reportvar = FALSE,
      keep = "right"
    )

  lnamest <- names(lt)
  setcolorder(rt, lnamest)

  expect_equal(lt, rt)


})


test_that("FULL- Compare with base::merge", {
  jn <- joyn(x1,
              y1,
              by = "id",
              reportvar = FALSE)

  br <- base::merge(x1, y1, by = "id", all = TRUE)

  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")

  expect_equal(jn, br)



  jn <-
    joyn(x2,
          y2,
          by = "id",
          reportvar = FALSE)

  br <- base::merge(x2, y2, by = "id", all = TRUE)
  br[, x := x.x][,
                 c("x.x", "x.y") := NULL]
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})

test_that("LEFT- Compare with base::merge", {
  jn <-
    joyn(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "left",
      match_type = "m:1"
    )
  br <- base::merge(x1, y1, by = "id", all.x = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")
  expect_equal(jn, br)


  jn <- joyn(
      x2,
      y2,
      by = "id",
      reportvar = FALSE,
      keep = "left",
      match_type = "1:1"
    )
  br <- base::merge(x2, y2, by = "id", all.x = TRUE)
  br[, x := x.x][,
                 c("x.x") := NULL] # ZP: changed
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})



test_that("RIGHT - Compare with base::merge", {
  jn <-
    joyn(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "right",
      match_type = "m:1"
    )
  br <- base::merge(x1, y1, by = "id", all.y = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    joyn(
      x2,
      y2,
      by = "id",
      reportvar = FALSE,
      keep = "right",
      match_type = "1:1"
    )

  br <- base::merge(x2, y2, by = "id", all.y = TRUE)
  br[, x := x.x][,
                 c("x.x") := NULL]
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})


test_that("INNER - Compare with base::merge", {
  jn <-
    joyn(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "inner",
      match_type = "m:1"
    )
  br <- base::merge(x1, y1, by = "id")
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    joyn(
      x2,
      y2,
      by        = "id",
      reportvar = FALSE,
      keep      = "inner",
      match_type = "1:1"
    )
  br <- base::merge(x2, y2, by = "id")

  br[, x := x.x][,
                 c("x.x") := NULL]

  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


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

# ZP: THIS GIVES ERROR
test_that("Update NAs", {
  # update NAs in x variable form x
  jn <- joyn(x2, # ZP: THIS GIVES ERROR
              y2,
              by = "id",
              update_NAs = TRUE)

  idx <- x2[is.na(x), "id"]

  expect_equal(jn[idx, on = "id"][, x], y2[idx, on = "id"][, x])


})

# ZP: THIS GIVES ERROR
test_that("Update actual values", {

  jn <-
    joyn(x2,
          y2,
          by = "id",
          update_values = TRUE)

  br <- base::merge(x2, y2, by = "id", all = TRUE)

  br[, x := fifelse(!is.na(x.x) & is.na(x.y),
                    x.x, x.y)]

  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  expect_equal(jn[, x], br[, x])


})

# ZP: THIS GIVES ERROR
test_that("y vars are extracted correctly", {
  yvars <- "y"
  jn <- joyn(x2,
              y2,
              by = "id",
              yvars = yvars)

  expect_equal(names(jn), c(names(x2), yvars, ".joyn"))


  jn <-
    joyn(
      x2,
      y2,
      by = "id",
      yvars = yvars,
      reportvar = FALSE
    )

  expect_equal(names(jn), c(names(x2), yvars))

  jn <- joyn(x2, # ZP: THIS GIVES ERROR
              y2,
              by = "id",
              y_vars_to_keep = FALSE)

  expect_equal(names(jn), c(names(x2), ".joyn"))


  yvars <- "reuiou"
  expect_error(joyn(x2,y2,by = "id",yvars = yvars))

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
              reportvar = FALSE)

  expect_false("report" %in% names(jn))

  expect_equal(unique(c(names(x2), names(y2))), names(jn))


  jn <- joyn(x2,
              y2,
              by = "id",
              reportvar = "t")

  allnames <- unique(c(names(x2), names(y2)))

  newname <- setdiff(names(jn), allnames)

  expect_true(length(newname) > 0)

})



test_that("Keep Y vars works", {
  jn <- joyn(x2,
              y2,
              by = "id",
              keep_y_in_x = TRUE)

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

  df <- joyn(x4, y4, by = c("id1 = id", "id2"))

  dd <- data.table(id1 = c(1, 1, 2, 2, 3, 3, 5, 6),
                   id2 = c(1, 1, 2, 1, 3, 4, 2, 3),
                   t = c(1L, 2L, 1L, NA, 2L, NA, NA, NA),
                   x = c(16, 12, NA, NA, NA, 15, NA, NA),
                   y = c(11L, 11L, NA, 15L, NA, 10L, 20L, 13L),
                   report = c("x & y", "x & y", "x", "y", "x", "x & y", "y", "y")
                   )

  setorderv(dd, "id1", na.last = TRUE)
  setattr(dd, 'sorted', "id1")

  expect_equal(df, dd)

})


test_that("invalid names are changed", {

  dd <- joyn(x1, y1, reportvar = "_report")
  expect_true("X_report"  %in% names(dd))

})


test_that("convert to data.table when dataframe", {

  yy1 <- as.data.frame(y1)

  expect_equal(joyn(x1, yy1), joyn(x1, y1))

})


test_that("no matching obs", {

  xx2 <- x2[1, x := 23]

  dd <- joyn(xx2, y2)
  dw <- dd[, unique(report)]
  expect_equal(dw, c("y", "x"))

})

test_that("convert to data.table", {
  xx1 <- as.data.frame(x1)
  expect_equal(joyn(xx1, y1), joyn(x1, y1))
})







# ------------------------------------------------------------------------------
# zander add from joyn_workhorse
# ------------------------------------------------------------------------------

test_that("left/right/full/inner/semi/anti joyn is correct", {

  x <- joyn_workhorse(
    x    = x1,
    y    = y1,
    by   = "id",
    keep = "left"
  )
  expect_equal(
    nrow(x),
    nrow(x1)
  )

  x <- joyn_workhorse(
    x          = x1,
    y          = y1,
    by         = "id",
    keep       = "right"
  )
  expect_equal(
    nrow(x),
    nrow(y1)
  )

  x <- joyn_workhorse(
    x          = x1,
    y          = y1,
    by         = "id",
    keep       = "full",
    match_type = "1:1"
  )
  expect_equal(
    nrow(x),
    c(
      x1$id,
      y1[!id %in% x1$id]$id
    ) |>
      length()
  )

  x <- joyn_workhorse(
    x          = x1,
    y          = y1,
    by         = "id",
    keep       = "inner"
  )
  expect_equal(
    nrow(x),
    max(
      x1[
        id %in% intersect(
          x = x1$id,
          y = y1$id
        )
      ] |> nrow(),
      y1[
        id %in% intersect(
          x = x1$id,
          y = y1$id
        )
      ] |> nrow()
    )
  )

  x <- joyn_workhorse(
    x          = x1,
    y          = y1,
    by         = "id",
    keep       = "anti"
  )
  expect_equal(
    x,
    x1[
      !id %in% y1$id
    ]
  )

  x <- joyn_workhorse(
    x          = x1,
    y          = y1,
    by         = "id",
    keep       = "semi"
  )
  expect_equal(
    x,
    x1[
      id %in% y1$id
    ]
  )

})
