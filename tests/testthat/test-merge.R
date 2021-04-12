library(data.table)
# options(joyn.verbose = FALSE)

test_that("slect by vars when no specified", {
  expect_equal(merge(x1, y1),
               merge(x1, y1, by = "id"))

})


test_that("Erros if no common variables", {
  xf <- copy(x1)
  xf[, id := NULL]
  expect_error(merge(xf, y1))
})

test_that("m:m and 1:1 gives the same if data is correct", {
  expect_equal(
    merge(
      x2,
      y2,
      by = "id",
      update_values = TRUE,
      match_type = "1:1"
    ),
    merge(
      x2,
      y2,
      by = "id",
      update_values = TRUE,
      verbose = FALSE
    )
  )

  expect_equal(
    merge(
      x2,
      y2,
      by = "id",
      update_NAs = TRUE,
      match_type = "1:1"
    ),
    merge(
      x2,
      y2,
      by = "id",
      update_NAs = TRUE
    )
  )

  expect_equal(merge(x2, y2, by = "id", match_type = "1:1"),
               merge(x2, y2, by = "id"))

})


test_that("left merge is correct", {
  x <- merge(x1, y1, by = "id",
             keep = "left")
  expect_equal(nrow(x), nrow(x1))


  w <- merge(x2,
             y2,
             by = "id",
             keep = "left",
             match_type = "1:1")
  expect_equal(nrow(w), nrow(x2))


})

test_that("inverse merge workds", {
  ll <-
    merge(
      y3,
      x3,
      by = "id",
      match_type = "m:1",
      reportvar = FALSE
    )
  rr <-
    merge(
      x3,
      y3,
      by = "id",
      match_type = "1:m",
      reportvar = FALSE
    )

  lnames <- names(ll)
  setcolorder(rr, lnames)

  expect_equal(ll, rr)

  lt <-
    merge(
      y3,
      x3,
      by = "id",
      match_type = "m:1",
      reportvar = FALSE,
      keep = "left"
    )
  rt <-
    merge(
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
  jn <- merge(x1,
              y1,
              by = "id",
              reportvar = FALSE)

  br <- base::merge(x1, y1, by = "id", all = TRUE)

  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")

  expect_equal(jn, br)



  jn <-
    merge(x2,
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
    merge(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "left"
    )
  br <- base::merge(x1, y1, by = "id", all.x = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    merge(
      x2,
      y2,
      by = "id",
      reportvar = FALSE,
      keep = "left"
    )
  br <- base::merge(x2, y2, by = "id", all.x = TRUE)
  br[, x := x.x][,
                 c("x.x", "x.y") := NULL]
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})



test_that("RIGHT - Compare with base::merge", {
  jn <-
    merge(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "right"
    )
  br <- base::merge(x1, y1, by = "id", all.y = TRUE)
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    merge(
      x2,
      y2,
      by = "id",
      reportvar = FALSE,
      keep = "right"
    )

  br <- base::merge(x2, y2, by = "id", all.y = TRUE)
  br[, x := x.x][,
                 c("x.x", "x.y") := NULL]
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})


test_that("INNER - Compare with base::merge", {
  jn <-
    merge(
      x1,
      y1,
      by = "id",
      reportvar = FALSE,
      keep = "inner"
    )
  br <- base::merge(x1, y1, by = "id")
  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  expect_equal(jn, br)


  jn <-
    merge(
      x2,
      y2,
      by        = "id",
      reportvar = FALSE,
      keep      = "inner"
    )
  br <- base::merge(x2, y2, by = "id")

  br[, x := x.x][,
                 c("x.x", "x.y") := NULL]

  setorderv(br, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})


test_that("match types work", {
  expect_error(merge(
    x3,
    y3,
    by = "id",
    match_type = "1:1"
  ))
  expect_error(merge(
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
  jn <- merge(x, y, by = by, match_type = "m:m")

  njn <- nrow(jn)

  ux <- x[, .N, by = by]

  uy <- y[, .N, by = by]

  dd <- merge.data.table(ux, uy, by = "id", all = TRUE)
  setnafill(dd, fill = 1)
  cN <- dd[,
           N := N.x * N.y][, sum(N)]

  expect_equal(njn, cN)

})


test_that("Update NAs", {
  # update NAs in x variable form x
  jn <- merge(x2,
              y2,
              by = "id",
              update_NAs = TRUE)

  idx <- x2[is.na(x), "id"]

  expect_equal(jn[idx, on = "id"][, x], y2[idx, on = "id"][, x])


})


test_that("Update actual values", {

  jn <-
    merge(x2,
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


test_that("y vars are extracted correctly", {
  yvars <- "y"
  jn <- merge(x2,
              y2,
              by = "id",
              yvars = yvars)

  expect_equal(names(jn), c(names(x2), yvars, "report"))


  jn <-
    merge(
      x2,
      y2,
      by = "id",
      yvars = yvars,
      reportvar = FALSE
    )

  expect_equal(names(jn), c(names(x2), yvars))

  jn <- merge(x2,
              y2,
              by = "id",
              yvars = FALSE)

  expect_equal(names(jn), c(names(x2), "report"))


  yvars <- "reuiou"
  expect_error(merge(x2,y2,by = "id",yvars = yvars))

})

test_that("selection of reportvar", {
  reportvar <- "wijf"
  jn <-
    merge(x2,
          y2,
          by = "id",
          reportvar = reportvar)

  expect_true(reportvar %in% names(jn))

  jn <- merge(x2,
              y2,
              by = "id",
              reportvar = FALSE)

  expect_false("report" %in% names(jn))

  expect_equal(unique(c(names(x2), names(y2))), names(jn))


  jn <- merge(x2,
              y2,
              by = "id",
              reportvar = "t")

  allnames <- unique(c(names(x2), names(y2)))

  newname <- setdiff(names(jn), allnames)

  expect_true(length(newname) > 0)

})



test_that("Keep Y vars works", {
  jn <- merge(x2,
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
  expect_error(merge(xx1, y1))
})


test_that("different names in key vars are working fine", {

  df <- merge(x4, y4, by = c("id1 = id", "id2"))

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
