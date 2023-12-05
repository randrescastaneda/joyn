
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
      joyn_workhorse(
        x = x1,
        y = y1
      ),
      joyn_workhorse(
        x  = x1,
        y  = y1,
        by = "id"
      )
    )
  }
)
test_that("Errors if no common variables", {
  xf <- copy(x1)
  xf[
    ,
    id := NULL
  ]
  expect_error(
    joyn_workhorse(xf, y1)
  )
})

test_that("m:m and 1:1 gives the same output if data is correct", {
  expect_equal(
    joyn_workhorse(
      x          = x2,
      y          = y2,
      by         = "id",
      match_type = "1:1"
    ),
    joyn_workhorse(
      x          = x2,
      y          = y2,
      by         = "id",
      match_type = "m:m"
    )
  )

  expect_equal(
    joyn_workhorse(
      x          = x2,
      y          = y2,
      by         = "id",
      match_type = "1:1"
    ),
    joyn_workhorse(
      x          = x2,
      y          = y2,
      by         = "id",
      match_type = "m:m"
    )
  )

  expect_equal(
    joyn_workhorse(
      x          = x2,
      y          = y2,
      by         = "id",
      match_type = "1:1"
    ),
    joyn_workhorse(
      x          = x2,
      y          = y2,
      by         = "id",
      match_type = "m:m"
    )
  )

})


test_that("full joyn is correct", {

  x <- joyn_workhorse(
    x          = x1,
    y          = y1,
    by         = "id",
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


})



test_that("FULL- Compare with base::merge", {

  jn <- joyn_workhorse(
    x  = x1,
    y  = y1,
    by = "id"
  )

  br <- base::merge(
    x   = x1,
    y   = y1,
    by  = "id",
    all = TRUE
  )

  setorderv(
    br,
    "id",
    na.last = TRUE
  )
  setorderv(
    jn,
    "id",
    na.last = TRUE
  )
  setattr(
    br,
    'sorted',
    "id"
  )
  setattr(
    jn,
    'sorted',
    "id"
  ) # ZP: check this

  expect_equal(
    jn,
    br
  )

  jn <- joyn_workhorse(
    x  = x2,
    y  = y2,
    by = "id"
  )

  br <- base::merge(
    x   = x2,
    y   = y2,
    by  = "id",
    all = TRUE
  )
  setorderv(br, "id", na.last = TRUE)
  setorderv(jn, "id", na.last = TRUE)
  setattr(br, 'sorted', "id")
  setattr(jn, 'sorted', "id") # ZP: check this


  setcolorder(jn, names(br))

  expect_equal(jn, br)

})




test_that("match types work", {

  # note: `joyn_workhorse` does not
  #       check whether match_type
  #       is correct
  x <- structure(
      list(
        id = c(1, 1, 2, 3, 4, 7),
        t = c(1L, 2L, 1L, 2L, NA, 2L),
        x = c(16, 12, NA, NA, 15, 12)
      ),
      row.names = c(NA, -6L),
      class = c("data.table", "data.frame")
    )

  y <- structure(
      list(
        id  =  c(1, 1, 2, 3, 4, 6),
        y = c(11L, 15L, 20L, 13L, 10L, 7L),
        x = 16:21
      ),
      row.names = c(NA, -6L),
      class = c("data.table", "data.frame")
    )

  by <- "id"
  jn <- joyn_workhorse(
    x, y, by = by, match_type = "m:m"
  )

  njn <- nrow(jn)

  ux <- x[, .N, by = by]

  uy <- y[, .N, by = by]

  dd <- data.table::merge.data.table(ux, uy, by = "id", all = TRUE)
  setnafill(dd, fill = 1)
  cN <- dd[,
           N := N.x * N.y][, sum(N)]

  expect_equal(njn, cN)

})



