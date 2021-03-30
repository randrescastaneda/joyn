library(data.table)
library(dplyr)

test_that("slect by vars when no specified", {
  expect_equal(merge(x1, y1, verbose = FALSE),
               merge(x1, y1, verbose = FALSE, by = "id")
               )

})


test_that("Erro if no common variables", {
  xf <- copy(x1)
  xf[, id := NULL]
  expect_error(merge(xf, y1))
})

test_that("m:m and 1:1 gives the same if data is correct", {

  expect_equal(merge(x2, y2, by = "id", update_values = TRUE, match_type = "1:1"),
               merge(x2, y2, by = "id", update_values = TRUE))

  expect_equal(merge(x2, y2, by = "id", update_NAs = TRUE, match_type = "1:1"),
               merge(x2, y2, by = "id", update_NAs = TRUE))

  expect_equal(merge(x2, y2, by = "id", match_type = "1:1"),
               merge(x2, y2, by = "id", ))

})


test_that("left merge is correct", {
  x <- merge(x1, y1,by = "id",
             keep = "left")
  expect_equal(nrow(x), nrow(x1))


  w <- merge(x2, y2,by = "id", keep = "left", match_type = "1:1")
  expect_equal(nrow(w), nrow(x2))


})

test_that("inverse merge workds", {

  ll <- merge(y3, x3, by = "id", match_type = "m:1", reportvar = FALSE)
  rr <- merge(x3, y3, by = "id", match_type = "1:m", reportvar = FALSE)

  lnames <- names(ll)
  setcolorder(rr, lnames)

  expect_equal(ll,rr)

  lt <- merge(y3, x3, by = "id", match_type = "m:1", reportvar = FALSE, keep = "left")
  rt <- merge(x3, y3, by = "id", match_type = "1:m", reportvar = FALSE, keep = "right")

  lnamest <- names(lt)
  setcolorder(rt, lnamest)

  expect_equal(lt,rt)


})




test_that("Compare with dplyr - left", {

  jn <- merge(x1, y1, by = "id", keep = "left", reportvar = FALSE, sort = FALSE)
  setorder(jn, "id", na.last = TRUE)
  dr <- left_join(x1, y1, by = "id") %>%
    arrange(id)

  expect_equal(jn, dr)



  jn <- merge(x2, y2, by = "id", keep = "left", reportvar = FALSE, sort = FALSE)
  setorder(jn, "id", na.last = TRUE)

  dr <- left_join(x2, y2, by = "id") %>%
    mutate(x = x.x) %>%
    select(-c(x.x, x.y)) %>%
    arrange(id)

  setcolorder(jn, names(dr))

  expect_equal(jn, dr)



})


test_that("Compare with dplyr - right", {

  jn <- merge(x1, y1, by = "id", keep = "right", reportvar = FALSE, sort = FALSE)
  setorder(jn, "id", na.last = TRUE)
  dr <- right_join(x1, y1, by = "id") %>%
    arrange(id)

  expect_equal(jn, dr)



  jn <- merge(x2, y2, by = "id", keep = "right", reportvar = FALSE, sort = FALSE)
  setorder(jn, "id", na.last = TRUE)

  dr <- right_join(x2, y2, by = "id") %>%
    mutate(x = x.x) %>%
    select(-c(x.x, x.y)) %>%
    arrange(id)

  setcolorder(jn, names(dr))

  expect_equal(jn, dr)


})


test_that("Compare with dplyr - full", {

  jn <- merge(x1, y1, by = "id", reportvar = FALSE, sort = FALSE)
  setorder(jn, "id", na.last = TRUE)
  dr <- full_join(x1, y1, by = "id") %>%
    arrange(id)

  expect_equal(jn, dr)



  jn <- merge(x2, y2, by = "id",  reportvar = FALSE, sort = FALSE)
  setorder(jn, "id", na.last = TRUE)

  dr <- full_join(x2, y2, by = "id") %>%
    mutate(x = x.x) %>%
    select(-c(x.x, x.y)) %>%
    arrange(id)

  setcolorder(jn, names(dr))

  expect_equal(jn, dr)

})







