library(data.table)

x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
               t  = c(1L, 2L, 1L, 2L, NA_integer_),
               x  = 11:15)

y1 = data.table(id = 1:2,
               y  = c(11L, 15L))


x2 = data.table(id = c(1, 1, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))


y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))


y3 <- data.table(x=rep(c("b","a","c"),each=3),
                y=c(1,3,6), v=1:9)

x3 <- data.table(x=c("c","b"),
                v=8:7,
                foo=c(4,2))

x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))


y4 = data.table(id  = c(1, 2, 5, 6, 3),
                id2 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                x   = c(16:20))

usethis::use_data(x1, x2, x3, y1, y2, y3, x4, y4,
                  overwrite = TRUE)

