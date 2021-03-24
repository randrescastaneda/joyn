library(data.table)

x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
               t  = c(1L, 2L, 1L, 2L, NA_integer_),
               x  = 11:15)

y1 = data.table(id = 1:2,
               y  = c(11L, 15L))


x2 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)


y2 = data.table(id = c(1, 2, 5),
                y  = c(11L, 15L, 20L))


y3 <- data.table(x=rep(c("b","a","c"),each=3),
                y=c(1,3,6), v=1:9)

x3 <- data.table(x=c("c","b"),
                v=8:7,
                foo=c(4,2))

usethis::use_data(x1, x2, x3, y1, y2, y3,
                  overwrite = TRUE)

