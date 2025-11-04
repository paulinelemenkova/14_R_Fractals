library{alphahull}
vertices <- koch(side = 2, niter = 4)
plot(vertices[, 1], vertices[, 2], type = "l", asp = TRUE,
main = "Koch snowflake", xlab = "", ylab = "", col = 4)
polygon(vertices[, 1], vertices[, 2] , col = 4)
