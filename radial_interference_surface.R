
x <- y <- seq(-8*pi, 8*pi, len = 40)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)),
               axes=FALSE,
               color.palette=turbo,
               asp=1)

x <- y <- seq(-9*pi, 9*pi, len = 25)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)),
               axes=FALSE,
               color.palette=turbo,
               asp=1)

a <- seq(-3*pi, 3*pi, len = 38)
b <- a
c <- sqrt(outer(a^2, b^2, "+"))
filled.contour(cos(c^2)*exp(-c/(2*pi)),
               color.palette=viridis,
             plot.axes = { axis(1); axis(2); points(10, 10) },
               asp=1)
#theme_bw()
#theme_dark()
#theme_gray()
#theme_void()
