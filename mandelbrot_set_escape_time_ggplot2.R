install.packages("ggplot2")
library(ggplot2)

generate_fractal <- function(x_min, x_max, y_min, y_max, resolution = 1000, max_iter = 1000) {
  x <- seq(from = x_min, to = x_max, length.out = resolution)
  y <- seq(from = y_min, to = y_max, length.out = resolution)
  
  fractal <- expand.grid(x = x, y = y)
  
  fractal$iteration <- with(fractal, mapply(function(x, y) {
    z <- complex(real = x, imaginary = y)
    c <- z
    iteration <- 0
    
    while(Mod(z) <= 2 && iteration < max_iter) {
      z <- z^2 + c
      iteration <- iteration + 1
    }
    
    return(iteration)
  }, x, y))
  
  return(fractal)
}

mandelbrot <- generate_fractal(-2, 1, -1.5, 1.5)

ggplot(mandelbrot, aes(x = x, y = y, colour = iteration)) +
    geom_point(size = 0.5, shape = 16) +
    scale_color_gradientn(colors = rainbow(7)) +
    theme_void() +
    theme(legend.position = "none")
