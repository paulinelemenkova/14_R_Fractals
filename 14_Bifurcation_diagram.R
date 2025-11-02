library(fractal)
# Bifurcation diagram
n <- 80
z <- fractal::mandelbrot(iter=n)
par(pty="s")
image(z, col=c(hcl.colors(n=10, pal="Viridis"), "blue"), las=1)
logistic(div=4000, k=128)
