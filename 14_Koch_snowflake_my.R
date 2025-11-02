library(RColorBrewer)
library(viridisLite)

# 1. Step: function to create empty canvas
emptyCanvas <- function(xlim, ylim, bg="lightblue") {
  par(mar=rep(1,4), bg=bg)
  plot(1,type="n", bty="n",
       xlab="", ylab="",
       xaxt="n", yaxt="n",
       xlim=xlim, ylim=ylim)}
# 2. Step: function to create canvas
emptyCanvas(xlim=c(0,1), ylim=c(0,1))
# function to draw a single line
drawLine <- function(line, col="darkblue", lwd=1) {
  segments(x0=line[1], y0=line[2],
           x1=line[3], y1=line[4],
           col=col,lwd=lwd)
}
# function to create entire objects
drawObject <- function(object, col="darkblue", lwd=1) {
  invisible(apply(object, 1, drawLine, col=col, lwd=lwd))
}
# 3. Koch snowflake (after six iterations)
A <- c(0,1e-9)
B <- c(3,5)
C <- c(6,0)
fractal <- matrix(c(A,B,B,C,C,A), nrow=3, byrow=T)
for(i in 1:6) fractal <- iterate(fractal, ifun=koch)
emptyCanvas(xlim=c(-2,8), ylim=c(-2,5))
drawObject(fractal)
