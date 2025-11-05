emptyCanvas <- function(xlim, ylim, bg="lightslategray") {
  par(mar=rep(1,4), bg=bg)
  plot(1,
       type="n",
       bty="n",
       xlab="", ylab="",
       xaxt="n", yaxt="n",
       xlim=xlim, ylim=ylim)
}
# function to draw a single line
drawLine <- function(line, col="white", lwd=1) {
  segments(x0=line[1],
           y0=line[2],
           x1=line[3],
           y1=line[4],
           col=col,
           lwd=lwd)
}
# wrapper around "drawLine" to draw entire objects
drawObject <- function(object, col="white", lwd=1) {
  invisible(apply(object, 1, drawLine, col=col, lwd=lwd))
}
# example
line1 = c(0,0,1,1)
line2 = c(-3,4,-2,-4)
line3 = c(1,-3,4,3)
mat = matrix(c(line1,line2,line3), byrow=T, nrow=3)
mat
# function to add a new line to an existing one
newLine <- function(line, angle, reduce=1) {
  
  x0 <- line[1]
  y0 <- line[2]
  x1 <- line[3]
  y1 <- line[4]
  
  dx <- unname(x1-x0)                      # change in x direction
  dy <- unname(y1-y0)                      # change in y direction
  l <- sqrt(dx^2 + dy^2)                   # length of the line
  
  theta <- atan(dy/dx) * 180 / pi          # angle between line and origin
  rad <- (angle+theta) * pi / 180          # (theta + new angle) in radians
  
  coeff <- sign(theta)*sign(dy)            # coefficient of direction
  if(coeff == 0) coeff <- -1
  
  x2 <- x0 + coeff*l*cos(rad)*reduce + dx  # new x location
  y2 <- y0 + coeff*l*sin(rad)*reduce + dy  # new y location
  return(c(x1,y1,x2,y2))
  
}
# function to run next iteration based on "ifun()"
iterate <- function(object, ifun, ...) {
  linesList <- vector("list",0)
  for(i in 1:nrow(object)) {
    old_line <- matrix(object[i,], nrow=1)
    new_line <- ifun(old_line, ...)
    linesList[[length(linesList)+1]] <- new_line
  }
  new_object <- do.call(rbind, linesList)
  return(new_object)
}
# function to run next iteration based on "ifun()"
iterate <- function(object, ifun, ...) {
  linesList <- vector("list",0)
  for(i in 1:nrow(object)) {
    old_line <- matrix(object[i,], nrow=1)
    new_line <- ifun(old_line, ...)
    linesList[[length(linesList)+1]] <- new_line
  }
  new_object <- do.call(rbind, linesList)
  return(new_object)
}
# iterator function: recursive tree
tree <- function(line0, angle=30, reduce=.7, randomness=0) {
  
  # angles and randomness
  angle1 <- angle+rnorm(1,0,randomness)  # left branch
  angle2 <- -angle+rnorm(1,0,randomness) # right branch
  
  # new branches
  line1 <- newLine(line0, angle=angle1, reduce=reduce)
  line2 <- newLine(line0, angle=angle2, reduce=reduce)
  
  # store in matrix and return
  mat <- matrix(c(line1,line2), byrow=T, ncol=4)
  return(mat)
  
}

# example: recursive tree (after twelve iterations)
fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
drawObject(fractal)
for(i in 1:12) {
  fractal <- iterate(fractal, ifun=tree, angle=23)
  drawObject(fractal)
}
# example: recursive tree (after 15 iterations with colors)
fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
drawObject(fractal)
for(i in 1:15) {
  fractal <- iterate(fractal, ifun=tree, angle=23)
  drawObject(fractal,col=i)
}

# recursive tree (organic with color)
set.seed(1234)
fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
lwd <- 7
drawObject(fractal, lwd=lwd,)
for(i in 1:17) {
  lwd <- lwd*0.75
  fractal <- iterate(fractal, ifun=tree, angle=29, randomness=9)
  drawObject(fractal, lwd=lwd,col=i+2)
}

library(RColorBrewer)

fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
lwd <- 7
drawObject(fractal, lwd=lwd,)
for(i in 1:10) {
  lwd <- lwd*0.75
  fractal <- iterate(fractal, ifun=tree, angle=29, randomness=9)
  drawObject(fractal, lwd=lwd, col = i+1)
}

colfunc <- colorRampPalette(c("green", "red"))

col = colfunc(10)
