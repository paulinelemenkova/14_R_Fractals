#Generate H-Fractals with ggplot The code below starts with setting the initial variables and defines the first line in a data frame. The data frame holds the start and end coordinates and the iteration number.

 library(ggplot2)
 ## H-Fractal Parameters
 p <- 8 ## Number of iterations
 a <- 1 / sqrt(2) ## Shortening parameter
 
 ## Define horizontal starting line
 h_fractal <- data.frame(x1 = -1,
                         y1 = 0,
                         x2 = 1,
                         y2 = 0,
                         m = 0)

The loop defines the iterations and adds a new row to the data frame for each line. First, the direction flips by setting the dx and dy values to either 0 or 1. The second part defines the row numbers for the previously defined lines and the new lines. The code then calculates the coordinates for the new line segments.

 # Generate coordinates (each line on a row)
 for (m in 1:p) {
   ## Flip direction
   dx <- (m - 1) %% 2
   dy <- m %% 2
   # Previous and current line numbers
   i <- (2^(m - 1)):(2^m - 1)
   j <- (2^m):(2^(m + 1) - 1)
   ## Calculate coordinates
   h_fractal[j, "x1"] <- c(h_fractal[i, "x1"] - dx * a^m, h_fractal[i, "x2"] - dx * a^m)
   h_fractal[j, "x2"] <- c(h_fractal[i, "x1"] + dx * a^m, h_fractal[i, "x2"] + dx * a^m)
   h_fractal[j, "y1"] <- c(h_fractal[i, "y1"] - dy * a^m, h_fractal[i, "y2"] - dy * a^m)
   h_fractal[j, "y2"] <- c(h_fractal[i, "y1"] + dy * a^m, h_fractal[i, "y2"] + dy * a^m)
   ## Set line property
   h_fractal[j, "m"] <- m
 }

Lastly, The fractal is visualised with ggplot and the line segment geometry. The colour of the line slowly fades by setting it to the iteration number.

   ## Visualise
 ggplot(h_fractal) +
   aes(x = x1, y = y1, xend = x2, yend = y2,
       col = m) +
   geom_segment(size = .5) +
   scale_size(guide = NULL) +
   scale_colour_gradientn(colours = gray.colors(p), guide = NULL) +
   theme_void()


