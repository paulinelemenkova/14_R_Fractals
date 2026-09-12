library(RColorBrewer)
library(viridisLite)
library(ggplot2)

SierpTriangle <- function(A){
    B <- cbind(A,0*A);
    C <- cbind(A,A);
    D <- rbind(B,C);
return(D);
}
par(mfrow=c(2,2))
for (i in 1:4){
    T <- matrix(1,1,1)
    for (i in 1:i) T <- SierpTriangle(T);
    image(T,col=c("white","red3"),axes=TRUE);
    text(0,1,i,col="black")
}

# Sierpiński Carpet
SierpCarpet <- function(A){
B <- cbind(A,A,A);
C <- cbind(A,0*A,A);
D <- rbind(B,C,B);
return(D);
}
S <- matrix(1,1,1);
for (i in 1:5) S <- SierpCarpet(S);
image(S, col=c("white","purple1"), axes=TRUE, oldstyle=TRUE);
