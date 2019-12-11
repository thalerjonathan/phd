png("sir_varyingcores_constgrid.png", width = 700, height = 700)
par(mar=c(5,5,1,1)+.1)

cores   <- c(1,2,3,4,5,6,7,8)
seq     <- c(73.9, 73.9, 73.9, 73.9, 73.9, 73.9, 73.9, 73.9)
lbNaive <- c(59.2, 46.5, 44.2, 47.4, 48.1, 49.1, 49.8, 57.2)
lbRw    <- c(55.0, 40.8, 35.8, 34.0, 34.5, 34.8, 35.9, 40.4)
hyb     <- c(51.0, 32.4, 25.5, 22.7, 22.6, 22.3, 22.8, 25.8)
stm     <- c(52.2, 33.2, 26.4, 23.3, 23.0, 23.1, 23.4, 26.2)

#dev.new(width = 500, height = 500, unit = "px")
plot(cores, seq, type="o", col="red", pch="-", lty=1,
     xlab = "Cores", 
     ylab = "Seconds",
     ylim=c(0,80),
     cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)

points(cores, lbNaive, col="orange", pch="o")
lines(cores, lbNaive, col="orange",lty=2)

points(cores, lbRw, col="blue", pch="x")
lines(cores, lbRw, col="blue",lty=3)

points(cores, hyb, col="dark green",pch="+")
lines(cores, hyb, col="dark green", lty=4)

points(cores, stm, col="black",pch="*")
lines(cores, stm, col="black", lty=5)

legend("bottomleft", 
      legend=c("Sequential",
               "Lock-Based Naive",
               "Lock-Based Read-Write",
               "Atomic IO", 
               "STM"), 
      col=c("red", "orange", "blue", "dark green", "black"),
      lty=c(1,2,3,4, 5),
      xpd=TRUE,
      cex = 1.8)

# 3. Close the file
dev.off()