png("sugarscape_varyingcores_constagents.png", width = 700, height = 700)
par(mar=c(5,5,1,1)+.1)

cores   <- c(1,2,3,4,5,6,7,8)
seq     <- c(25.2, 25.2, 25.2, 25.2, 25.2, 25.2, 25.2, 25.2)
lb      <- c(21.0, 20.0, 21.9, 24.0, 26.7, 29.3, 30.0, 31.2)
tvar    <- c(21.1, 22.2, 23.6, 25.2, 31.0, 35.2, 38.7, 49.0)
tarray  <- c(42.0, 24.5, 19.7, 18.9, 20.3, 21.2, 21.0, 21.1)

#dev.new(width = 500, height = 500, unit = "px")
plot(cores, seq, type="o", col="red", pch="-", lty=1,
     xlab = "Cores", 
     ylab = "Seconds",
     ylim=c(10,60),
     cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)

points(cores, lb, col="blue", pch="o")
lines(cores, lb, col="blue",lty=2)

points(cores, tvar, col="dark green",pch="+")
lines(cores, tvar, col="dark green", lty=3)

points(cores, tarray, col="black",pch="*")
lines(cores, tarray, col="black", lty=4)

legend("topleft", 
      legend=c("Sequential", "Lock-Based", "TVar", "TArray"), 
      col=c("red", "blue", "dark green", "black"),
      lty=c(1,2,3,4),
      xpd=TRUE,
      cex = 1.8)

# 3. Close the file
dev.off()