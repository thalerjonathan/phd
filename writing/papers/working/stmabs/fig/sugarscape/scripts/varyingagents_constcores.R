png("sugarscape_varyingagents_constcores.png", width = 700, height = 700)

agents   <- c(500,1000,1500,2000,2500)
seq     <- c(70.1, 145.0, 220.0, 213.0, 193.0)
lb      <- c(67.9, 130.0, 183.0, 181.0, 272.0)
tvar    <- c(69.1, 136.0, 192.0, 214.0, 147.0)
tarray  <- c(25.7, 38.8, 40.1, 49.9, 55.2)

#dev.new(width = 500, height = 500, unit = "px")
plot(agents, seq, type="o", col="red", pch="-", lty=1,
     xlab = "Agents", 
     ylab = "Seconds",
     ylim=c(20, 300),
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

points(agents, lb, col="blue", pch="o")
lines(agents, lb, col="blue",lty=2)

points(agents, tvar, col="dark green",pch="+")
lines(agents, tvar, col="dark green", lty=3)

points(agents, tarray, col="black",pch="*")
lines(agents, tarray, col="black", lty=4)

legend("topleft", 
      legend=c("Sequential", "Lock-Based", "TVar", "TArray"), 
      col=c("red", "blue", "dark green", "black"),
      lty=c(1,2,3,4),
      xpd=TRUE,
      cex = 1.5)

# 3. Close the file
dev.off()