png("sir_varyinggrid_constcores.png", width = 700, height = 700)

agents <- c(10201, 22801, 40401, 63001)
lbRw  <- c(139, 314, 559, 861)
hyb   <- c(91.1, 204, 360, 0)
stm   <- c(96.5, 212, 382, 0)

#dev.new(width = 500, height = 500, unit = "px")
plot(agents, lbRw, type="o", col="red", pch="o", lty=1,
     xlab = "Agents", 
     ylab = "Seconds",
     ylim=c(0,1000),
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

points(agents, hyb, col="blue", pch="*")
lines(agents, hyb, col="blue",lty=2)

points(agents, stm, col="dark green",pch="+")
lines(agents, stm, col="dark green", lty=3)

legend("topleft", 
       legend=c("Lock-Based Read-Write","Hybrid", "STM"), 
       col=c("red", "blue", "dark green"),
       lty=c(1,2,3),
       xpd=TRUE,
       cex = 1.5)

# 3. Close the file
dev.off()