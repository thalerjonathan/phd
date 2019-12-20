png("sugarscape_population_dynamics.png", width = 700, height = 700)
par(mar=c(5,5,1,1)+.1)

plot(popSize[0:50,1], popSize[0:50,2], type="l", col="blue", pch="-", lty=1,
     xlab = "Time", 
     ylab = "Population Size",
     ylim=c(200, 500),
     cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)

# 3. Close the file
dev.off()