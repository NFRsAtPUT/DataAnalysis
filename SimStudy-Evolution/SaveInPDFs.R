
pdf(paste("./output/ValuesSim-All-NoS", ".pdf", sep=''), width = 20, height =8)
valueDirPlot_InitWithMultiple <- valueDirPlot_InitWithMultiple + labs(title="A. Multiple simulations") 
valuePlotBar40 <- valuePlotBar40 + labs(title="B. Frequency analysis")
valueWithReplPlotS <- valueWithReplPlotS + labs(title="C. Pseudo-outliers analysis")
multiplot(plotlist = list(valueDirPlot_InitWithMultiple, valuePlotBar40, valueWithReplPlotS), cols=3)
dev.off()
dev.off()

pdf(paste("./output/MESim-All-NoS", ".pdf", sep=''), width = 20, height =8)
multiplot(plotlist = list(mePercPlot, dataMEPrPlotBar, meWithReplPlot), cols=3)
dev.off()
dev.off()

pdf(paste("./output/UtilSim-All-NoS", ".pdf", sep=''), width = 20, height =8)
multiplot(plotlist = list(utilPercPlot, utilPercPlot402, utilWithReplPlot), cols=3)
dev.off()
dev.off()