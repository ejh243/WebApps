library(shiny)
load("data/data.rda")

shinyServer(
  
function(input, output) {
	
	output$title <- renderText({ 
		if(input$probeID %in% rownames(betas)){
			paste(input$probeID)
		} else {
			paste(input$probeID, "not found")
		}
	})
	output$twinPlot1 <- renderPlot({
		if(input$probeID %in% rownames(betas)){
			y_lim = c(min(betas[input$probeID,]), max(betas[input$probeID,]))*100
			par(mar = c(6,5,2,1))
			plot(betas[input$probeID,mz1]*100, betas[input$probeID,mz2]*100, pch = 16, main = "MZ twins", xlab = "DNA methylation (%) twin 1", ylab = "DNA methylation (%) twin 2", xlim = y_lim, ylim = y_lim, cex.lab = 1.3, cex.axis = 1.3)
			mtext(paste("r =", signif(cor(betas[input$probeID,mz1], betas[input$probeID,mz2]),3)), side = 3, line = 0.2, cex = 1.3, adj = 1)
		}
		
	})
	output$twinPlot2 <- renderPlot({
		if(input$probeID %in% rownames(betas)){
			y_lim = c(min(betas[input$probeID,]), max(betas[input$probeID,]))*100
			par(mar = c(6,5,2,1))
			plot(betas[input$probeID,dz1]*100, betas[input$probeID,dz2]*100, pch = 16, main = "DZ twins", xlab = "DNA methylation (%) twin 1", ylab = "DNA methylation (%) twin 2", xlim = y_lim, ylim = y_lim, cex.lab = 1.3, cex.axis = 1.3)
			mtext(paste("r =", signif(cor(betas[input$probeID,dz1], betas[input$probeID,dz2]),3)), side = 3, line = 0.2, cex = 1.3, adj = 1)
		}
	})
	output$acePlot <- renderPlot({
		if(input$probeID %in% rownames(betas)){
			par(mar = c(5,0.5,1,1))
			par(mgp = c(1.75,0.75,0))
			barplot(t(as.matrix(herit[input$probeID,])), horiz = TRUE, col = rainbow(3), legend.text = c("A", "C", "E"), main = "% variance explained", ylab = "", cex.axis = 1.3, cex.lab = 1.3, ylim = c(0,4), axes = FALSE, args.legend = list(horiz = TRUE, box.col = "white", cex = 1.2), cex.main = 1.5)
			axis(1, cex.axis = 1.3, cex.lab = 1.3)
		}
	})
})
