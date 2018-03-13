library(shiny)
source("data.r")

shinyServer(
  
function(input, output, session) {
	
	observeEvent(input$plot, {
	showNotification(paste("Plot will be generated shortly."), duration = 15)
	updateTabsetPanel(session, "inTabset", selected = "single")
	output$title <- renderText({ 
		if(input$probeID %in% rownames(betasMZ1)){
			paste(input$probeID)
			
		} else {
			paste(input$probeID, "not found")
		}
	})
	output$twinPlot1 <- renderPlot({
		if(input$probeID %in% rownames(betasMZ1)){
			y_lim = range(c(range(betasDZ1[input$probeID,]), range(betasDZ2[input$probeID,]), range(betasMZ1[input$probeID,]), range(betasMZ2[input$probeID,])))*100
			par(mar = c(6,5,2,1))
			plot(betasMZ1[input$probeID,]*100, betasMZ2[input$probeID,]*100, pch = 16, main = "MZ twins", xlab = "DNA methylation (%) twin 1", ylab = "DNA methylation (%) twin 2", xlim = y_lim, ylim = y_lim, cex.lab = 1.3, cex.axis = 1.3)
			mtext(paste("r =", signif(cor(betasMZ1[input$probeID,], betasMZ2[input$probeID,]),3)), side = 3, line = 0.2, cex = 1.3, adj = 1)
		}
		
	})
	output$twinPlot2 <- renderPlot({
		if(input$probeID %in% rownames(betasMZ1)){
			y_lim = range(c(range(betasDZ1[input$probeID,]), range(betasDZ2[input$probeID,]), range(betasMZ1[input$probeID,]), range(betasMZ2[input$probeID,])))*100
			par(mar = c(6,5,2,1))
			plot(betasDZ1[input$probeID,]*100, betasDZ2[input$probeID,]*100, pch = 16, main = "DZ twins", xlab = "DNA methylation (%) twin 1", ylab = "DNA methylation (%) twin 2", xlim = y_lim, ylim = y_lim, cex.lab = 1.3, cex.axis = 1.3)
			mtext(paste("r =", signif(cor(betasDZ1[input$probeID,], betasDZ2[input$probeID,]),3)), side = 3, line = 0.2, cex = 1.3, adj = 1)
		}
	})
	output$acePlot <- renderPlot({
		if(input$probeID %in% rownames(herit)){
			par(mar = c(5,0.5,1,1))
			par(mgp = c(1.75,0.75,0))
			barplot(t(as.matrix(herit[input$probeID,])), horiz = TRUE, col = rainbow(3), legend.text = c("A", "C", "E"), main = "% variance explained", ylab = "", cex.axis = 1.3, cex.lab = 1.3, ylim = c(0,4), axes = FALSE, args.legend = list(horiz = TRUE, box.col = "white", cex = 1.2), cex.main = 1.5)
			axis(1, cex.axis = 1.3, cex.lab = 1.3)
		}
	})
	output$aceValues <-renderText({
		if(input$probeID %in% rownames(herit)){				
			paste("Heritability Estimates for ", input$probeID, "\n A = ", signif(herit[input$probeID,1],3), "%, C = ",
			signif(herit[input$probeID,2], 3), "%, E = ", signif(herit[input$probeID,3],3), "%", sep = "")
		}
	})
	})

	output$downloadPlot <- downloadHandler(	
		filename = function() { paste('H2_', input$probeID, ".pdf", sep='') },
		content = function(file){
			pdf(file) 
			layout(matrix(c(1,2,3,3), nrow = 2, byrow = TRUE), heights = c(3,1.5))
			y_lim = range(c(range(betasDZ1[input$probeID,]), range(betasDZ2[input$probeID,]), range(betasMZ1[input$probeID,]), range(betasMZ2[input$probeID,])))*100
			par(mar = c(6,5,2,1))
			plot(betasMZ1[input$probeID,]*100, betasMZ2[input$probeID,]*100, pch = 16, main = "MZ twins", xlab = "DNA methylation (%) twin 1", ylab = "DNA methylation (%) twin 2", xlim = y_lim, ylim = y_lim, cex.lab = 1.1, cex.axis = 1.1)
			mtext(paste("r =", signif(cor(betasMZ1[input$probeID,], betasMZ2[input$probeID,]),3)), side = 3, line = 0.2, cex = 1.1, adj = 1)
			plot(betasDZ1[input$probeID,]*100, betasDZ2[input$probeID,]*100, pch = 16, main = "DZ twins", xlab = "DNA methylation (%) twin 1", ylab = "DNA methylation (%) twin 2", xlim = y_lim, ylim = y_lim, cex.lab = 1.1, cex.axis = 1.1)
			mtext(paste("r =", signif(cor(betasDZ1[input$probeID,], betasDZ2[input$probeID,]),3)), side = 3, line = 0.2, cex = 1.1, adj = 1)
			par(mar = c(6,0.5,1,1))
			par(mgp = c(1.75,0.75,0))
			barplot(t(as.matrix(herit[input$probeID,])), horiz = TRUE, col = rainbow(3), legend.text = c("A", "C", "E"), main = "% variance explained", ylab = "", cex.axis = 1.1, cex.lab = 1.1, ylim = c(0,4), axes = FALSE, args.legend = list(horiz = TRUE, box.col = "white", cex = 1.1), cex.main = 1.2)
			axis(1, cex.axis = 1.1, cex.lab = 1.1)
			mtext(paste("Heritability Estimates for ", input$probeID, "\n A = ", signif(herit[input$probeID,1],3), "%, C = ",
			signif(herit[input$probeID,2], 3), "%, E = ", signif(herit[input$probeID,3],3), "%", sep = ""), line = 5, side = 1)
			dev.off()
		}
	)
	dataIn<-reactive({
		inFile <- input$batchQuery
		if (is.null(inFile))
		  return(NULL)
		read.table(inFile$datapath, header = input$header, row.names = NULL)[,1]
	})
	observeEvent(input$upload,{
		showNotification(paste("File will be uploaded shortly."), duration = 15)
		updateTabsetPanel(session, "inTabset", selected = "multiple")
		
	})
	output$batchTable <- renderTable({
		herit[intersect(rownames(herit), dataIn()),]		
		}, include.rownames = TRUE)

		
	output$downloadData <- downloadHandler(	
		filename = function() { paste('H2_', input$batchQuery, sep='') },
		content = function(file) {
		write.table(herit[intersect(rownames(herit), dataIn()),], file)
    }
  )
	
})
