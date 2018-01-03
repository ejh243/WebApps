library(shiny)
load("data/data.rda")

shinyServer(
  
function(input, output, session) {
	
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
	output$aceValues <-renderText({
		if(input$probeID %in% rownames(betas)){				
			paste("Heritability Estimates for ", input$probeID, "\n A = ", signif(herit[input$probeID,1],3), "%, C = ",
			signif(herit[input$probeID,2], 3), "%, E = ", signif(herit[input$probeID,3],3), "%", sep = "")
		}
	})
	dataIn<-reactive({
		inFile <- input$batchQuery
		if (is.null(inFile))
		  return(NULL)
		read.table(inFile$datapath, header = input$header, row.names = NULL)[,1]
	})
	observeEvent(input$batchQuery, {
    updateTabsetPanel(session, "inTabset",
      selected = "multiple")
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
