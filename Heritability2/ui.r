library(shiny)

# Define UI for application that plots DNA methylation against age
shinyUI(fluidPage(
  # Application title
  titlePanel(h2("Heritability of DNA methylation", align = "center")),
  headerPanel("Heritability of DNAm"),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
	 textInput("probeID", h4("Enter CpG identifier"), value = "cg00000029"),
	 submitButton(text = "Plot CpG", icon = NULL, width = NULL)
		),
    # Show a plot of the generated distribution
    mainPanel(
	fluidPage(
	  fluidRow(
	  column(12, h4(textOutput("title")), align = "center")
	  ),
	  fluidRow(
	  column(6, plotOutput("twinPlot1", height = "500px")),
	  column(6, plotOutput("twinPlot2", height = "500px"))
	  ),
	  fluidRow(
	  column(12, plotOutput("acePlot", height = "180px"))
	  ),
	  fluidRow(
          column(12, dataTableOutput("aceValues"))
	  )
	)
    )    
  )
 )
)

