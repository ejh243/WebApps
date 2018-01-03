library(shiny)

# Define UI for application that plots DNA methylation against age
shinyUI(fluidPage(
  # Application title
  titlePanel(h2("Heritability of DNA methylation", align = "center")),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
	tags$h3("Single CpG"),
	 textInput("probeID", h4("Enter CpG identifier"), value = "cg00000029"),
	 submitButton(text = "Plot CpG", icon = NULL, width = NULL),
	tags$hr(),
	
	tags$h3("Multiple CpGs"),
	helpText("To get the heritability estimates for multiple CpGs upload a text file with a single column of CpG identifiers"),
	 fileInput("batchQuery", h4("Choose file with CpG identifiers"), accept = c(
          "text/plain")),
		
		checkboxInput("header", "Header", TRUE),
		downloadButton('downloadData', 'Download Table')
		),
    # Show a plot of the generated distribution
    mainPanel(
	tabsetPanel(id = "inTabset", 
	tabPanel("Single CpG", value = "single",
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
          column(12, h4(textOutput("aceValues")), align = "center")
	  )
	)
    ),
	tabPanel("Multiple CpGs", value = "multiple",	tableOutput("batchTable"))
	)
	)    
  )
 )
)

