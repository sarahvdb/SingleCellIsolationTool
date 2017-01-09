#----------------------------------#
# HEADER
#----------------------------------#

#list.of.packages 		<- c("shiny", "shinydashboard", "shinyjs", "DT", "XML", "Hmisc", "shinyBS", "knitr", "ggplot2", "gsubfn")
#new.packages 			<- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#install.packages(c("shiny","shinydashboard","shinyjs","DT","Hmisc","shinyBS","knitr","ggplot2","gsubfn","RMySQL",'rdrop2'))


library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)				# For datatables
library(Hmisc) 				# For capitalize function
library(shinyBS)
library(knitr)				# For download
library(ggplot2)
library(gsubfn)				# For regular expression matching
library(RMySQL)				# MySQL server access
library(rdrop2)				# Dropbox access

#----------------------------------#
# VARIABLES
#----------------------------------#

choicesMagnification 		<- c("0x","2x","4x","8x")
choicesDetectionMode		<- c("Manual","Automatic")
choicesBuffer			<- c("Water", "PBS")
choicesVolume			<- c("1 ml", "2 ml", "5 ml")
choicesDeposit			<- c("PCR Tube", "Slide")

#----------------------------------#
# DROPBOX AUTHENTICATION
#----------------------------------#

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

#----------------------------------#
# HTML TWEAKS
#----------------------------------#

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 200px;
                                   -webkit-column-count: 4; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 4;    /* Firefox */ 
                                   column-count: 4; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "))))
                                 
controls <- tags$div(align = "left",
					class = "multicol",
					checkboxGroupInput(
						inputId = "showColumns", 
						label = "Select columns:", 
						choices = c("ExperimentName","ParticleID","DestinationWell","Before","After","BF","DAPI","Cy5","FITC","BF_Area","BF_Diameter","BF_ShapeFactor","Cy5_Area","Cy5_Diameter","Cy5_ShapeFactor","Cy5_GrayValueMean","DAPI_Area","DAPI_Diameter","DAPI_ShapeFactor","DAPI_GrayValueMean","FITC_Area","FITC_Diameter","FITC_ShapeFactor","FITC_GrayValueMean"), 
						selected = c("ExperimentName","ParticleID","DestinationWell","Before","After","BF","DAPI","Cy5","FITC","BF_Area","BF_Diameter","BF_ShapeFactor","Cy5_Area","Cy5_Diameter","Cy5_ShapeFactor","Cy5_GrayValueMean","DAPI_Area","DAPI_Diameter","DAPI_ShapeFactor","DAPI_GrayValueMean","FITC_Area","FITC_Diameter","FITC_ShapeFactor","FITC_GrayValueMean"), 
						inline = FALSE
					))

#----------------------------------#
# UI.R
#----------------------------------#

ui = dashboardPage(

	dashboardHeader(
		title = "SCIE Documentation"
	),
	
	dashboardSidebar(
		sidebarMenu(id = "tabs",
			menuItem("Home", icon = icon("home"), tabName = "home"),
			menuItem("Add Experiment(s)", icon = icon("edit"), tabName = "add"),
			menuItem("View Experiment(s)", icon = icon("clone"), tabName = "view"),
			menuItem("Analysis", icon = icon("area-chart"), tabName = "analysis")
#			,
#			menuItem("Performance", icon = icon("line-chart"), tabName = "performance")
		)
	),
	
	dashboardBody(
	
		tags$head(
      		tags$style(HTML("
                .dataTables_wrapper { overflow-x: scroll; }
            " )
            )
    	),
    	tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "thumbwrap.css")
		),
	
		bsAlert("alertExperimentName"),
		bsAlert("alertExperimentName2"),
		bsAlert("alertCsvFileUpload"),
		bsAlert("alertTifFileUpload"),
		bsAlert("alertSubmitter"),
		bsAlert("alertDateCellCelector"),		
		bsAlert("alertIsolatedCTCs"),
		bsAlert("alertUnique"),
		bsAlert("alertDirectoryName"),
    
	
		fluidRow(
			useShinyjs(),
			tabItems(
				tabItem("home",
					box(width = 12,
						title = "Home", status = "primary", solidHeader = TRUE, collapsible = FALSE,	
						div(
							id = "home",
							htmlOutput('homeText'),
							br(),
							HTML('<center><img src="cellcelector.jpg" width="400"></center>')
						)
					)
				),	
					
				tabItem("add",
					box(width = 12,
						title = "Add experiment(s)", status = "primary", solidHeader = TRUE, collapsible = FALSE,	
						div(
							id = "addExperiment",
							textInput("experimentName", label= h5("Experiment Name*"), value = ""),
							fileInput('csvfile', label = h5('Select .csv-file to upload'), accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv')),
							fileInput('tiffiles', label = h5('Select all .tif-files to upload'), accept = c('.tif'), multiple = TRUE),
							textInput("dateCellSearch", label= h5("DateCellSearch"), value = ""), bsTooltip("dateCellSearch", "Format: dd.mm.yyyy", placement = "top", trigger = "hover", options = NULL),
							textInput("dateCellCelector", label= h5("DateCellCelector*"), value = ""), bsTooltip("dateCellCelector", "Format: dd.mm.yyyy", placement = "top", trigger = "hover", options = NULL),
							textInput("submitter", label= h5("Submitter*"), value = ""),
							numericInput("detectedCTCs", label= h5("Number of detected CTCs (CellSearch)"), value = ""),
							numericInput("isolatedCTCs", label= h5("Number of isolated CTCs (CellCelector)*"), value = ""),
							numericInput("controlCells", label= h5("Number of isolated control cells"), value = ""),
							selectInput("detectionMode", label = h5("Detection mode"), choices = c("" , choicesDetectionMode)),
							selectInput("magnification", label = h5("Magnification"), choices = c("", choicesMagnification)),
							selectInput("buffer", label = h5("Buffer medium"), choices = c("", choicesBuffer)),
							selectInput("volume", label = h5("Picking volume"), choices = c("", choicesVolume)),
							selectInput("deposit", label = h5("Deposit"), choices = c("", choicesDeposit)),
							br(),
							actionButton("saveButton", "Submit", icon("paper-plane"), style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4")
						),
					
						hidden(
							div(id = "thankYou",
								htmlOutput("thankYouMessage"),
								actionButton("addAnother", "Add another experiment", style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4")
							)
						)
					)
				),
				
				tabItem("view", 
					div(id = "choose",
						box(width = 12,
							title = "Choose experiment(s)", status = "primary", solidHeader = TRUE,
							DT::dataTableOutput("tableChoose"),
							br(),
							actionButton("submitChoice", "Submit", icon("paper-plane"), style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4")
							)
					),
					hidden(
						div(id="viewAll",
							tabBox(
    							width = 12,
								tabPanel("Settings",
									DT::dataTableOutput("tableSettings")
								),
								tabPanel("Results",
									#fluidRow(tweaks, column(width = 12, controls),
									#column(width = 8, DT::dataTableOutput("tableResults")))
									tweaks,
									controls,
									DT::dataTableOutput("tableResults"),
									br(),
									actionButton("download", label = "Download", icon("download"), style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4"),
									actionButton("switchTab", label = "Further analysis", icon("area-chart"), style= "color: #fff; background-color: #3c8dbc; border-color: #2e6da4") 
								)
							)
						)
					)
				),
				
				tabItem("analysis",
					div(id = "analysisDiv",
						box(
							width = 12,
							title = "Analysis", status = "primary", solidHeader = TRUE, collapsible = FALSE
						)	
					)
				)
#				,
#				
#				tabItem("performance",
#					div(id = "performanceDiv",
#						box(
#							width = 12,
#							title = "Performance", status = "primary", solidHeader = TRUE, collapsible = FALSE
#						)	
#					)
#				)	
			)
		)
	)	
)
