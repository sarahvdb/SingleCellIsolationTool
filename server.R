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
library(DT)					# For datatables
library(Hmisc) 				# For capitalize function
library(shinyBS)
library(knitr)				# For download
library(ggplot2)
library(gsubfn)				# For regular expression matching
library(RMySQL)				# MySQL server access
library(rdrop2)				# Dropbox access

#----------------------------------#
# DEFINE DIRECTORIES
#----------------------------------#

dropboxDir				<- file.path("App")

#----------------------------------#
#	DEFINE VARIABLES
#----------------------------------#

choicesMagnification 	<- c("0x","2x","4x","8x")
choicesDetectionMode	<- c("Manual","Automatic")
choicesBuffer			<- c("Water", "PBS")
choicesVolume			<- c("1 ml", "2 ml", "5 ml")
choicesDeposit			<- c("PCR Tube", "Slide")

#----------------------------------#
# FUNCTIONS
#----------------------------------#

loadExperimentName			<- function(){
		con 					<- dbConnect(MySQL(),
		user 		= 'savan103',
		password 	= 'Rub1csCub3',
		host 		= 'savansdbinstance.c2aavosf8ou5.us-west-2.rds.amazonaws.com',
		dbname		= 'CellCelectorApplication'
	)

	data 						<- dbGetQuery(con, 'SELECT ExperimentName FROM Documentation')
	dbDisconnect(con)
	data
}

loadDataSettings			<- function(){
	con 						<- dbConnect(MySQL(),
		user 		= '********',
		password 	= '********',
		host 		= '********',
		dbname		= '********'
	)

	data 						<- dbGetQuery(con, 'SELECT * FROM Documentation')
	dbDisconnect(con)
	data
	
}

fixUploadedFilesNames 		<- function(x) {
	 if (is.null(x)) {
	   return()
	 }
	 else{
	 	oldNames 	= x$datapath
	 	newNames 	= file.path(dirname(x$datapath),x$name)
	 	file.rename(from = oldNames, to = newNames)
	 	x$datapath 				<- newNames
	 	x
	 }
}


#----------------------------------#
# SERVER.R
#----------------------------------#

server <- function(input,output,session){
  	
  	#----------------------------------#
	# ADD EXPERIMENT(S)
	#----------------------------------#
  	
  	observe({
  		if(input$saveButton == 0){
  			return()
  		}
  		isolate({
  		
  			dateCellCelector	<- paste0(substr(input$dateCellCelector, 7, 10), substr(input$dateCellCelector, 4, 5), substr(input$dateCellCelector, 1, 2))
  			data				<- unlist(loadExperimentName())
  			
  			validate(
   				if(is.null(input$csvfile)){
  						shinyBS::createAlert(session, "alertCsvFileUpload", "alertCsvFileUploadID", title = "You missed something...", content = "Please select a .csv-file to upload!", append = TRUE, style = "warning")
  				}, 
  				need(input$csvfile, "Csv File!"), 
  				
  				if(is.null(input$tiffiles)){
  						shinyBS::createAlert(session, "alertTifFileUpload", "alertTifFileUploadID", title = "You missed something...", content = "Please select .tif-files to upload!", append = TRUE, style = "warning")
  				}, 
  				need(input$tiffiles, "Tif Files!"), 				
  			
  				if(input$experimentName == ""){
  						shinyBS::createAlert(session, "alertExperimentName", "alertExperimentNameID", title = "You missed something...", content = "Please enter an Experiment Name!", append = TRUE, style = "warning")
  				}, 
  				need(input$experimentName, "Experiment Name!"),
  				
  				if(input$experimentName != "" & input$experimentName %in% data){
  						shinyBS::createAlert(session, "alertExperimentName2", "alertExperimentName2ID", title = "Watch out...", content = "The experiment name already exists. Please enter a unique experiment name!", append = TRUE, style = "warning")
  				}, 
  				need((input$experimentName %in% data) == FALSE, "Experiment Name!"),
  				
  				if(input$dateCellCelector == ""){
  						shinyBS::createAlert(session, "alertDateCellCelector", "alertDateCellCelectorID", title = "You missed something....", content = "Please enter a valid date!", append = TRUE, style = "warning")
  				},
  				need(input$dateCellCelector, "Date CellCelector!"),
  				
  				if(input$submitter == ""){
  						shinyBS::createAlert(session, "alertSubmitter", "alertSubmitterID", title = "You missed something....", content = "Please enter a Submitter!", append = TRUE, style = "warning")
  				},
  				need(input$submitter, "Submitter!"),
  				
  				if(is.na(input$isolatedCTCs)){
  						shinyBS::createAlert(session, "alertIsolatedCTCs", "alertIsolatedCTCsID", title = "You missed something....", content = "Please enter the number of isolated CTCs!", append = TRUE, style = "warning")
  				},
  				need(input$isolatedCTCs, "Isolated CTCs!")
  				
  			)
		
		saveData()
		
		shinyjs::reset("addExperiment")
		shinyjs::hide("addExperiment")
		shinyjs::show("thankYou")
			 	
  		})
    })
    
    #----------------------------------#
	# SAVE DATA
	#
    # Save data from user input sheet to 'Documentation'-table of 'CellCelectorApplication'-database
	# Copy the data from the csv-file to 'Results'-table of 'CellCelectorApplication'-database
    #----------------------------------#
    
    saveData		<- reactive({
    
    	# CREATE DATAFRAME 'DATA' FROM USER INPUT SHEET
    	submitter					<- capitalize(input$submitter)
		data						<- data.frame(input$experimentName, input$dateCellSearch, input$dateCellCelector, submitter, input$detectedCTCs, input$isolatedCTCs, input$controlCells, input$detectionMode, input$magnification, input$buffer, input$volume, input$deposit)
    	colnames(data)				<- c('ExperimentName','DateCellSearch','DateCellCelector','Submitter','DetectedCTCs','IsolatedCTCs','ControlCells','DetectionMode','Magnification','BufferMedium','PickingVolume','Deposit')
   	
   		# CREATE DATAFRAME 'CSV' FROM SELECTED .CSV-FILE
   		inCsvFile 					<- input$csvfile
    	csv 						<- read.csv2(inCsvFile$datapath)				# Read in csv file
    	
    	# MODIFY CSV-FILE TO INCLUDE COLUMN 'EXPERIMENTNAME'
    	experimentName	<- input$experimentName							# Add column ExperimentName to dataframe
    	ExperimentName 	<- rep(experimentName, dim(csv)[1])
    	csv				<- cbind(ExperimentName, csv)
   		
   		# CONNECT TO MYSQL SERVER
		con <- dbConnect(MySQL(),
			user 		= '*******',
			password 	= '*******',
			host 		= '*******',
			dbname		= '*******'
		)
		
		# APPEND DATAFRAME 'DATA' TO EXISTING 'DOCUMENTATION' TABLE
		dbWriteTable(conn = con, name = 'Documentation', data, append = TRUE, row.names = FALSE)
		
		# APPEND DATAFRAME 'CSV' TO EXISTING 'RESULTS' TABLE
		dbWriteTable(conn = con, name = 'Results', csv, header = TRUE, append = TRUE, row.names = FALSE)
		
		# COPY SELECTED .TIF-FILES TO DROPBOX
    	#inTifFiles 					<- fixUploadedFilesNames(input$tiffiles)
    	#tif							<- inTifFiles$datapath
    	#for (i in 1:length(tif)){
    	#	drop_upload(tif[i], dest = dropboxDir)
    	#}
		
		# COPY SELECTED .TIF-FILES TO WWW FOLDER
		if(is.null(input$tiffiles)){
			return()
		}
		else{
		    inTifFiles 					<- fixUploadedFilesNames(input$tiffiles)
    		tif							<- inTifFiles$datapath
			file.copy(tif, "./www/")
		}
		
		
		# DISCONNECT FROM MYSQL SERVER
		dbDisconnect(con)
   
    })
    
    # CLOSE ALL ALERTS
    observeEvent(input$csvfile, {
		closeAlert(session, 'alertCsvFileUploadID')
    })

    observeEvent(input$tiffiles, {
		closeAlert(session, 'alertTifFileUploadID')
    })

    observeEvent(input$experimentName, {
		closeAlert(session, 'alertExperimentNameID')
    })
    
    observeEvent(input$experimentName, {
		closeAlert(session, 'alertExperimentName2ID')
    })
    
    observeEvent(input$dateCellCelector, {
		closeAlert(session, 'alertDateCellCelectorID')
    })
    
    observeEvent(input$submitter, {
		closeAlert(session, 'alertSubmitterID')
    })

    observeEvent(input$isolatedCTCs, {
		closeAlert(session, 'alertIsolatedCTCsID')
    })
	
	observeEvent(input$addAnother, {
  		shinyjs::show("addExperiment")
  		shinyjs::hide("thankYou")
	})	
    
	#----------------------------------#
	# THANK YOU MESSAGE
	#----------------------------------#
    
    output$thankYouMessage 	<- renderUI({
    	HTML(
        	paste0("Thank you for your submission.", '<br/>', "Your experiment has been successfully uploaded.", '<br/>', '<br/>', '<br/>')
        )	
    })
    
    #----------------------------------#
	# VIEW EXPERIMENT(S)
	#----------------------------------#
	
	# VIEW EXPERIMENT(S) START TAB
	observe({
		if(input$tabs == "view"){

			closeAlert(session, 'alertExperimentNameID')
			closeAlert(session, 'alertExperimentName2ID')
			closeAlert(session, 'alertCsvFileUploadID')
			closeAlert(session, 'alertTifFileUploadID')
			closeAlert(session, 'alertDateCellCelectorID')
			closeAlert(session, 'alertSubmitterID')
			closeAlert(session, 'alertIsolatedCTCsID')

			shinyjs::hide("viewAll")
			shinyjs::show("choose")

		}

	})
	
	output$tableChoose		<- DT::renderDataTable(
		loadDataChoose(),
		rownames = FALSE,
		options = list(paging = FALSE, searching = TRUE, sort = TRUE, info = FALSE, columnDefs = list(list(className = "dt-center", targets = "_all")))
	)
	
	# SAVE EXPERIMENT SELECTION (ROW SELECTION) IN VARIABLE S
	s 						<- reactive({
		input$tableChoose_rows_selected
	})
	
	# SHOW WARNING IF NO EXPERIMENTS ARE SELECTED
	# SHOW 'VIEW' TAB IF AT LEAST ONE EXPERIMENT IS SELECTED
	observe({
  		if(input$submitChoice == 0){
   			return()
  		}
  		isolate({
  		  	validate(
  		  		need(s(), "Please select at least one row")
  		  	)
  		  	
  		  	shinyjs::show("viewAll")
  		  	shinyjs::hide("choose")
  		})  
  		
	})
	
	# SHOW VIEW 'EXPERIMENT(S)' -> START TAB
	loadDataChoose				<- reactive({
		if(input$saveButton == 0 | input$saveButton){
			con 						<- dbConnect(MySQL(),
				user 		= '*******',
				password 	= '*******',
				host 		= '*******',
				dbname		= '*******'
			)

			data 						<- dbGetQuery(con, 'SELECT ExperimentName, DateCellCelector, Submitter FROM Documentation')
			dbDisconnect(con)
			data
		}
	})
	
	# SHOW 'VIEW EXPERIMENT(S)' -> 'SETTINGS' TAB
	output$tableSettings	<- DT::renderDataTable(
		loadDataSettings()[s(),],
		class = "compact", escape = FALSE, rownames = FALSE, 
		options = list(paging = FALSE, searching = TRUE, sort = TRUE, info = FALSE, columnDefs = list(list(className = "dt-center", targets = "_all")))
	)
	
	# GENERATE TABLE FOR 'VIEW EXPERIMENT(S)' -> 'RESULTS' TAB
	loadDataResults					<- function(){
		selectedExperiments				<- unlist(loadExperimentName())[s()]
		
		con 							<- dbConnect(MySQL(),
			user 		= 'savan103',
			password 	= 'Rub1csCub3',
			host 		= 'savansdbinstance.c2aavosf8ou5.us-west-2.rds.amazonaws.com',
			dbname		= 'CellCelectorApplication'
		)
		
		paste(selectedExperiments, collapse = ", ")
		query 							<- sprintf("SELECT * FROM Results WHERE ExperimentName IN ('%s')", paste(selectedExperiments, collapse = "', '"))
		data							<- dbGetQuery(con, query)
		dbDisconnect(con)
		
		table							<- data.frame(matrix(0, nrow = dim(data)[1], ncol = 6))
		rownames(table)					<- NULL
		colnames(table)					<- c("Before","After","BF","DAPI","Cy5","FITC")
	
		#----------------------------------#
		# LOAD TIF IMAGES FROM DROPBOX
		#----------------------------------#
		#link <- drop_media("App/blank.jpg")$url
		#typeof(link)
		#print(link)
	
		#----------------------------------#
		# LOAD IMAGES FROM WWW FOLDER
		#----------------------------------#
		table$"Before"					<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = "blank.jpg" height = "150"/><span><img src = "blank.jpg" height = "500"></span></a></div>')
		table$"After"					<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = "blank.jpg" height = "150"/><span><img src = "blank.jpg" height = "500"></span></a></div>')
		table$"BF"						<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = "blank.jpg" height = "150"/><span><img src = "blank.jpg" height = "500"></span></a></div>')
		table$"DAPI"					<- paste0('<div id="thumbwrap"> <a class = "thumb2" href = "#"><img src = "blank.jpg" height = "150"/><span><img src = "blank.jpg" height = "500"></span></a></div>')
		table$"Cy5"						<- paste0('<div id="thumbwrap"> <a class = "thumb2" href = "#"><img src = "blank.jpg" height = "150"/><span><img src = "blank.jpg" height = "500"></span></a></div>')
		table$"FITC"					<- paste0('<div id="thumbwrap"> <a class = "thumb2" href = "#"><img src = "blank.jpg" height = "150"/><span><img src = "blank.jpg" height = "500"></span></a></div>')

		data2 <- cbind(data[,1:3], table, data[,4:ncol(data)])
		
		rownames(data2) <- NULL
		colnames(data2) <- c("ExperimentName","ParticleID","DestinationWell","Before","After","BF","DAPI","Cy5","FITC","BF_Area","BF_Diameter","BF_ShapeFactor","Cy5_Area","Cy5_Diameter","Cy5_ShapeFactor","Cy5_GrayValueMean","DAPI_Area","DAPI_Diameter","DAPI_ShapeFactor","DAPI_GrayValueMean","FITC_Area","FITC_Diameter","FITC_ShapeFactor","FITC_GrayValueMean")
		
		for (i in 1:(dim(data2)[1])){
			if(file.exists(paste0("www/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_Before.tif"))){
				beforeLink			<- paste0("/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_Before.tif")
				data2$Before[i] 	<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = ',beforeLink,' height = "150"/><span><img src = ',beforeLink,' height = "500"></span></a></div>')
			}

			if(file.exists(paste0("www/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_After.tif"))){
				AfterLink		<- paste0("/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_After.tif")
				data2$After[i] 	<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = ',AfterLink,' height = "150"/><span><img src = ',AfterLink,' height = "500"></span></a></div>')
			}

			if(file.exists(paste0("www/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_BF.tif"))){
				BFLink			<- paste0("/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_BF.tif")
				data2$BF[i] 	<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = ',BFLink,' height = "150"/><span><img src = ',BFLink,' height = "500"></span></a></div>')
			}
			
			if(file.exists(paste0("www/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_DAPI.tif"))){
				DAPILink		<- paste0("/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_DAPI.tif")
				data2$DAPI[i] 	<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = ',DAPILink,' height = "150"/><span><img src = ',DAPILink,' height = "500"></span></a></div>')
			}

			if(file.exists(paste0("www/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_Cy5.tif"))){
				Cy5Link			<- paste0("/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_Cy5.tif")
				data2$Cy5[i] 	<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = ',Cy5Link,' height = "150"/><span><img src = ',Cy5Link,' height = "500"></span></a></div>')
			}

			if(file.exists(paste0("www/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_FITC.tif"))){
				FITCLink		<- paste0("/",data2$ExperimentName[i],"_",data2$ParticleID[i],"_FITC.tif")
				data2$FITC[i] 	<- paste0('<div id="thumbwrap"> <a class = "thumb" href = "#"><img src = ',FITCLink,' height = "150"/><span><img src = ',FITCLink,' height = "500"></span></a></div>')
			}
			
		}
		
		data2
	}
	
	output$image <- renderUI({
		link <- drop_media("App/blank.jpg")
		tags$img(src = drop_media("App/blank.jpg")$url)
	})
	
	# SHOW 'VIEW EXPERIMENT(S)' -> 'RESULTS' TAB
	output$tableResults	<- DT::renderDataTable({
		DT::datatable(loadDataResults()[, input$showColumns, drop = FALSE],
		class = "compact", escape = FALSE, rownames = FALSE, 
		options = list(paging = FALSE, searching = TRUE, sort = TRUE, info = FALSE, columnDefs = list(list(className = "dt-center", targets = "_all"))))
	})
		
}