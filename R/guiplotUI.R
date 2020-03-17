tabPanel_name_ui<-function(id="guiplot") {
  #first Data
  ns <- NS(id)
  textOutput(ns('tab1'))
}

tabPanel_value_ui<-function(id="guiplot") {
  #first Data
  ns <- NS(id)
  DTOutput(ns('dt'))
}


guiplotUI2<-function(id="guiplot"){
	ns <- NS(id)
	tagList(
		#titlePanel("guiplot"),
		#"Import Data list",
		fluidRow(
			column(12,actionButton(ns("ExecuteButton"), "Execute!"))
		),
		fluidRow(
			column(6,plotOutput(ns('plot')))
		),
	)
}

guiplotUI <- fluidPage(
  navlistPanel(
    tabPanel(
      tabPanel_name_ui("guiplot"),
      tabPanel_value_ui("guiplot")
    ),
    tabPanel(
      "second data"
    )
  ) ,
  guiplotUI2("guiplot")
)