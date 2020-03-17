tital_ui<-function(id="guiplot"){
  ns <- NS(id)
  tagList(
    fluidRow(style='background-color:#48D1CC;
             border-style:solid;
             border-width:1px;
             border-color:Black',
      column(3,actionButton(
        ns("ExecuteButton"),
        "Execute!",
        style='color:Black;background-color:LimeGreen;font-weight:bold;border-color:Black'
        )),
      column(6,"guiplot:User-friendly R programming language ploting tools",style='font-weight:bold'),
      column(3,actionButton(
        ns("ColseButton"),
        "Finish and Close",
        style='color:Black;background-color:Darkorange;font-weight:bold;border-color:Black'
        ))
    )
  )
}

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


plot_ui<-function(id="guiplot"){
	ns <- NS(id)
	tagList(
		fluidRow(
			column(8,plotOutput(ns('plot')))
		)
	)
}

tabPanel_layout_ui<-function(id="guiplot") {
  #first Data
  ns <- NS(id)
  tabPanel(
  "layout",
  numericInput(ns('Panle_Height'),'Panle Height(mm)',250),
  numericInput(ns('Panle_Width'),'Panle Width(mm)',500)
  )
}

guiplotUI <- fluidPage(
  style='border-style:solid;
  border-width:1px;
  border-color:Black',
  tital_ui("guiplot"),
  navlistPanel(
    well = TRUE,
    fluid = TRUE,
    widths = c(3, 9),
    tabPanel(
      tabPanel_name_ui("guiplot"),
      tabPanel_value_ui("guiplot")
    ),
    tabPanel(
      "second data"
    )
  ) ,
  plot_ui("guiplot"),
  navlistPanel(
    well = TRUE,
    fluid = TRUE,
    widths = c(3, 9),
    tabPanel_layout_ui("guiplot"),
    tabPanel(
      "Axes"
    )
  ) ,
)
