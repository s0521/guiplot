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
  fluidPage(
    style='float:left',
    DTOutput(ns('dt'))
  )
}


plot_ui<-function(id="guiplot"){
	ns <- NS(id)
	tagList(
		fluidRow(
			column(8,plotOutput(ns('plot'),width = "auto", height = "auto"))
		)
	)
}

# tabPanel_layout_ui<-function(id="guiplot") {
#   #first Data
#   ns <- NS(id)
#   tabPanel(
#   "layout",
#   numericInput(ns('Panle_Height'),'Panle Height(mm)',250),
#   numericInput(ns('Panle_Width'),'Panle Width(mm)',500)
#   )
# }
tabPanel_layout_ui<-function(id="guiplot") {
  #first Data
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      sidebarMenu(
        menuItem("layout", tabName = "layout"),
        menuItem("axes", tabName = "axes")
      )
    ),
    dashboardBody(
        # tags$page(tags$style(HTML('
        # tags$style(HTML('
        #   .content-wrapper {min-height: 200px}
        # ')),
         # style = 'min-height: 200px',
    #   tags$style('
    #   .content-wrapper element.style {
    #     min-height: 200px
    #   }
    # '),
      tags$script(src = "js.js"),
      # includeScript(path = "js.js"),
      # tags$script(HTML('
      #                   $("div .content-wrappe").css("min-height",10);
      #                   $("div .content-wrappe").css("minHeight",10);
      #                   $("div .content-wrappe").attr("style","min-height:360px;other-styles");
      #                  ')),



        tabItem(tabName = "layout",
          numericInput(ns('Panle_Height'),'Panle Height(mm)',250),
          numericInput(ns('Panle_Width'),'Panle Width(mm)',500)
        ),
        tabItem(tabName = "axes",
          h1('test')
        )
      )
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
  tabPanel_layout_ui("guiplot")
  # navlistPanel(
  #   well = TRUE,
  #   fluid = TRUE,
  #   widths = c(3, 9),
  #   tabPanel_layout_ui("guiplot"),
  #   tabPanel(
  #     "Axes"
  #   )
  # ) ,


)
