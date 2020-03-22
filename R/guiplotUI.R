#Header
tital_ui<-function(id="guiplot"){
  ns <- NS(id)
  tagList(
    fluidRow(style='background-color:#48D1CC;
             border-style:solid;
             border-width:1px;
             border-color:Black',
	"guiplot:User-friendly R programming language ploting tools")
  )
}

toolbar_ui<-function(id="guiplot"){
  ns <- NS(id)
  tagList(
    fluidRow(style='background-color:#48D1CC;
             border-style:solid;
             border-width:1px;
             border-color:Black',
	  column(6,'toolbar_ui'),
      column(3,actionButton(
        ns("ExecuteButton"),
        "Execute!",
        style='color:Black;background-color:LimeGreen;font-weight:bold;border-color:Black'
        )),
      column(3,actionButton(
        ns("ColseButton"),
        "Finish and Close",
        style='color:Black;background-color:Darkorange;font-weight:bold;border-color:Black'
        ))
    )
  )
}

#Data and Plot
setup_ui<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel("Setup Panel",
    navlistPanel(
      well = TRUE,
      fluid = TRUE,
      widths = c(3, 9),
      tabPanel(
        setup_tabPanel_name_ui(id),
        setup_tabPanel_value_ui(id)
      ),
      tabPanel(
        "second data"
      )
    ),
    plot_ui("guiplot")
  )
}

setup_tabPanel_name_ui<-function(id="guiplot") {
  #first Data
  ns <- NS(id)
  textOutput(ns('tab1'))
}
setup_tabPanel_value_ui<-function(id="guiplot") {
  #first Data
  ns <- NS(id)
  fluidPage(
    style='float:left',
    DTOutput(ns('dt'))
  )
}

results_ui<-function(id="guiplot") {
  tabPanel("Results Panel",
    navlistPanel(
      well = TRUE,
      fluid = TRUE,
      widths = c(3, 9),
      "data",
      tabPanel(
        "data"
      ),
      "plot",
      tabPanel(
        "plot"
      ),
      "text",
      tabPanel(
        "text"
      ),
      "other",
      tabPanel(
        "other"
      )
    )
  )
}

plot_ui<-function(id="guiplot"){
	ns <- NS(id)
	tagList(
		fluidRow(
			column(8,offset = 2,plotOutput(ns('plot'),width = "auto", height = "auto"))
		)
	)
}

#Object Options
object_options_ui<-function(id="guiplot") {
  ns <- NS(id)
  navlistPanel(
    fluid = TRUE,
    widths = c(3, 9),
    "Plot",
      tabPanel(
        "Layout",
        numericInput(ns('Panle_Height'),'Panle Height(mm)',250),
        numericInput(ns('Panle_Width'),'Panle Width(mm)',500)
      ),
      tabPanel(
        "Lattice"
      ),
    "Axes",
      tabPanel(
        "X",
        tagList(
          fluidRow(
            column(3,
              radioButtons(ns("X_Scale"), "Scale",
                          c("Linear" = "identity",
                            "log10" = "log10",
                            "log2" = "log2",
                            "logit" = "logit",
                            "probability" = "probability",
                            "sqrt" = "sqrt"))
            ),
            column(3,
                   radioButtons(ns("X_Range"), "Range",
                                c("Auto-scale Uniform" = "none",
                                  "Custom" = "Custom")),
                   numericInput(ns('X_Minimum'),'Minimum',0),
                   numericInput(ns('X_Maximum'),'Maximum',100)
            ),
            column(3,
                   numericInput(ns('X_expand_p'),'expand_plot',0.05),
                   numericInput(ns('X_expand_u'),'expand_unit',0)
            ),
          )
        )
      ),
      tabPanel(
        "Y",
        tagList(
          fluidRow(
            column(3,
                   radioButtons(ns("Y_Scale"), "Scale",
                                c("Linear" = "identity",
                                  "log10" = "log10",
                                  "log2" = "log2",
                                  "ln" = "ln",
                                  "sqrt" = "sqrt"))
            ),
            column(3,
                   radioButtons(ns("Y_Range"), "Range",
                                c("Auto-scale Uniform" = "none",
                                  "Custom" = "Custom")),
                   numericInput(ns('Y_Minimum'),'Minimum',0),
                   numericInput(ns('Y_Maximum'),'Maximum',100)
            ),
            column(3,
                   numericInput(ns('Y_expand_p'),'expand_s',0.05),
                   numericInput(ns('Y_expand_u'),'expand_e',0)
            )
          )
        )
      ),
      tabPanel(
        "Y2"
      ),
    tabPanel(
      "Reference Lines "
    )
  )
}

guiplotUI <- fluidPage(
  #Header
  tital_ui("guiplot"),
  toolbar_ui("guiplot"),

  #Data and Plot
  tabsetPanel(
	  setup_ui("guiplot"),
	  results_ui("guiplot")
  ),
  # plot_ui("guiplot"),

  #Object Options
  object_options_ui("guiplot"),

  #JS customer
  tags$script(HTML(
    '
    $("ul:gt(1) a").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("height","25px");
    '
  ))
)