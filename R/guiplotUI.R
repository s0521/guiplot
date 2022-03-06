#Header
tital_ui<-function(id="guiplot"){
  ns <- NS(id)
  tagList(
    fluidRow(style='
              background-color:#48D1CC;
             ',
              "guiplot:User-friendly R programming language ploting tools"
             ),
    fixedRow(style='
              background-color:#48D1CC;
              border-style:solid;
              border-width:1px;
              border-color:Black;
              position: fixed;
              top: 0px;
              z-index:9;
              width:100%;
             ',
             column(12,"guiplot:User-friendly R programming language ploting tools"))
  )
}

toolbar_ui<-function(id="guiplot"){
  ns <- NS(id)
  tagList(
    fluidRow('!--toolbar_ui',
             style='background-color:#48D1CC;
             height:35px
             ',
             ),
    fluidRow(style='background-color:#48D1CC;
               border-style:solid;
               border-width:1px;
               border-color:Black;
               position: fixed;
               top: 20px;
               z-index:10;
               width:100%;
             ',
            'toolbar_ui'

            ,actionButton(
              ns("ColseButton"),
              "02.Finish and Close",
              style='
                color:Black;
                background-color:Darkorange;
                font-weight:bold;
                border-color:Black;
                float:right;
              '
            )

            ,actionButton(
              ns("ExecuteButton"),
              "01.Execute!",
              style='
                color:Black;
                background-color:LimeGreen;
                font-weight:bold;
                border-color:Black;
                float:right;
              '
            )

    )
  )
}

geom_type_ui<-function(id="guiplot"){
  ns <- NS(id)
  tagList(
    fluidRow(style='background-color:#48D1CC;
             border-style:solid;
             border-width:1px;
             border-color:Black',
             "geom type",
             checkboxGroupInput(ns("geom_type_1variable"),
                                label = NULL, #h3("geom type of 1 variable"),
                                choices = list(
                                  "area",
                                  "density",
                                  "dotplot",
                                  "freqpoly",
                                  "histogram",
                                  "bar",
                                  "col"
                                  ),
                                selected = NULL,
                                inline=TRUE
                                ),
             checkboxGroupInput(ns("geom_type_2variable"),
                                label = NULL, #h3("geom type"),
                                choices = list(
                                  "line",
                                  "point",
                                  "ribbon",
                                  "qq_line",
                                  "quantile",
                                  "rug",
                                  "segment",
                                  "smooth",
                                  "text",
                                  "boxplot",
                                  "violin",
                                  "bin2d",
                                  "density2d",
                                  "path",
                                  "step"
                                ),
                                selected = c("point","line"),
                                inline=TRUE
             ),
             checkboxGroupInput(ns("geom_type_other"),
                                label = NULL, #h3("geom type"),
                                choices = list(
                                  "crossbar",
                                  "errorbar",
                                  "linerange",
                                  "pointrange",
                                  "map",
                                  "contour",
                                  "raster",
                                  "tile",
                                  "polygon",
                                  "rect"
                                ),
                                selected = NULL,
                                inline=TRUE
             ),
             )
  )
}

#Data and Plot
# setup_ui<-function(id="guiplot") {
#   ns <- NS(id)
#   tabPanel("Setup Panel",
#     navlistPanel(
#       well = TRUE,
#       fluid = TRUE,
#       widths = c(3, 9),
#       # tabPanel(
#       #   setup_tabPanel_name_ui(id),
#       #   setup_tabPanel_value_ui(id)
#       # ),
#       setup_tabPanel_panel(id),
#       tabPanel(
#         "second data"
#       )
#     ),
#     plot_ui("guiplot")
#   )
# }
setup_tabPanel_panel2<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel(
    'test01',
    fluidPage(
      style='float:left',
      'test02'
    )
  )
}


setup_tabPanel_panel<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel(
    textOutput(ns('tab1')),
    fluidPage(
      style='float:left',
      DTOutput(ns('dt'))
    )
  )
}

# setup_tabPanel_name_ui<-function(id="guiplot") {
#   #first Data
#   ns <- NS(id)
#   textOutput(ns('tab1'))
# }
# setup_tabPanel_value_ui<-function(id="guiplot") {
#   #first Data
#   ns <- NS(id)
#   fluidPage(
#     style='float:left',
#     DTOutput(ns('dt'))
#   )
# }

results_ui<-function(id="guiplot") {
  ns <- NS(id)

    navlistPanel(
      well = TRUE,
      fluid = TRUE,
      widths = c(3, 9),
      # "data",
      # tabPanel(
      #   "data"
      # ),
      "plot",
      tabPanel(
        "plot",
        plotOutput(ns('Results_Plot1'),width = "auto", height = "auto")
      ),
      "text",
      tabPanel(
        "text",
        verbatimTextOutput(ns('Results_Text1'))
      ),
      "other",
      tabPanel(
        "other"
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
        "themes",
        tagList(
          fluidRow(
            column(3,
                   radioButtons(ns("themes"), "themes",
                                c("theme_gray" = "theme_gray",
                                  "theme_bw" = "theme_bw",
                                  "theme_linedraw" = "theme_linedraw",
                                  "theme_light" = "theme_light",
                                  "theme_dark" = "theme_dark",
                                  "theme_minimal" = "theme_minimal",
                                  "theme_classic" = "theme_classic",
                                  "theme_void" = "theme_void",
                                  "theme_minimal" = "theme_minimal")
                                )
            )
          )
        )
      ),
      tabPanel(
        "Layout",
        tagList(
          fluidRow(
            column(3,
                   "Preview Plot Set(pixels)",
                   numericInput(ns('web_plot_height'),'web plot height(pixels)',250),
                   numericInput(ns('web_plot_width'),'web plot width(pixels)',500),
                   numericInput(ns('web_plot_scale'),'web plot scale',2,min = 0.1, max = 100, step = 0.1)
            ),
            column(3,
                   "Output Plot Set(cm)",
                   numericInput(ns('output_plot_height'),label='output plot height(cm)',4,max=4,min=4),
                   numericInput(ns('output_plot_width'),'output plot width(cm)',8),
                   numericInput(ns('output_plot_dpi'),'output plot DPI',300)
            ),
            column(3,
                   numericInput(ns('Outer_Margin'),'Outer Margin',0)
            )
          )
        )
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
                                  "logit" = "logit",
                                  "probability" = "probability",
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
                   numericInput(ns('Y_expand_p'),'expand_plot',0.05),
                   numericInput(ns('Y_expand_u'),'expand_unit',0)
            )
          )
        )
      ),
      tabPanel(
        "Y2"
      ),
    tabPanel(
      "Reference Lines ",
	  tabsetPanel(
		  tabPanel(
				"X(vline)",
				#DTOutput(ns('vline'))
				fluidPage(
				 "Preview Plot Set(pixels)",
					style='float:left',
					DTOutput(ns('vline'), width = "100%", height = "auto")
				)
			),
		  tabPanel("Y(hline)"),
		  tabPanel("Y2()"),
		  tabPanel("unity(abline)")
	  )
    )
  )
}

setup_tabPanel_panel<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel(
    textOutput(ns('tab1')),
    fluidPage(
      style='float:left',
      DTOutput(ns('dt'))
    )
  )
}


guiplotUI <- fluidPage(
  #Header
  tital_ui("guiplot"),
  toolbar_ui("guiplot"),
  geom_type_ui("guiplot"),

  ####################################
  #Data and Plot
  tabsetPanel( id="ChildTabset",
      ##Setup Panel UI
      tabPanel(title = "Setup Panel",
               uiOutput("ui"),
               plot_ui("guiplot")
      ),
	    ##Results Panel UI
      tabPanel(title = "Results Panel",
	      results_ui("guiplot")
      )
  ),
  # plot_ui("guiplot"),
  ####################################

  #Object Options
  object_options_ui("guiplot"),

  # JS customer
  tags$script(HTML(
    '
    $("ul:gt(1) a").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("height","25px");
    '
  ))
)
