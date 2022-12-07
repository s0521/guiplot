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
                                choices = StaticData_geom_type_1variable,
                                selected = NULL,
                                inline=TRUE
                                ),
             checkboxGroupInput(ns("geom_type_2variable"),
                                label = NULL, #h3("geom type"),
                                choices = StaticData_geom_type_2variable,
                                selected = c("point","line"),
                                inline=TRUE
             ),
             checkboxGroupInput(ns("geom_type_other"),
                                label = NULL, #h3("geom type"),
                                choices = StaticData_geom_type_other,
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
setup_tabPanel_panel<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel(
    textOutput(ns('tab1')),
    fluidPage(
      style='float:left',
      fluidRow(
        column(width = 3,
          excelOutput(ns('Rexcle_tb'), width = "100%", height = "100%")
        ),
        column(width = 7,
          DTOutput(ns('dt'))
        )
      )
    )
  )
}


setup_tabPanel_panel01<-function(id="guiplot") {
  ns <- NS(id)
  tabPanel(
    textOutput(ns('tab1')),
    fluidPage(
      style='float:left',
      # excelOutput("Rexcle_tb"),
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
      widths = c(2, 10),
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
        "other",
        verbatimTextOutput(ns('Results_Text2'))
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
    widths = c(2, 10),
    "Plot",
      tabPanel(
        "themes",
        tagList(
          fluidRow(
            column(2,
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
                                ),
                  checkboxInput(ns("coord_flip"), "Axes_flip?(X\u2194Y)", value = FALSE)
            ),
            column(3,
                  textInput(ns('title_label'),'title'),
                  textInput(ns('subtitle_label'),'subtitle'),
                  textInput(ns('caption_label'),'caption'),
                  textInput(ns('tag_label'),'tag')
            ),
            column(7,
                  helpText("For example:
                  geom_smooth(aes(x,y),data=data,method='lm')+theme(panel.grid.major  = element_line(colour = 'gray90'))
                  "),
                  textAreaInput(ns("UGC"), "Additional custom R language code", rows = 10)
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
      tabPanel(
        "Legend",
        tagList(
          fluidRow(
            column(3,
                  checkboxInput(ns('Legend_Visible'),"Legend Visible?",value = TRUE),
                   "Legend_Tital_Label",
                  textInput(ns('Legend_Tital_Color_Label'),'Color Label'),
                  textInput(ns('Legend_Tital_Shape_Label'),'Shape(mark) Label'),
                  textInput(ns('Legend_Tital_Linetype_Label'),'linetype Label')
            ),
            column(3,
                   radioButtons(
                    ns('Legend_Docking'),"Legend_Docking",
                    c(
                      "Relative Position"="Relative_Position",
                      "Absolute Position"="Absolut_Position"
                    ),
                    selected =c("Relative_Position")
                   ),
                   sliderInput(ns("Legend_X_Offset"),"X Offset%",0,100,90),
                   sliderInput(ns("Legend_Y_Offset"),"Y Offset%",0,100,50)

            ),
            column(3,
                   selectInput(
                              ns('Relative_Position_Select'),'Relative Position Select',
                              c(
                                "top"="top",
                                "bottom"="bottom",
                                "left"="left",
                                "right"="right"
                              ),
                              selected =c("right")
                   ),
            )
          )
        )
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
                            "sqrt" = "sqrt")),
              textInput(ns('X_Axis_label'),'X Axis label')
            ),
            column(3,
                   radioButtons(ns("X_Range"), "Range",
                                c("Auto-scale Uniform" = "none",
                                  "Custom" = "Custom")),
                   numericInput(ns('X_Minimum'),'Minimum',0),
                   numericInput(ns('X_Maximum'),'Maximum',100),
                   textInput(ns("X_Tick"),"Customer Axis Scale Tick" )
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
                                  "sqrt" = "sqrt")),
                  textInput(ns('Y_Axis_label'),'Y Axis label')
            ),
            column(3,
                   radioButtons(ns("Y_Range"), "Range",
                                c("Auto-scale Uniform" = "none",
                                  "Custom" = "Custom")),
                   numericInput(ns('Y_Minimum'),'Minimum',0),
                   numericInput(ns('Y_Maximum'),'Maximum',100),
                   textInput(ns("Y_Tick"),"Customer Axis Scale Tick" )
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
        tagList(
          fluidRow("Usage:geom_*line(*intercept=*intercept,color=*color,size=*size,UserCustomerCode)"),
          fluidRow(
          # "X(vline)",
            column(3,
              "","X(vline)",
              textInput(ns('x_intercept'),'x intercept'),
              textInput(ns('x_color'),'x color'),
              textInput(ns('x_size'),'x size'),
              textInput(ns('x_add_UGC'),'x additional UserCustomerCode')
            ),
            column(3,
              "","Y(hline)",
              textInput(ns('y_intercept'),'y intercept'),
              textInput(ns('y_color'),'y color'),
              textInput(ns('y_size'),'y size'),
              textInput(ns('y_add_UGC'),'y additional UserCustomerCode')
            ),
            column(3,
              "","lin(abline)",
              textInput(ns('abline_intercept'),'lin intercept'),
              textInput(ns('abline_color'),'lin color'),
              textInput(ns('abline_size'),'lin size'),
              textInput(ns('abline_add_UGC'),'lin additional UserCustomerCode')
            ),
            column(3,
              checkboxInput(ns('Diagonal_Line'),"Diagonal Line?",value = FALSE),
              textInput(ns('abline_slope'),'lin slope')
            )
          )
        )
      ),
    "Geom",
      tabPanel(
        "geom_Additional_UGC",
        tagList(
          fluidRow(
            "geom Additional User Customer Code",
            helpText("Usage:  geom_line(data=data,aes(x=x,y=y, geom_Additional_AesCode ), geom_Additional_Code )"),
            helpText("For Example Code: method = 'lm' , method = 'glm' ,  color='red', color='blue' , shape=2"),
            DTOutput(ns('geom_Additional_UGC'))
          )
        )
      )
  )
}

# setup_tabPanel_panel<-function(id="guiplot") {
#   ns <- NS(id)
#   tabPanel(
#     textOutput(ns('tab1')),
#     fluidPage(
#       style='float:left',
#       DTOutput(ns('dt'))
#     )
#   )
# }
text_gg_codes_ui <- function(id = "guiplot") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        6,
        "Generated Codes",
        verbatimTextOutput(
          ns("text_gg_codes")
        )
      ),
      column(
        6,
        textAreaInput(
          ns("text_editor"),
          "Just A Text Editor",
          rows = 10
        )
      )
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
  text_gg_codes_ui("guiplot"),

  # JS customer
  tags$script(HTML(
    '
    $("ul:gt(1) a").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("padding","1px");
    $("ul:gt(1) .navbar-brand").css("height","25px");
    '
  ))
)
