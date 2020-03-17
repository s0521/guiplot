#R is a very anti-human language. The package aims to improve this dilemma and create a user-friendly drawing GUI for R.
#plot engine ggplot2
#shiny runGadget callModule browserViewer plotOutput textOutput actionButton NS fluidPage navlistPanel tabPanel tagList fluidRow column
#'
#' @title guiplot
#' @param data Matrix or data frame
#' @export
#' @return code to reproduce chart.
#' @import shiny ggplot2
#' @importFrom DT datatable DTOutput renderDT JS editData
#' @importFrom rlang parse_expr expr
#'
#' @examples
#' \dontrun{
#' guiplot(PK)
#' }

guiplot <- function(data = NULL) {

res_data <- get_data(data, name = deparse(substitute(data)))
colna<-c("none","xvar","yvar","group")
# Panle_Height<-reactive(input$Panle_Height)
# Panle_Width<-reactive(input$Panle_Width)

guiplotServer = function(input, output, session) {
  # Panl_Height<-reactive({input$Panle_Height})
  # Panl_Width<-reactive({input$Panle_Width})
  callModule(
    module = guiplot_tital_Server,
    id = "guiplot"
  )

  callModule(
    module = guiplot_result_Server,
    id = "guiplot"
  )

  mptable<- callModule(
    module = guiplot_dt_Server,
    id = "guiplot",
    data = res_data,
    colname=colna
  )
  #browser()
  callModule(
    module = guiplot_plot_Server,
    id = "guiplot",
    data = mptable,
    dataname=res_data$guiplot_data_name
  )
}

  runGadget(
    #browser(),
    guiplotUI, guiplotServer, viewer = browserViewer()
  )
}
