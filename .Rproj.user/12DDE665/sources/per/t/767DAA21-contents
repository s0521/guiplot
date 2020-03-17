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


guiplotServer = function(input, output, session) {

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
