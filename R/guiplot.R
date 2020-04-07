#R is a very anti-human language. The package aims to improve this dilemma and create a user-friendly drawing GUI for R.
#plot engine ggplot2
#shiny runGadget callModule browserViewer plotOutput textOutput actionButton NS fluidPage navlistPanel tabPanel tagList fluidRow column
#'
#' @title guiplot
#' @param ... Matrix or data frame
#' @export
#' @return code to reproduce chart.
#' @import shiny ggplot2
#' @importFrom DT datatable DTOutput renderDT JS editData
#' @importFrom rlang parse_expr expr
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' guiplot(PK)
#' }
#' \dontrun{
#' guiplot()
#' }
guiplot <- function(...) {
  ########################################################
  #Obtaining and Specifying the Data and Parameters Needed
  colna<-c("none","x","y","group","ymin","ymax")

  get_data_arry<-function(...){
    # browser()
    if(...length()<1){
      lsname<-ls(envir =.GlobalEnv)
    }else{
      lsname<-as.character(eval(substitute(alist(...))))
    }

    j=1
    arry_data<-NULL
    for (i in 1:length(lsname)){
      ind_data <- get_data(lsname[i], name = lsname[i])
      if(is.matrix(ind_data$guiplot_data)||is.data.frame(ind_data$guiplot_data)){
        arry_data<-rbind(arry_data,ind_data)
      }
    }
    j<-NULL
    return(arry_data)
  }
  res_data<-get_data_arry(...)
  # res_data <- get_data(data, name = deparse(substitute(data)))
  #Obtaining and Specifying the Data and Parameters Needed
  ########################################################


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

  ##############################
  #setup_tabPanel_panel
  output$ui<-renderUI({
    id = "guiplot"
    ns <- NS(id)
    eval(text_panels())
  })

  text_panels<-reactive({
    # browser()
    a<-NULL
    b<-NULL
    for (j in 1:nrow(res_data)){
      Parname<-paste(sep="","data_panae_",j)
      a[j]<-paste(sep="" ,"setup_tabPanel_panel(","'",Parname,"'",")")
    }
    a<-paste(a,collapse =",")
    b<-paste(sep="" ,"navlistPanel(widths = c(3, 9),",a,")")
    parse_expr(b)
  })

  mptable<-reactive({
    mp_table<-list()
    for (j in 1:nrow(res_data)){
      Parname<-paste(sep="","data_panae_",j)
      mp_item<-NULL

      mp_item<- callModule(
        module = guiplot_dt_Server,
        id = Parname,
        data = res_data[j,],
        colname=colna
      )
        mp_table[j]<-mp_item
    }
    mp_table
  })
  #
  ##############################

  callModule(
    module = guiplot_plot_Server,
    id = "guiplot",
    data = mptable(),
    dataname=res_data[,2]
  )

  callModule(
    module =   guiplot_layout_updata_server,
    id = "guiplot"
  )

}

  runGadget(
    #browser(),
    guiplotUI, guiplotServer, viewer = browserViewer()
  )
}
