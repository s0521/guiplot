#Create a user-friendly plotting GUI for R.
#plot engine is ggplot2
#'
#' @title guiplot
#' @param ... Matrix or data frame
#' @param out_dir The storage path of the output picture, recommend 'out_dir=getwb()'
#' @export
#' @return png and pdf of plot
#' @import shiny ggplot2 svglite
#' @importFrom DT datatable DTOutput renderDT JS editData formatStyle
#' @importFrom rlang parse_expr expr
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @importFrom htmltools withTags
#' @importFrom methods new
#' @examples
#' \donttest{
#' guiplot()
#' }
#' \donttest{
#' guiplot(PK)
#' }
#' \donttest{
#' guiplot(PK,out_dir=getwb())
#' }
guiplot <- function(..., out_dir = NULL) {
  ########################################################
  #Static data

  c1name <- c("none","x","y","ymin","ymax","column","row","group","color","linetype","mark")
  c2group <- c(rep("1",5),rep("3",2),rep("4",4))
  c3display <-c(rep("Plot Data",5),rep("Lattice By",2),rep("Group By",4))
  c_name <- matrix (nrow=3,ncol=length(c1name),byrow = T )
  c_name[1,] <- c1name
  c_name[2,] <- c2group
  c_name[3,] <- c3display

  field_groups<-c_name

  #########################################################
  #Obtaining the Data and Parameters
  get_data_arry<-function(...){
    # browser()
    if(...length()<1){
      lsname<-ls(envir =.GlobalEnv)
    }else{
      lsname<-as.character(eval(substitute(alist(...))))
    }

    arry_data<-NULL
    # browser()
    for (i in 1:length(lsname)){
      ind_data <- get_data(lsname[i], name = lsname[i])
      if(is.matrix(ind_data$guiplot_data)||is.data.frame(ind_data$guiplot_data)){
        arry_data<-rbind(arry_data,ind_data)
      }
    }
    return(arry_data)
  }

  res_data<-get_data_arry(...)

  if(is.null(out_dir)) out_dir<-tempdir()
  # res_data <- get_data(data, name = deparse(substitute(data)))
  #Obtaining and Specifying the Data and Parameters Needed
  ########################################################


  guiplotServer = function(input, output, session) {
    # Panl_Height<-reactive({input$Panle_Height})
    # Panl_Width<-reactive({input$Panle_Width})
    # browser()
    callModule(
      module = guiplot_tital_Server,
      id = "guiplot"
    )

    callModule(
      module = guiplot_result_Server,
      id = "guiplot",
      out_dir=out_dir
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
          data_and_name = res_data[j,],
          field_groups = field_groups
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
      module = guiplot_layout_updata_server,
      id = "guiplot"
    )

  }

  runGadget(
    #browser(),
    guiplotUI, guiplotServer, viewer = browserViewer()
  )
}
