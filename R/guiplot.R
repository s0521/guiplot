#Create a user-friendly plotting GUI for R.
#plot engine is ggplot2
#'
#' @title guiplot
#' @param ... Matrix or data frame
#' @param out_dir The storage path of the output picture, recommend 'out_dir=getwd()'
#' @export
#' @return Export files(png and pdf of plot) to a temporary directory, or user-defined folders.
#' @import shiny ggplot2 svglite R6
#' @importFrom excelR excelOutput renderExcel excelTable excel_to_R
#' @importFrom DT datatable DTOutput renderDT JS editData formatStyle
#' @importFrom rlang parse_expr parse_exprs expr is_empty
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @examples
#' if (interactive()) {
#' # Launch with built-in PK data set:
#' guiplot(PK)
#' }
#'
#' \dontrun{
#' # Launch with memory-in data set:
#' guiplot()
#'
#' # Launch with memory-in data set, and output plot to user-defined folders:
#' guiplot(PK,out_dir= Any_directory_you_want_to_export)
#'
#' }
#'
guiplot <- function(..., out_dir = getwd()) {
  #Static data########################################################

  c1name <- c("hide","none", "x", "y", "ymin", "ymax", "column", "row", "group", "color", "fill", "linetype", "mark")
  c2display <- c("",rep("Plot Data", 5), rep("Lattice By", 2), rep("Group By", 5))
  c3rowset <- c("0",rep("1", 5), rep("3", 2), rep("4", 5))
  c4colset <- c("0","0", "1", rep("0", 3), rep("1", 2), rep("0", 5))
  c5intset <- c("0","1", "0", rep("0", 10))
  c6colhide <- c("0","1", rep("1", 11))

  c_name <- matrix(nrow = 6, ncol = length(c1name), byrow = T)
  c_name[1, ] <- c1name
  c_name[2, ] <- c2display
  c_name[3, ] <- c3rowset
  c_name[4, ] <- c4colset
  c_name[5, ] <- c5intset
  c_name[6, ] <- c6colhide

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
      id = "guiplot",
      Moudel_plot_codes = Moudel_plot_codes
    )

    callModule(
      module = guiplot_result_Server,
      id = "guiplot",
      out_dir=out_dir,
      Moudel_plot_codes = Moudel_plot_codes,
      parentSession=session
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
      #用于动态的显示设置标签页中的所有数据，作为一个标签页
      a<-NULL
      b<-NULL
      for (j in 1:nrow(res_data)){
        Parname<-paste(sep="","data_panae_",j)
        a[j]<-paste(sep="" ,"setup_tabPanel_panel(","'",Parname,"'",")")
      }
      a<-paste(a,collapse =",")
      b<-paste(sep="" ,"navlistPanel(widths = c(2, 10),",a,")")
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
        # col_class_item<-NULL
        # col_class_item<- callModule(
        #   module = guiplot_Rexcle_Server,
        #   id = Parname,
        #   data_and_name = res_data[j,],
        #   field_groups = field_groups
        # )

          mp_table[j]<-mp_item
      }
      mp_table
    })
    colClass_table<-reactive({
      #基于mp table函数修改得到，用于传递给列类型处理服务相应的需要的参数
      col_class_table<-list()
    # browser()
      for (j in 1:nrow(res_data)){
        Parname<-paste(sep="","data_panae_",j)
        col_class_item<-NULL
    # browser()
        col_class_item<- callModule(
          module = guiplot_Rexcle_Server,
          id = Parname,
          data_and_name = res_data[j,],
          field_groups = field_groups
        )
          col_class_table[j]<-col_class_item
      }
      col_class_table
    })
    #
    ##############################
    geom_Additional_UGC_codes_Table <- callModule(
      module = guiplot_geom_Additional_UGC_dt_Server,
      id = "guiplot"
    )

    Moudel_plot_codes <- callModule(
      module = guiplot_plot_Server,
      id = "guiplot",
      data = mptable(),
      dataname=res_data[,2],
      data_col_Class_as=colClass_table(),
      geom_Additional_UGC_codes_Table = geom_Additional_UGC_codes_Table
    )

    callModule(
      module = guiplot_layout_updata_server,
      id = "guiplot"
    )

  }
  # suppressMessages({
  #   suppressWarnings({
      runGadget(
        #browser(),
        guiplotUI, guiplotServer, viewer = browserViewer()
      )
  #   })
  # })
}
