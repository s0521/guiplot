guiplot_tital_Server<- function(input, output, session) {
  observeEvent(input$ColseButton, {
    stopApp()
  })
  onStop(function() {
    stopApp()
    cat("Session stopped\n")
    })
}

guiplot_result_Server <- function(input, output, session, out_dir =NULL) {
  # pixelratio<- reactive({session$clientData$pixelratio})
  # web_plot_width <- reactive({input$web_plot_width})
  # web_plot_height <- reactive({input$web_plot_height})
  # web_plot_scale <- reactive({input$web_plot_scale})
  # output_plot_width <- reactive({input$output_plot_width})
  # output_plot_height <- reactive({input$output_plot_height})
  # output_plot_dpi <- reactive({input$output_plot_dpi})
  units <- reactive({"cm"})
  # out_dir<-tempdir()

  observeEvent(input$ExecuteButton, {
    ggsave("ggplot.svg",
           path=out_dir,
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
    )
    ggsave("ggplot.pdf",
           path=out_dir,
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
           )
    ggsave("ggplot.png",
           path=out_dir,
           dpi=input$output_plot_dpi,
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
           )
  })
}

guiplot_plot_Server <- function(input, output, session, data =NULL,datanames=NULL) {
  # browser()

  #get geom type Codes
  get_geomtype_codes<- reactive({
    c(
      input$geom_type_1variable,
      input$geom_type_2variable,
      input$geom_type_other
    )
  })

  #get geom Codes
  get_geom_codes<- reactive({
    # browser()
    ls1<-data
    geom_data_names<-c(datanames)
    n_data<-length(geom_data_names)
    data_code<-NULL
    for (i in 1:n_data){
      mptable<-data[[i]]
      dataname<-c(geom_data_names[[i]])

      if((is.null(mptable) )){
        return()
      }
      x<-GetMappingValue(mptable(),2)
      y<-GetMappingValue(mptable(),3)

      ymin<-GetMappingValue(mptable(),4)
      ymax<-GetMappingValue(mptable(),5)
      group<-GetMappingValue(mptable(),8)
      color<-GetMappingValue(mptable(),9)
      linetype<-GetMappingValue(mptable(),10)
      mark<-GetMappingValue(mptable(),11)
      # type<-c("point","line")
      type<-get_geomtype_codes()
      code<-geomCode(
        type=type,
        data=dataname,
        x=x,
        y=y,
        ymin=ymin,
        ymax=ymax,
        group=group,
        color=color,
        linetype=linetype,
        shape=mark
        )
      if (!is.null(code))
        data_code[i]<-code
      # cat(file=stderr(), "\n data_code is ",data_code)
    }
    data_code<-na.omit(data_code)
    data_codes<-paste(collapse ="+",data_code)
    data_codes
  })

  #get facets codes
  get_facets_codes<-reactive({
    # browser()
    ls1<-data
    geom_data_names<-c(datanames)
    n_data<-length(geom_data_names)
    facets_codes<-NULL
    for (i in 1:n_data){
      mptable<-data[[i]]
      dataname<-c(geom_data_names[[i]])

      if((is.null(mptable) )){
        return()
      }
      cols<-GetMappingValue(mptable(),6)
      rows<-GetMappingValue(mptable(),7)
      code<-facets_code(
        cols=cols,
        rows=rows
      )
      if (!is.null(code))
        facets_codes[i]<-code
      # cat(file=stderr(), "\n facets_code is ",facets_code)
      facets_codes<-na.omit(facets_codes)
      # browser()
      return(facets_codes)
    }
  })

  #get coord codes
  get_coord_trans_codes <- reactive({
    axis_x<-list(
      Scale=input$X_Scale,
      Range=input$X_Range,
      Minimum=input$X_Minimum,
      Maximum=input$X_Maximum,
      expand_p=input$X_expand_p,
      expand_u=input$X_expand_u
    )
    axis_y<-list(
      Scale=input$Y_Scale,
      Range=input$Y_Range,
      Minimum=input$Y_Minimum,
      Maximum=input$Y_Maximum,
      expand_p=input$Y_expand_p,
      expand_u=input$Y_expand_u
    )
    a<-coord_trans_code(axis_x,axis_y)
    # browser()
    if(nchar(a)<17)
      return()
    # return(coord_trans_code(axis_x,axis_y))
    return(a)
  })


  #get themes codes
  get_plot_themes_codes <- reactive({
    # browser()
    p_plot_thems<-list(
      plot_themes=input$themes
    )
    a<-plot_themes_code(p_plot_thems)
    # browser()
    if(nchar(a)<4)
      return()
    # return(coord_trans_code(axis_x,axis_y))
    return(a)
  })

  get_plot_codes <- reactive({
    # browser()
    gg_geom_codes<-get_geom_codes()
    cat(file=stderr(), "\n gg_geom_codes is ",gg_geom_codes)
    # browser()

    gg_coord_code<-get_coord_trans_codes()
    cat(file=stderr(), "\n gg_coord_code is ",gg_coord_code)

    gg_themes_codes<-get_plot_themes_codes()
    cat(file=stderr(), "\n gg_themes_codes is ",gg_themes_codes)

    gg_facets_codes<-get_facets_codes()
    cat(file=stderr(), "\n gg_facets_codes is ",gg_facets_codes)

    gg2<-c("ggplot() ",gg_geom_codes, gg_coord_code, gg_themes_codes,gg_facets_codes)
    gg2<-paste(sep="+",collapse ="+",gg2)
    cat(file=stderr(), "\n gg2 is ",gg2)
    # req(gg_geom_codes)
    if (is.null(gg_geom_codes)||gg_geom_codes==""){
        # return(ggplot())
      return("ggplot()")
    }else{
      # eval(parse_expr(as.character(gg2)))
      gg2
    }
  })

  output$plot <- renderImage({
    # pixelratio<- reactive({session$clientData$pixelratio})
    # web_plot_width <- reactive({input$web_plot_width})
    # web_plot_height <- reactive({input$web_plot_height})
    # web_plot_scale <- reactive({input$web_plot_scale})
    # output_plot_width <- reactive({input$output_plot_width})
    # output_plot_height <- reactive({input$output_plot_height})
    # output_plot_dpi <- reactive({input$output_plot_dpi})

    eval(parse_expr(as.character(get_plot_codes())))
    outfile <- tempfile(fileext='.png')
    ggsave(outfile,#"ggplot.svg",
           # path=out_dir<-tempdir(),
           width = input$output_plot_width,
           height =input$output_plot_height,
           units ="cm",
           scale = input$web_plot_scale
    )
   list(
     src = outfile,#"ggplot.svg",
     width = input$web_plot_width*session$clientData$pixelratio,
     height =input$web_plot_height*session$clientData$pixelratio,
     alt = "This is preview plot"
   )
  })
}


guiplot_dt_Server <- function(input, output, session, data_and_name =NULL, field_groups=NULL) {
  #server = FALSE

  #################################
  #################################
  #panel的名字dataname
  dataname<-c(data_and_name[[2]])
  output$tab1 <-renderText(dataname)

  #################################
  #################################
  #################################
  #使用的外部数据集
  # data<-as.data.frame(data1[[1]])
  r_name<-c(colnames(data_and_name[[1]]))

  #################################
  #################################
  #################################
  #实例化一个Mapping_Table对象
  obj_mptbl<-Mapping_Table_class$new(field_groups=field_groups,variable=r_name,default_field=1)
  # browser()
  obj_mptbl$create_mptbl()

  env_guiplot<- new.env(parent = emptyenv())
  env_guiplot$dat<-obj_mptbl$mapping_table
  sketch <-obj_mptbl$DT_container
  field_right_bound<-obj_mptbl$field_right_bound

  #################################
  #################DataTable#######
  #################################
  output$dt = renderDT({
    datatable(
      env_guiplot$dat,
      rownames = TRUE,width=100 ,
      # editable = list(target = "cell"),
      selection = list(mode = 'single', target = 'cell'),
      callback = JS(callback),
      extensions = c('AutoFill'),
      container =sketch,
      class = 'table-hover',
      options = list(
        autoFill = list(horizontal=FALSE,vertical=TRUE,alwaysAsk=FALSE),
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '20px', targets = 1:ncol(env_guiplot$dat)),
          list(className = 'dt-center success', targets = 1:ncol(env_guiplot$dat))
        ),
        dom = 't',paging = FALSE, ordering = FALSE
      )
    )%>% formatStyle(field_right_bound, 'border-right' = 'solid')
  })

  #################################
  #################reactive########
  #################################
  # observe({
  #   Data_fill()
  #   Data_select()
  # })

  Data_fill <- reactive({
    # browser()
    info <- input[["dt_cells_filled"]]
	#print(c("\n info is ",info))
    if(!is.null(info)){
      info <- unique(info)
      info$value[info$value==""] <- NA

      obj_mptbl$mptbl_event_fill(info)
      env_guiplot$dat <- editData(env_guiplot$dat, as.data.frame(obj_mptbl$inf_of_mptbl), proxy = "dt")
    }
    env_guiplot$dat
  })

  Data_select <- reactive({
    # browser()
    a<-env_guiplot$dat
    info <- input[["dt_cells_selected"]]
    if(is.null(info)||ncol(info)<2){
      # return()
    }else{
      info<-cbind(info,env_guiplot$dat[info[1],info[2]])

      obj_mptbl$mptbl_event_select(info)
      env_guiplot$dat <- editData(env_guiplot$dat, as.data.frame(obj_mptbl$inf_of_mptbl), proxy = "dt")
    }
    env_guiplot$dat
  })

  #################################
  #################################
  #################################
  #return
  return(list(mptable=reactive({
    Data_fill()
    Data_select()
    a<-env_guiplot$dat
    return(a)
  })))

  # browser()
  #end
  #################################
}

guiplot_layout_updata_server<-function(input, output, session){
  x<-reactive({input$web_plot_height/input$web_plot_width})
  observeEvent(input$web_plot_height,
               {
                 new<-isolate(input$output_plot_width)
                 value <- round(x()*new,2)
                 updateNumericInput(session,
                                    "output_plot_height",
                                    'output plot width(cm)',
                                    value = value,
                                    max<- value,
                                    min<- value
                 )

               })

  observeEvent(input$web_plot_width,
               {
                 new<-isolate(input$output_plot_width)
                 value <- round(x()*new,2)
                 updateNumericInput(session,
                                    "output_plot_height",
                                    'output plot width(cm)',
                                    value = value,
                                    max<- value,
                                    min<- value
                 )
               })

  # observeEvent(input$output_plot_height,
  #              {
  #                new<-isolate(input$output_plot_height)
  #                updateNumericInput(session,
  #                                   "output_plot_width",
  #                                   value = round(new/x(),2)
  #                )
  #              })

  observeEvent(input$output_plot_width,
               {
                 new<-isolate(input$output_plot_width)
                 value <- round(x()*new,2)
                 updateNumericInput(session,
                                    "output_plot_height",
                                    'output plot width(cm)',
                                    value = value,
                                    max<- value,
                                    min<- value
                 )
               })
	#################################
  #################vline#######
  #################################
	linshi_vline_dt_table<-data.frame(Axis=c("X"),Value=c(1),Line_Weight=c(1),Title=c(1))
  output$vline = renderDT({
		linshi_vline_dt_table
  })
  
  
  
}

#################################
#################################
#################################
#GetMappingValue for guiplot_plot_Server
GetMappingValue<-function(data,column){
  # browser()
  nr<-nrow(data)
  if (is.null(nr))
    return()
  var1<-c()
  for (i in seq_len(nr)) {
    if (data[i,column]==1) {
      var1<-c(var1,rownames(data)[i])
    }
  }
  var1
}

#################################
#################################
#################################
#callback for guiplot_dt_Server
callback <- c(
  "var tbl = $(table.table().node());",
  "var id = tbl.closest('.datatables').attr('id');",
  "table.on('preAutoFill', function(e, datatable, cells){",
  "  var out = [];",
  "  for(var i = 0; i < cells.length; ++i){",
  "    var cells_i = cells[i];",
  "    for(var j = 0; j < cells_i.length; ++j){",
  "      var c = cells_i[j];",
  "      var value = c.set === null ? '' : c.set;",
  "      out.push({",
  "        row: c.index.row + 1,",
  "        col: c.index.column ,",
  "        value: value",
  "      });",
  "    }",
  "  }",
  "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
  "  table.rows().invalidate();", # this updates the column type
  "});",
  "delete $.fn.dataTable.AutoFill.actions.increment;"
)
