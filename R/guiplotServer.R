guiplot_tital_Server<- function(input, output, session) {
  observeEvent(input$ColseButton, {
    stopApp()
  })
  onStop(function() {
    # stopApp()
    cat("Session stopped\n")
    })
}

guiplot_result_Server <- function(input, output, session) {
  # pixelratio<- reactive({session$clientData$pixelratio})
  # web_plot_width <- reactive({input$web_plot_width})
  # web_plot_height <- reactive({input$web_plot_height})
  # web_plot_scale <- reactive({input$web_plot_scale})
  # output_plot_width <- reactive({input$output_plot_width})
  # output_plot_height <- reactive({input$output_plot_height})
  # output_plot_dpi <- reactive({input$output_plot_dpi})
  units <- reactive({"cm"})

  observeEvent(input$ExecuteButton, {
    ggsave("ggplot.svg",
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
    )
    ggsave("ggplot.pdf",
           width = input$output_plot_width,
           height =input$output_plot_height,
           units =units(),
           scale = input$web_plot_scale
           )
    ggsave("ggplot.png",
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
      # type<-c("point","line")
      type<-get_geomtype_codes()
      code<-geomCode(type,dataname,x,y,ymin,ymax,group)
      if (!is.null(code))
        data_code[i]<-code
      cat(file=stderr(), "\n data_code is ",data_code)
    }
    data_code<-na.omit(data_code)
    data_codes<-paste(collapse ="+",data_code)
    data_codes
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

    gg2<-c("ggplot() ",gg_geom_codes, gg_coord_code, gg_themes_codes)
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
    ggsave("ggplot.svg",
           width = input$output_plot_width,
           height =input$output_plot_height,
           units ="cm",
           scale = input$web_plot_scale
    )
   list(
     src = "ggplot.svg",
     width = input$web_plot_width*session$clientData$pixelratio,
     height =input$web_plot_height*session$clientData$pixelratio,
     alt = "This is preview plot"
   )
  })
}


guiplot_dt_Server <- function(input, output, session, data1 =NULL,colname=NULL) {
	#server = FALSE
  # browser()
  colna<-colname
	data<-NULL
	data<-as.data.frame(data1[[1]])
	dataname<-c(data1[[2]])
	#
	dat<-Tint(mpm(data,colna),1)

	#container
	sketch <- htmltools::withTags(table(
	  class = 'display',
	  thead(
	    tr(
	      th(rowspan = 2, ''),
	      th(colspan = 5, class='sorting_disabled dt-center',style='border-right: dashed;','Plot Data'),
	      th(colspan = 2, class='sorting_disabled dt-center',style='border-right: solid;', 'Lattice By'),
	      th(colspan = 4, class='sorting_disabled dt-center', 'Group By')
	    ),
	    tr(
	      lapply(colna, th)
	    )
	  )
	))
	#################################
	########render output############
	#################################
	#table name first data
	output$tab1 <-renderText(dataname)
	# browser()
	#DataTable
	output$dt = renderDT({

	  # browser()
	  datatable(
	    dat,
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
    		  list(width = '20px', targets = 1:ncol(dat)),
    		  list(className = 'dt-center success', targets = 1:ncol(dat))
    		  # list(className = 'success', targets = 1:4)
    		  ),
    		dom = 't',paging = FALSE, ordering = FALSE
  		)
	  )%>% formatStyle(5, 'border-right' = 'dashed')%>% formatStyle(7, 'border-right' = 'solid')
	# ,c(6,8), 'border-left'=c('dashed','solid'))
	})

	#################################
	#################reactive########
	#################################


	Data_fill <- reactive({
	  # browser()
		info <- input[["dt_cells_filled"]]
		if(!is.null(info)){
		  info <- unique(info)
		  info$value[info$value==""] <- NA
		  if (info[1,2]>7){
		    dat <<- editData(dat, info, proxy = "dt")
		  }else{
		    info<-as.data.frame(Binfo(Tautofill(dat[,1:8],info,1)))
		    dat <<- editData(dat, info, proxy = "dt")
		  }
		}
		dat
	})

	Data_select <- reactive({
	  # browser()
	  a<-dat
	  info <- input[["dt_cells_selected"]]
	  # cat(info)
	  if(is.null(info)||ncol(info)<2){
	    # return()
	  }else{
	    if (info[1,2]>7){
	      inf<-cbind(info,dat[info[1],info[2]])
	      info<-as.data.frame(inf,byrow = TRUE)
	      colnames(info)<-c("row","col","value")
	      val1<-info[1,3]
	      if (val1==1){info[1,3]<-0}else{info[1,3]<-1}
	      info <- unique(info)
	      info$value[info$value==""] <- NA
	      dat <<- editData(dat, info, proxy = "dt")
	    }else{
  	    inf<-cbind(info,dat[info[1],info[2]])
  	    info<-as.data.frame(inf,byrow = TRUE)
  	    colnames(info)<-c("row","col","value")
  	    val1<-info[1,3]
  	    if (val1==1){info[1,3]<-0}else{info[1,3]<-1}
  	    info <- unique(info)
  	    info$value[info$value==""] <- NA
  	    info<-as.data.frame(Binfo(Tautofill(dat[,1:8],info,1)))
  	    dat <<- editData(dat, info, proxy = "dt")
	    }
	  }
	  a<-dat
	  dat
	})
	return(list(mptable=reactive({
	                # browser()
	                a<-NULL
	                a<-Data_fill()
	                a<- input[["dt_cells_filled"]]
	                a<-Data_select()
	                a<-input[["dt_cells_selected"]]
	                a<-dat
	                return(a)
	              })))
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
}
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
  "});"
)

valueBloon<-function(infoo){
  if(!is.null(infoo)){
    val<-infoo[1,3]
    if (val==1){infoo[1,3]<-0}else{infoo[1,3]<-1}
    infoo
  }
}


mpm<-function(data,coln){
  nr<-ncol(data)
  nc<-length(coln)
  m<-matrix(as.numeric(rep(0,nc*nr)),nrow=nr,ncol=nc, byrow = TRUE)

  colnames(m)<-coln
  rownames(m)<-colnames(data)

  m
}

Tselect<-function(data,infoo){
  srfist<-infoo[1,1]
  srend<-infoo[nrow(infoo),1]
  Ts<-data[srfist:srend,,drop = FALSE]

  Ts
}

Tautofill<-function(data,infoo,dr){

  i<-c(infoo[1,2])
  srfist<-infoo[1,1]
  srend<-infoo[nrow(infoo),1]

  #ST information
    ST<-as.matrix(`Tselect`(data,infoo), byrow = TRUE)
  nr<-nrow(ST)
  nc<-ncol(ST)
  #browser()
  if (infoo[1,3]==0){
    ST[,i]<-rep(0,nrow(ST))
    ST<-Tint(ST,dr)
  }
  else
  {
    ST[]<-matrix(rep(0,nr*nc),nrow=nr,ncol=nc)
    ST[,i]<-rep(1,nr)
  }
  data[srfist:srend,]<-ST
  out<-(data)
  out
}

Tint<-function(data,dr){
  nr<-nrow(data)
  nc<-ncol(data)
  #browser()
  for (i in seq_len(nr)){
    if(sum(data[i,])!=1){
      data[i,]<-rep(0,nc)
      data[i,dr]<- 1
    }
  }
  data
}

Binfo<-function(data){
  nr<-nrow(data)
  nc<-ncol(data)
  m<-matrix(as.numeric(rep(0,nr*nc*3)),nrow=nr*nc,ncol=3, byrow = TRUE)

  colnames(m)<-c("row","col","value")
  k<-1
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      m[k,1] = i
      m[k,2] = j
      m[k,3] = data[i,j]
      k<-k+1
    }
  }
  m
}

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
