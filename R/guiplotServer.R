guiplot_tital_Server<- function(input, output, session, Moudel_plot_codes) {
  observeEvent(input$ColseButton, {
    a<-parse_exprs(Moudel_plot_codes$plot_code_expr())
    stopApp(a)
    #cat("Session stopped ,Because observeEvent \n")
  })
  onStop(function() {
    #cat("Session stopped, Because onStop \n")
    })
}

guiplot_result_Server <- function(input, output, session, out_dir =NULL, Moudel_plot_codes,parentSession) {
  pixelratio<- reactive({session$clientData$pixelratio})
  web_plot_width <- reactive({input$web_plot_width})
  web_plot_height <- reactive({input$web_plot_height})
  web_plot_scale <- reactive({input$web_plot_scale})
  output_plot_width <- reactive({input$output_plot_width})
  output_plot_height <- reactive({input$output_plot_height})
  output_plot_dpi <- reactive({input$output_plot_dpi})
  units <- reactive({"cm"})
  textOfCode_guipot_codes = reactive(gsub(";",";\n",gsub("\\+","\\+\n", Moudel_plot_codes$plot_code_expr())))
  textOfCode_Phoenix_codes = reactive(gsub(";","\n",gsub("\\+","\\+\n", Moudel_plot_codes$UsedColNames_texts())))

  doSavePlot = reactive({
    aa <- textOfCode_guipot_codes()
    parameterList <- list(path=out_dir,
                          width = input$output_plot_width,
                          height =input$output_plot_height,
                          units =units(),
                          scale = input $web_plot_scale)
    do.call(ggsave, c("guiplot.svg", parameterList))
    do.call(ggsave, c("guiplot.pdf", parameterList))
    do.call(ggsave, c("guiplot.png", parameterList))
    do.call(ggsave, c("guiplot2.png", parameterList))
  })
  # out_dir<-tempdir()
  # out_dir<-getwd()

  observeEvent(input$ExecuteButton, {
    updateTabsetPanel(session = parentSession, inputId="ChildTabset",
      selected = "Results Panel"
    )
    #保存图片为图片文件
    doSavePlot()
    #将代码保存为文本文件
    cat(textOfCode_guipot_codes(),file = "guiplot.r")

    textOfCode_Phoenix_codes_shell <- paste0(
      "#Load dependent packages,\u52a0\u8f7d\u4f9d\u8d56\u7684\u6dfb\u52a0\u5305","\n",
      "require(guiplot)","\n",
      "require(ggplot2)","\n",
      "\n",
      "#The following statement describes the required dataset and the columns it contains,","\n",
      "#which are required by Phoenix software and can be deleted if non-Phoenix software users.","\n",
      "#This is generated based on the mapping state in the mapping table","\n",
      "#\u4e0b\u8ff0\u8bed\u53e5\u8bf4\u660e\u4e86\u6240\u5fc5\u987b\u7684\u6570\u636e\u96c6\u4ee5\u53ca\u6570\u636e\u96c6\u5305\u542b\u7684\u5217,","\n",
      "#\u8fd9\u662fPhoenix\u8f6f\u4ef6\u6240\u9700\u8981\u7684,\u5982\u679c\u975ePhoenix\u8f6f\u4ef6\u7528\u6237\u53ef\u5220\u9664\u6389\u3002","\n",
      "#\u8fd9\u662f\u57fa\u4e8e\u6620\u5c04\u8868\u683c\u4e2d\u6620\u5c04\u60c5\u51b5\u751f\u6210\u7684","\n",
      "\n",
      textOfCode_Phoenix_codes(),"\n",
      "\n",
      "#The drawing code of plot,\u56fe\u8868\u7ed8\u5236\u7684\u4ee3\u7801","\n",
      textOfCode_guipot_codes()
      )
    cat(textOfCode_Phoenix_codes_shell,file = "Phoenix_codes.r")

  })

  output$Results_Plot1 <- renderImage({
    doSavePlot()
    list(
      src = paste(out_dir,"/guiplot2.png",sep=""),
      width = input$web_plot_width*session$clientData$pixelratio,
      height =input$web_plot_height*session$clientData$pixelratio,
      alt = "This is preview plot"
    )
  },deleteFile=TRUE)

  output$Results_Text1_guipot_codes <- renderText({
    textOfCode_guipot_codes()
  })
  output$Results_Text1_Phoenix_codes <- renderText({
    paste0(
      "#Load dependent packages,\u52a0\u8f7d\u4f9d\u8d56\u7684\u6dfb\u52a0\u5305","\n",
      "require(guiplot)","\n",
      "require(ggplot2)","\n",
      "\n",
      "#The following statement describes the required dataset and the columns it contains,","\n",
      "#which are required by Phoenix software and can be deleted if non-Phoenix software users.","\n",
      "#This is generated based on the mapping state in the mapping table","\n",
      "#\u4e0b\u8ff0\u8bed\u53e5\u8bf4\u660e\u4e86\u6240\u5fc5\u987b\u7684\u6570\u636e\u96c6\u4ee5\u53ca\u6570\u636e\u96c6\u5305\u542b\u7684\u5217,","\n",
      "#\u8fd9\u662fPhoenix\u8f6f\u4ef6\u6240\u9700\u8981\u7684,\u5982\u679c\u975ePhoenix\u8f6f\u4ef6\u7528\u6237\u53ef\u5220\u9664\u6389\u3002","\n",
      "#\u8fd9\u662f\u57fa\u4e8e\u6620\u5c04\u8868\u683c\u4e2d\u6620\u5c04\u60c5\u51b5\u751f\u6210\u7684","\n",
      "\n",
      textOfCode_Phoenix_codes(),"\n",
      "\n",
      "#The drawing code of plot,\u56fe\u8868\u7ed8\u5236\u7684\u4ee3\u7801","\n",
      textOfCode_guipot_codes()
    )
  })
}

guiplot_plot_Server <- function(input, output, session, data =NULL,datanames=NULL,data_col_Class_as=NULL,geom_Additional_UGC_codes_Table=NULL) {
  #browser()
  #get data_col_Class_as TextCode获取数据列类型转换的代码文本
  get_data_col_Class_as<- reactive({
    # browser()
    ls1<-data_col_Class_as
    geom_data_names<-c(datanames)#datanames数据集名称集合
    n_data<-length(geom_data_names)#n_data数据集数量
    Class_as_code<-NULL

    for (i in 1:n_data){
      try({
        mptable<-data_col_Class_as[[i]]
        Class_as_code[i]<-mptable()
      },silent = TRUE)
    }
    Class_as_text<-unlist(Class_as_code)

    Class_as_text<-Class_as_text[!is.null(Class_as_text)]
    # browser()
    if (is.null(Class_as_text)) {
       return()
    }

    ClassAs_text<-paste(Class_as_text,seq =";")
    # browser()
    return(ClassAs_text)
  })

  #get geom type Codes
  get_geomtype_codes<- reactive({
    c(
      input$geom_type_1variable,
      input$geom_type_2variable,
      input$geom_type_other
    )
  })

  #get geom_User Costomer Codes
  # browser()
  # bb <- geom_Additional_UGC_codes_Table
  # type_UGC <- bb
  # ls02 <- geom_Additional_UGC_codes_Table$use_geomtype
  # type_UGC <- geom_Additional_UGC_codes_Table$[ls02]
  # type_Aes_UGC <- geom_Additional_UGC_codes_Table$[ls02]

  #get geom Codes
  get_geom_codes<- reactive({
    # browser()
    ls1<-data
    geom_data_names<-c(datanames)
    n_data<-length(geom_data_names)
    data_code<-NULL#绘图代码集合，一维列表
    UsedColNames_text<-NULL#数据集中使用列的名称代码集合，一维列表
    for (i in 1:n_data){#循环所有数据集
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
      fill<-GetMappingValue(mptable(),10)
      linetype<-GetMappingValue(mptable(),11)
      mark<-GetMappingValue(mptable(),12)
      # type<-c("point","line")
      type<-get_geomtype_codes()
      ##geom User Customer Code
      type_UGC <- geom_Additional_UGC_codes_Table()

      code<-geomCode(
        type=type,
        data=dataname,
        x=x,
        y=y,
        ymin=ymin,
        ymax=ymax,
        group=group,
        color=color,
        fill=fill,
        linetype=linetype,
        shape=mark,
        type_UGC=type_UGC
        )
      if (!is.null(code)) data_code[i]<-code
      # #cat(file=stderr(), "\n data_code is ",data_code)

      ###获取使用的数据集中使用的列，并将其与数据名字何在一起转化为文本
      UsedDataColumnsNames <- GetUsedDataColumnsNames(mptable())

      if (!is.null(UsedDataColumnsNames)) {
        UsedDataColumnsNames_text<-paste0("attach(",dataname,") #WNL_IN ",paste(UsedDataColumnsNames,collapse=" "),"#")
        # browser()
        UsedColNames_text[i]<-UsedDataColumnsNames_text
      }
    }
    data_code<-na.omit(data_code)
    data_codes<-paste(collapse ="+",data_code)
    # data_codes

    UsedColNames_text<-na.omit(UsedColNames_text)
    UsedColNames_texts<-paste(collapse =";",UsedColNames_text)
    
    list(data_codes=data_codes,UsedColNames_texts=UsedColNames_texts)
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
      # #cat(file=stderr(), "\n facets_code is ",facets_code)
      facets_codes<-na.omit(facets_codes)
      # browser()
      return(facets_codes)
    }
  })

  #get coord codes
  get_coord_trans_codes <- reactive({

    coord_flip <- NULL
    if(input$coord_flip==TRUE) coord_flip <- "coord_flip()"
    axis_x<-list(
      Scale=input$X_Scale,
      Range=input$X_Range,
      Minimum=input$X_Minimum,
      Maximum=input$X_Maximum,
      expand_p=input$X_expand_p,
      expand_u=input$X_expand_u,
      Tick=input$X_Tick
    )
    axis_y<-list(
      Scale=input$Y_Scale,
      Range=input$Y_Range,
      Minimum=input$Y_Minimum,
      Maximum=input$Y_Maximum,
      expand_p=input$Y_expand_p,
      expand_u=input$Y_expand_u,
      Tick=input$Y_Tick
    )
    a<-coord_trans_code(axis_x,axis_y)
    if(nchar(a)<17){a=NULL }
    ###labs
    plot_labs_codes<-plot_labs_code(
      input$X_Axis_label,
      input$Y_Axis_label,
      input$title_label,
      input$subtitle_label,
      input$caption_label,
      input$tag_label,
      input$Legend_Tital_Color_Label,
      input$Legend_Tital_Shape_Label,
      input$Legend_Tital_Linetype_Label
    )
    ###

    coord_labs_codes<-c(a,plot_labs_codes,coord_flip)
    coord_labs_codes<-coord_labs_codes[!sapply(coord_labs_codes,function(a)any(is_empty(a),is.null(a),a==""))]
    coord_labs_codes<-paste0(collapse ="+",c(coord_labs_codes))
    # browser()
    if(any(is_empty(coord_labs_codes),is.null(coord_labs_codes),coord_labs_codes=="")){return()}
    # return(coord_trans_code(axis_x,axis_y))
    return(coord_labs_codes)
  })

  #get reference line codes
  get_reference_line_code <- reactive({
    # browser()
    #X(vline)
    X_Line_Code<-reference_line_code(
      type="v",
      intercept=input$x_intercept,
      color=input$x_color,
      size=input$x_size,
      add_UGC=input$x_add_UGC,
      slope="",
      Diagonal_Line=""
    )
    #Y(hline)
    Y_Line_Code<-reference_line_code(
      type="h",
      intercept=input$y_intercept,
      color=input$y_color,
      size=input$y_size,
      add_UGC=input$y_add_UGC,
      slope="",
      Diagonal_Line=""
    )
    #lin(abline)
    AB_Line_Code<-reference_line_code(
      type="ab",
      intercept=input$abline_intercept,
      color=input$abline_color,
      size=input$abline_size,
      add_UGC=input$abline_add_UGC,
      slope=input$abline_slope,
      Diagonal_Line=input$Diagonal_Line
    )
    Line_Code <-list(X_Line_Code,Y_Line_Code,AB_Line_Code)
    Line_Code <- Line_Code[!sapply(Line_Code,function(a)any(is_empty(a),is.null(a),a==""))]

    if(any(is_empty(Line_Code),is.null(Line_Code),Line_Code=="")){return()}
    # return(coord_trans_code(axis_x,axis_y))
    return(Line_Code)
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
  #get legend Position codes
  get_legend_position_code <- reactive({
    # browser()
    Code<-legend_position_code(
      Legend_Visible=input$Legend_Visible,
      Legend_Docking=input$Legend_Docking,
      Relative_Position_Select=input$Relative_Position_Select,
      Legend_X_Offset=input$Legend_X_Offset,
      Legend_Y_Offset=input$Legend_Y_Offset
    )
    a <- Code
    if(Code=="theme(legend.position = 'right')")
      return()
    # return(coord_trans_code(axis_x,axis_y))
    return(a)
  })

  #get User Generat codes
  get_UGC_codes <- reactive({
    # browser()
      UGC=input$UGC
      a<-UGC_code(UGC)
    # browser()
    if(a=="")
      return()
    # return(coord_trans_code(axis_x,axis_y))
    return(a)
  })

  get_plot_codes <- reactive({
    ##输出数据的列类型转换代码

    gg_data_col_Class_as<-get_data_col_Class_as()
    #cat(file=stderr(), "\n gg_data_col_Class_as is ",gg_data_col_Class_as)
    # browser()
    gg_geom_codes<-get_geom_codes()[1]
    #cat(file=stderr(), "\n gg_geom_codes is ",gg_geom_codes)
    # browser()

    gg_coord_code<-get_coord_trans_codes()
    #cat(file=stderr(), "\n gg_coord_code is ",gg_coord_code)

    gg_reference_line_code<-get_reference_line_code()

    gg_themes_codes<-get_plot_themes_codes()
    #cat(file=stderr(), "\n gg_themes_codes is ",gg_themes_codes)

    gg_facets_codes<-get_facets_codes()
    #cat(file=stderr(), "\n gg_facets_codes is ",gg_facets_codes)

    gg_legend_position_code <- get_legend_position_code()

    gg_UGC_codes<-get_UGC_codes()
    #cat(file=stderr(), "\n gg_UGC_codes is ",gg_UGC_codes)

    gg2<-c("ggplot() ",gg_geom_codes, gg_coord_code, gg_themes_codes,gg_facets_codes,gg_legend_position_code,gg_reference_line_code,gg_UGC_codes)
    gg2<-gg2[!sapply(gg2,function(a)any(is.null(a),a==""))]
    gg2<-paste(sep="+",collapse ="+",c(gg2))
    gg2<-paste(c(gg_data_col_Class_as,gg2),sep="",collapse ="")

    #cat(file=stderr(), "\n gg2 is ",gg2)
    message("\n guiplot() generate code is ",gg2)
    # req(gg_geom_codes)
    if (all(is.null(gg_geom_codes)||gg_geom_codes=="",is.null(gg_UGC_codes)||gg_UGC_codes=="")){
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

    # eval(parse_exprs(as.character(get_plot_codes())))
    local({
      aa<-parse_exprs(as.character(get_plot_codes()))
      for (i in seq_along(aa)){
        # browser()
        res<-eval(aa[[i]])
      }
      res
      # browser()
    } )
    # browser()
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

  #text code
  plot_code_expr=reactive({as.character(get_plot_codes())})
  output$text_gg_codes <- renderText({
    # browser()
    a <- plot_code_expr()
    gsub(";",";\n",gsub("\\+","\\+\n", a))
  })
  #text code
  UsedColNames_texts=reactive({as.character(get_geom_codes()[2])})
  #返回的内容
  return(
    list(
      #plot_code_expr=reactive({as.character(get_plot_codes())})
      plot_code_expr=plot_code_expr,
      UsedColNames_texts=UsedColNames_texts
      # width=reactive({input$output_plot_width}),
      # height=reactive({input$output_plot_height}),
      # scale=reactive({input$web_plot_scale}),
      # units="cm"
      )
    )
  # return(list(get_plot_codes()))
}
guiplot_Rexcle_Server <- function(input, output, sesson, data_and_name =NULL, field_groups=NULL) {
  #基于guiplot_dt_Server 函数修改得到，用于服务colClase_table函数

  ###########################################################
  ####自定以函数
  df_tran2typeTable<-function(data){
    ##用于从数据表格生成一个每个列数据类型的设置表格
    data<-data
    Class<-sapply(data, class)
    Class<-factor(Class,levels=c("","logical","numeric","factor","character"))
    Name<-names(data)
    data02<-data.frame(Name,Class)
    rownames(data02)<-NULL
    data02
  }

  txt_colClass<-function(data,typeTab,data_name){
    ##用于生成将列的类型由当前强制转换为typeTab中指定类型的文字，以便之后通过evalue使用
    out_txt<-""
    ##原类型和新类型是否一致
    class_Eq01<-mapply(identical ,typeTab[,2],sapply(data, class))
    ##新类型是否为空
    class_Eq02<-mapply(identical ,typeTab[,2],"")
    class_Eq<-!mapply(any,class_Eq01,class_Eq02)
    if(any(class_Eq)){
      ##获取需要修改类型的列的列名，并将此代码转换为文本

      txt_col_name<-typeTab[class_Eq,1]
      txt_col_name<-mapply(function(x){paste0("'",x,"'")},txt_col_name)
      txt_col_name<-if(length(txt_col_name)==1){txt_col_name}else{paste(txt_col_name,collapse=",")}
      #browser()
      txt_col_name<-paste0(c("c(",txt_col_name,")"),collapse="" )
      ##获取需要修改类型的列的拟修改类型，并将此代码转换为文本
      txt_col_class<-typeTab[class_Eq,2]
      txt_col_class<-mapply(function(x){paste0("'",x,"'")},txt_col_class)
      txt_col_class<-if(length(txt_col_class)==1){txt_col_class}else{paste(txt_col_class,collapse=",")}
      txt_col_class<-paste0(c("c(",txt_col_class,")"),collapse="")

      ##data[txt_col_name]<-colClass_as(data[txt_col_name],txt_col_class)
      out_txt<-paste0(c("",data_name,"[",txt_col_name,"]<-colClass_as(",data_name,"[",txt_col_name,"],",txt_col_class,")"),collapse="")
    }else {
       out_txt<-NULL
    }
    out_txt
  }
  ###end
  ############################################################

  ###########
  ###正文
  ##数据准备
  dataname<-c(data_and_name[[2]])
  data<-data_and_name[[1]]#####原始数据表格
  data02<-df_tran2typeTable(data)#####准备好的显示每个列类型的数据表格
  columns02 = data.frame(
    width= c(150, 120),
    #type=c('text', 'autocomplete'),
    readOnly=c(TRUE,FALSE)
  )
  ##列类型设置表格输出
  output$Rexcle_tb <- renderExcel(excelTable(
	  data=data02,
		columns = columns02,
		allowInsertRow = FALSE,
		allowInsertColumn = FALSE,
		allowDeleteRow = FALSE,
		allowDeleteColumn = FALSE,
		allowRenameColumn = FALSE,
		allowComments = FALSE,
		rowDrag = FALSE,
		columnSorting = FALSE,
		digits = NA,
		contextMenu =function() {FALSE},
		onselection= DT::JS("
		  function(el, x1, y1, x2, y2, origin){
  		  if(x1 == x2 && y1 == y2) {
			var cell = jexcel.current.records[y1][x1]
  		    jexcel.current.openEditor(jexcel.current.records[y1][x1], false);
			cell.children[0].dropdown.close(true)
		    }
		  }
    ")
	))
  ###########
  ###输出用于在绘图中使用的列类型转换的代码
  # browser()
  return(list(colClass_table=reactive({
      text=txt_colClass(data=data,excel_to_R(input$Rexcle_tb),data_name=dataname)
      return(text)
    })
  ))
  #end
  #################################
}

guiplot_dt_Server <- function(input, output, sesson, data_and_name =NULL, field_groups=NULL) {
  #server = FALSE
  #browser()
  #panel的名字dataname#################################
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
        autoFill = list(alwaysAsk=FALSE),
        # autoWidth = TRUE,
        columnDefs = list(
          # list(width = '20px', targets = 1:ncol(env_guiplot$dat)),
          list(className = 'dt-center success', targets = 1:ncol(env_guiplot$dat)),
          list(render=JS(Colum_render_js),targets = 1:ncol(env_guiplot$dat))
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
guiplot_geom_Additional_UGC_dt_Server <- function(input, output, sesson) {
  #geom_Additional_UGC
  #get geom type Codes
  # browser()
  ###设定geomUGC表格的初始结构与数值
  StaticData_geom_type<-c(StaticData_geom_type_1variable, StaticData_geom_type_2variable, StaticData_geom_type_other)
  int_use_geomtype<-FALSE
  int_geom_Additional_Code<-""
  int_geom_Additional_AesCode<-""

  ###设定geomUGC表格的初始结构与数值，放入一个dataframe中
  int_geomtype_UGC_table<-data.frame(

    use_geomtype=int_use_geomtype
    ,geom_type=StaticData_geom_type
    ,geom_Additional_Code=int_geom_Additional_Code
    ,geom_Additional_AesCode=int_geom_Additional_AesCode
  )
    #新建一个环境，用于储存需要跨函数修改的变量
  env_geomtype_UGC<- new.env(parent = emptyenv())
  env_geomtype_UGC$table <- int_geomtype_UGC_table

  get_use_geomtype<- reactive({
    ###去读哪些geom被选中
    c(
      input$geom_type_1variable,
      input$geom_type_2variable,
      input$geom_type_other
    )
  })

  geomtype_UGC_table<- reactive({
      # browser()
      ###动态更新表格中use_geomtype列的数值
      use_geomtype<-StaticData_geom_type %in% get_use_geomtype()
      env_geomtype_UGC$table$use_geomtype <- use_geomtype
      return(env_geomtype_UGC$table)
  })

  output$geom_Additional_UGC = renderDT({
    ###可视化geomtype_UGC_table表格中的内容
    geomtype_UGC_table()
    table_edit_info()
      datatable(
        env_geomtype_UGC$table,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0,1))),
        selection = list(mode = 'single', target = 'cell'),
        extensions = c('AutoFill'),
        class = 'table-hover',
        options = list(
          autoFill = list(alwaysAsk=FALSE, columns = c(2, 3)),
          dom = 't',paging = FALSE, ordering = FALSE,
          columnDefs = list(
            list(targets = c(1:3), searchable = FALSE),
            list(targets = c(0), visible=FALSE)
          ),
          search = list(search = "true")
        )
      )
  })

  table_edit_info<- reactive({input$geom_Additional_UGC_cell_edit})###获取celledit的信息
  observeEvent(input$geom_Additional_UGC_cell_edit, {
    ###应用celledit的信息更新表格
    #########
    #由于DT表格显示时，未显示行名称列，由此导致列数少了1，从而导致info中的列数计算向左偏了1列，所有此处更新编辑的信息时，取子集
    #########
    # 由于仅包含一个observe，仅能更新环境变量中的数据，而不是同时更新DT表格的显示，所以此处额外增加了一个info的reactive数值，作为中转，
    # 并且在observer和DT中都同时引用此reactive，以便达成更新环境中的表格时更新DT表格显示
    ########
    # browser()
    env_geomtype_UGC$table[2:4] <- editData(env_geomtype_UGC$table[2:4], table_edit_info(), 'geom_Additional_UGC')
  })

  # return(env_geomtype_UGC$table)

  return(
    reactive({
    geomtype_UGC_table()
    table_edit_info()
      ls <- env_geomtype_UGC$table
      return(ls)
    })
  )
}

#################################
#################################
#################################
#GetMappingValue for guiplot_plot_Server
GetMappingValue<-function(data,column){
  #browser()
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

GetUsedDataColumnsNames<-function(data){
  nr<-nrow(data)
  if (is.null(nr)) return(NULL)#如果表格为空，则退出
  # browser()
  data_exclude_None <- data[,-1]#去掉none字段
  sumD <- apply(data_exclude_None,1,sum)#求每一行的和
  if(sum(sumD)==0) return(NULL)
  usecols <- rownames(data)[sumD!=0]#如果和不为0（未作任何映射），则返回此行的名称（数据集中的列名）
  usecols <- gsub(" ","_",usecols)#WNL的列名中不支持空格，所以此处将空格替换为下划线
  usecols
  # browser()
}

####~~ JS回调代码-----
####~~~ callback for guiplot_dt_Server------
####获取Autofill的填充相关的事件，并包装为一个输入项_cells_filled返还给shiny
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
  #########################################################################
  ####~~ 将“AutoFill”的填充提示选项中的不需要的部分移除，以使不弹出提示#######
  "delete $.fn.dataTable.AutoFill.actions.increment;",
  "delete $.fn.dataTable.AutoFill.actions.fillHorizontal;",
  "delete $.fn.dataTable.AutoFill.actions.fillVertical;",
  ##########################################################################
  ####为tbody添加鼠标悬浮事件捕获，以设置dt-autofill-handle的附加类#########
  ####通过附加类为dt-autofill-handle添加其他固定的样式
  ####以使dt-autofill-handle不在消失
  "$('tbody').on('mouseover', function() {$('.dt-autofill-handle').addClass('hdAa')});",
  "	var style = document.createElement('style');",
  "	style.innerHTML = '.hdAa {background: green !important;display: block!important}';",
  "	document.head.appendChild(style);"
  ##########################################################################
)


Colum_render_js <- c(
  ##########################################
  ####列的反应式渲染，已将0、1变为复选框####
  " function(data, type, row, meta) {",
  " if(data == 0){return '<input type=\"checkbox\" >'}",
  " if(data == 1){return '<input type=\"checkbox\"  checked>'}}"
)
