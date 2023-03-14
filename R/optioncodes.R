#ggplot facets codes
facets_code<-function(cols,rows){
  if (is.null(cols)&&is.null(rows))
    return()
  cols<-GetFacetGridText(cols)
  rows<-GetFacetGridText(rows)
  text<-paste(sep="","facet_grid(",paste(collapse=",",c(cols,rows)),")")
  return(text)
}
GetFacetGridText<-function(data){
  # browser()
  if(is.null(data))
    return(NULL)
  n<-length(data)
  name<-substitute(data)
  # browser()
  if(n==1){
    FacetGridText<-paste(sep="",collapse="",name,"=","(","vars","(",data,")",")")
    FacetGridText
  }else{
    text<-paste(sep="",collapse=",",c(data))
    text<-paste(sep="",collapse="","interaction(",text,",","sep = ':'",")")
    FacetGridText<-paste(sep="",collapse="",name,"=","(","vars","(",text,")",")")
    FacetGridText
  }
}

#ggplot axis codes
coord_trans_code<-function(axis_x,axis_y){
	if (is.null(axis_x)||is.null(axis_x))
		return()
	x_code<-axis_to_coord_code(axis_x,"x")
	y_code<-axis_to_coord_code(axis_y,"y")

	arry<-c(x_code,y_code)
	axis_code<-paste(sep="+",collapse ="+",arry)
	if (is.null(axis_code)){
		return()
	}else{
	#coord_trans_code<-paste(collapse ="","coord_trans","(",axis_code,")")
	coord_trans_code<-axis_code
	return(coord_trans_code)
	}
}

axis_to_coord_code<-function(axis,x_y){
	#set Axis trans
	coord_code<-list(trans = NULL,lim = NULL)
	if (axis$Scale=='identity')
	{
	  coord_code$trans<-NULL
	}else{
	  arry1<- c(
			"trans =","'",
			switch(
				axis$Scale,
				identity = 'identity',
				log10 = 'log10',
				log2 = 'log2',
				logit = 'logit',
				probability = 'probability',
				sqrt = 'sqrt'
			),
			"'"
		)
		coord_code$trans<- paste(sep="",collapse ="",arry1)
		arry1<-NULL
	}

	#set Axis range
	if (axis$Range=='none')
	{
	  coord_code$lim<-NULL
	}else{
	  arry2<- c(
			"limits "," = c(",
			axis$Minimum,
			",",
			axis$Maximum,
			")"
		)
		coord_code$lim<- paste(sep="",collapse ="",arry2)
		arry2<-NULL
	}

	#set Axis Tick
	if (any(is.null(axis$Tick),axis$Tick==""))
	{
	  coord_code$Tick<-NULL
	}else{
		coord_code$Tick<- paste(sep="",collapse ="","breaks = c(",axis$Tick,")")
	}

	#set expand range
	if (axis$expand_p==0.05 && axis$expand_u==0){
	  coord_code$expand<-NULL
	}else{
  	arry2<- c(
  		"expand "," = c(",
  		axis$expand_p,
  		",",
  		axis$expand_u,
  		")"
  	)
  	coord_code$expand<- paste(sep="",collapse ="",arry2)
  	arry2<-NULL
	}


	# browser()
	#paste trans and range
	if(
	  is.null(coord_code$trans) &&
	  is.null(coord_code$lim) &&
	  is.null(coord_code$expand) &&
	  is.null(coord_code$Tick)
	){
	  one_axis<-NULL
	}else{
		arry3 <- paste1(sep=",",collapse ="",coord_code$trans,coord_code$lim,coord_code$Tick,coord_code$expand)
	#   arry3<-c("scale_",x_y,"_continuous(",coord_code$trans,",",coord_code$lim,",",coord_code$expand,")")
	#   one_axis<-paste(sep="",collapse ="",arry3)
	  one_axis<-paste(sep="",collapse ="","scale_",x_y,"_continuous(",arry3,")")
	  arry3<-NULL
	}
	return(one_axis)
}
plot_labs_code<-function(x=NULL,y=NULL,title=NULL,subtitle=NULL,caption=NULL,tag=NULL,color=NULL,shape=NULL,linetype=NULL){
	labs_value<-c(x,y,title,subtitle,caption,tag,color,shape,linetype)
	#  browser()
	labs_codes<-c(
	  paste0("x = '",x,"'"),
	  paste0("y = '",y,"'"),
	  paste0("title = '",title,"'"),
	  paste0("subtitle = '",subtitle,"'"),
	  paste0("caption = '",caption,"'"),
	  paste0("tag = '",tag,"'"),
	  paste0("color = '",color,"'"),
	  paste0("shape = '",shape,"'"),
	  paste0("linetype = '",linetype,"'")
	)
	labs_code<-labs_codes[!sapply(labs_value,function(a)any(is_empty(a),is.null(a),a==""))]

	if(any(is_empty(labs_code),is.null(labs_code),labs_code=="")){
		return()
	}else {
		labs_code <-paste0(labs_code,collapse =",")
		labs_code <-paste0("theme(text=element_text(family='Songti SC'))+labs(",labs_code,")")
		return(labs_code)
	}
}

#Reference line codes
reference_line_code<-function(type=NULL,intercept=NULL,color=NULL,size=NULL,add_UGC=NULL,slope=NULL,Diagonal_Line=FALSE){
	#将如果空需要剔除的变量分别装入 line_value和line_code
	line_value<-c(intercept,color,size,slope,add_UGC)
	#转换下字符
	ls <- switch(type,
		v="x",
		h="y",
		ab=""
	)

	line_code<-c(
	  paste0(ls,"intercept = ",intercept,""),
	  paste0("color = ",color,""),
	  paste0("size = ",size,""),
	  paste0("slope = ",slope,""),
	  add_UGC
	)

	#此处剔除掉所有空的变量对应的代码
	line_code<-line_code[!sapply(line_value,function(a)any(is_empty(a),is.null(a),a==""))]

	if(Diagonal_Line==TRUE){
		#如果室对角线，则直接输出，无论图形参数中其他是否有
		line_codes<-paste(sep="",collapse =",",line_code)
		ref_line_code <- paste0("geom_",type,"line(",line_codes,")")
		return(ref_line_code)
	}
	#如果空则返回空，否则返回代码
	if(any(is_empty(line_code),is.null(line_code),line_code=="")){
		return()
	}else {
		line_codes<-paste(sep="",collapse =",",line_code)
		ref_line_code <- paste0("geom_",type,"line(",line_codes,")")
		return(ref_line_code)
	}
}
plot_themes_code<-function(radioButtons_plot_themes){
  arry4<-c(switch(
    radioButtons_plot_themes$plot_themes,
    theme_gray = 'theme_gray()',
    theme_bw = 'theme_bw()',
    theme_linedraw = 'theme_linedraw()',
    theme_light = 'theme_light()',
    theme_dark = 'theme_dark()',
    theme_minimal = 'theme_minimal()',
    theme_classic = 'theme_classic()',
    theme_void = 'theme_void()',
    theme_minimal = 'theme_minimal()'
  ))
  plot_themes<-paste(sep="",collapse ="",arry4)
  arry4<-NULL
  return(plot_themes)
}

#ggplot legend Position codes
legend_position_code<-function(Legend_Visible="",Legend_Docking="",Relative_Position_Select="",Legend_X_Offset="",Legend_Y_Offset=""){
	if(Legend_Visible==FALSE){
		return("theme(legend.position = 'none')")
	}else{
		if(Legend_Docking=="Relative_Position"){
			return(paste0(
				"theme(legend.position = '",
				Relative_Position_Select,
				"')"
			))
		}else {
			return(paste0(
		   		"theme(legend.position = c(",
				Legend_X_Offset/100,",", Legend_Y_Offset/100,
				"))"
		   ))
		}
	}
}

#ggplot User customer codes
UGC_code<-function(UGC_text){
	a<-UGC_text
	if(any(is_empty(a),is.null(a),a==""))a<-""
	return(a)
}

