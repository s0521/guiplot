#ggplot codes
coord_trans_code<-function(axis_x,axis_y){
	if (is.null(axis_x)||is.null(axis_x))
		return()
	x_code<-axis_to_coord_code(axis_x,"x")
	y_code<-axis_to_coord_code(axis_y,"y")
	
	axis_code<-paste(sep=",",x_code,y_code)
	if (is.null(axis_code)){
		return()
	}else{
	coord_trans_code<-paste(collapse ="","coord_trans","(",axis_code,")")
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
			x_y,"=","'",
			switch(
				axis$Scale, 
				identity = 'identity', 
				log10 = 'log10', 
				log2 = 'log2',
				ln = 'ln',
				sqrt = 'sqrt'
			),
			"'"
		)
		coord_code$trans<- paste(sep="",collapse ="",arry1)
	}

	#set Axis range
	if (axis$Range=='none')
	{
	  coord_code$lim<-NULL
	}else{
	  arry2<- c(
			"lim",x_y," = c(",
			axis$Minimum,
			",",
			axis$Maximum,
			")"
		)
		coord_code$lim<- paste(sep="",collapse ="",arry2)
	}

	#paste trans and range
	arry3<-c(coord_code$trans,coord_code$lim)
	one_axis<-paste(sep=",",collapse =",",arry3)
	return(one_axis)
}