#ggplot codes
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
	
	#set expand range

	arry2<- c(
		"expand "," = c(",
		axis$expand_p,
		",",
		axis$expand_u,
		")"
	)
	coord_code$expand<- paste(sep="",collapse ="",arry2)
	arry2<-NULL
	
	
	
	#paste trans and range
	arry3<-c("scale_",x_y,"_continuous(",coord_code$trans,",",coord_code$lim,",",coord_code$expand,")")
	one_axis<-paste(sep="",collapse ="",arry3)
	return(one_axis)
}