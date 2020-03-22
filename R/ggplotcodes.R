#ggplot codes
geomCode<-function(type,data,x,y,group=NULL){
	if (is.null(data)||is.null(x)||is.null(y))
		return(expr(ggplot()))
  #name<-deparse(substitute(data))
  name<-data
	type<-GetTypeText(type)
	data<-GetDataText(name)
	x<-GetXText(x)
	y<-GetYText(y)
	group<-GetGroupText(group)
	#geomCode<-GetGeomCode(type,data,x,y,group)
	geomCode<-SetEveryY(type,data,x,y,group)

	ggcalltext<-paste(sep="","ggplot() +",geomCode)
	cat(file=stderr(), "ggcalltext is ",ggcalltext)
	return(ggcalltext)

	#browser()
	# ggcall<-parse_expr(ggcalltext)
	# #cat(file=stderr(), "drawing histogram with",ggcall)
	# ggcall
	# eval(ggcall)

}

#anasisy Y
SetEveryY<-function(type,data,x,y,group){
  if(is.null(y))
    return()
  j<-1L
  geomCode<-c()
  for(i in y){
    geomCode[j]<-GetGeomCode(type,data,x,i,group)
    j<-j+1
  }
  geomCodes<-paste(geomCode,collapse ="+")
  #geomCodes<-parse_expr(geomCodes)
  geomCodes
}



#Get ggplot Geom Codes
GetGeomCode<-function(type,data,x,y,group){
  if(is.null(type))
    return()

  aes<-paste(sep="","aes","(",x,",",y,",",group,")")

  j<-1
  Code<-c()
  for(i in type){
    Code[j]<-paste(sep="",i,"(",paste(sep=",",data,aes),")")
                   j<-j+1
  }
  Codes<-paste(Code,collapse ="+")
  Codes
}


GetTypeText<-function(data){
	if(is.null(data))
		return()
  GetTypeText<-paste(sep="","geom_",data)
  GetTypeText
}

GetDataText<-function(data){
if(is.null(data))
return()
GetDataText<-textp("data",data)
GetDataText
}



GetXText<-function(data){
if(is.null(data))
return()
GetXText<-textp("x",data)
GetXText
}



GetYText<-function(data){
if(is.null(data))
return()
GetYText<-textp("y",data)
GetYText
}



GetGroupText<-function(data){
if(is.null(data))
return(NULL)
GetGroupText<-textp("color",data)
GetGroupText
}

#textp int
textp1<-function(name,data){
if(is.null(data)||is.null(name))
return()
if(is.character(data)){
GetDataText<-paste(sep="",name,"=",data)
}
else{
GetDataText<-paste(sep="",name,"=",substitute(data))
}
GetDataText
}

#textp final
textp<-function(name,data){
if(is.null(data)||is.null(name))
return()
GetDataText<-paste(sep="",name,"=",data)
GetDataText
}
