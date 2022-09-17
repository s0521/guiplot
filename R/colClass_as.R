#each column in data.frame() as user-specified class.
#'
#' @title colClass_as
#' @param data data.frame only. The dataset to be modified
#' @param type Vector of strings. Only four class are supported c("logical","numeric","factor","character")
#' @export
#' @return data.frame.
#' @examples
#' if (interactive()) {
#' # Launch with built-in PK data set:
#' colClass_as(PK,c("character","numeric","factor","character"))
#' }
#'
colClass_as<-function(data,type){
	##用于将列的类型由当前强制转换为typeTab中指定类型，并范围
	data<-data
	type<-type
	out_data<-data
	#browser()
    try({
        if(is.data.frame(data)&ncol(data)!=1){
            for( i in 1:ncol(data)){
                txt=paste0(c("as.",type[i],"(",quote(out_data[,i]),")"),collapse="")
                out_data[,i]<-eval(parse(text=txt))
            }
        }else{
            txt=paste0("as.",type,"(unlist(",quote(out_data),"))",collapse="")
            ls<-eval(parse(text=txt))
            out_data<-ls
        }
        out_data
    })
}