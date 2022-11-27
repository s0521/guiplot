#Static data########################################################
##geom type
StaticData_geom_type_1variable <- c("area","density","dotplot","freqpoly","histogram","bar","col")
StaticData_geom_type_2variable <- c("line","point","ribbon","qq_line","quantile","rug","segment","smooth","text","boxplot","violin","bin2d","density2d","path","step")
StaticData_geom_type_other <- c("crossbar","errorbar","linerange","pointrange","map","contour","raster","tile","polygon","rect")
paste1 <- function(...,sep="",collapse="",na.omit=TRUE){
    dots <- list(...)
    # browser()
    # NAValues=c("",NA,NULL)
    dots <- dots[!sapply(dots,is.null)]
    dots <- dots[dots!=""]
    dots <- dots[!is.na(dots)]  
    paste(dots,collapse=sep)
}