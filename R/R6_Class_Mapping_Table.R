#调度类，用于接收数据，并调用多选类和单选类处理数据
#A R6 Class to handle mapping_table events
Mapping_Table_class<-R6Class("Mapping_Table",
                       #inherit =父类,
                       public =list(
                             field_groups= NULL,
                             variable= NULL,
                             default_field= NULL,
                             #field_group= NULL,
                             preselection_field= NULL,
                             # selection_inf= NULL,
                             # fill_inf= NULL,
                             field_index_transform= NULL,
                             mapping_table= NULL,
                             inf_of_mptbl= NULL,
                             field_right_bound= NULL,
                             DT_container= NULL,

                             ############################################
                             # methods 方法
                             #初始化方法，将参数传递给字段
                             initialize = function(field_groups=NULL,
                                                   variable=NULL,
                                                   default_field=NULL,
                                                   preselection_field=NULL,
                                                   field_index_transform=NULL,
                                                   mapping_table=NULL,
                                                   inf_of_mptbl=NULL,
                                                   field_right_bound=NULL,
                                                   DT_container=NULL
                             ){self$field_groups=field_groups
                             self$variable=variable
                            #  self$default_field=default_field
                             self$default_field=field_groups[5, ]
                             self$preselection_field=preselection_field
                             self$field_index_transform=field_index_transform
                             self$mapping_table=mapping_table
                             self$inf_of_mptbl=inf_of_mptbl
                             self$field_right_bound=field_right_bound
                             self$DT_container=DT_container
                             },

                             #生成映射列表
                             create_mptbl= function() {
                               #"create a mapping_table, and select default field,and set field_right bound, set DT container"
                               nr<-length(self$variable)
                               nc<-length(self$field_groups[1,])

                               local_mptbl<-matrix(as.integer(rep(0,nc*nr)),nrow=nr,ncol=nc, byrow = TRUE)
                               colnames(local_mptbl)<-self$field_groups[1,]
                               rownames(local_mptbl)<-self$variable

                               #给mapping_table赋值
                               self$mapping_table <- local_mptbl
                               local_mptbl<-NULL

                               # 设置行的隐藏，直接赋值为1
                               self$mapping_table[,1]<-1L

                               #选中默认选项
                              #  browser()
                               self$mptbl_select_default_field(self$default_field)

                               self$mapping_table <- rbind(self$field_groups,self$mapping_table)

                               #给field_right_bound赋值
                               self$set_field_right_bound(self$field_groups)
                               #给DT_container赋值
                               self$set_DT_container(self$field_groups)
                             },

                             #分别配字段通过字段组#该方法暂时没有用
                             assign_field_by_field_group=function() {
                               #"assign field by field_group"
                               field_value<-self$field_groups[1,]
                               field_group<-as.integer(c(self$field_groups[2,]))
                               field_display_group<-self$field_groups[3,]
                               group<-unique(self$field_group)

                               for (i in group){
                                 if (i%%2 ==0){
                                   group_name<-paste("multiple",i,sep ="")
                                   group_index<-c(field_group==i)
                                   assign(group_name,Multiple_Mptbl$new(mapping_table=self$mapping_table[,group_index,drop=F]))
                                 }else{
                                   group_name<-paste("single",i,sep ="")
                                   group_index<-c(field_group==i)
                                   assign(group_name,Multiple_Mptbl$new(mapping_table=self$mapping_table[,group_index,drop=F]))
                                 }
                               }
                             },

                             #选中默认选项
                             mptbl_select_default_field = function(default_field) {
                               #"mapping_table select default field"
                              #  browser()
                               nc<-length(self$field_groups[1,])
                               for (i in 1:nc){
                                  if (default_field[i]=="1"){
                                    self$mapping_table[,i]<-1L
                                    i
                                    # browser()
                                  }
                               }
                             },

                             #预选事件
                             mptbl_select_preselection_field = function(preselection_field) {
                               #"mapping_table select preselection field"
                               self$mptbl_event_select(preselection_field)
                             },

                             ############################################
                             #内部方法——从mapping_table获取子表格
                             get_sub_group=function(group_name) {
                               #"Internal method: get sub_group from mapping_table"
                               # field_value<-field_groups[1,]
                               field_group<-as.integer(c(self$field_groups[2,]))
                               # field_display_group<-field_groups[3,]
                               aaab<-rbind(self$field_groups,NA)
                               self$field_index_transform<-aaab

                               get_group<-function(group_name){
                                 if (substring(group_name,1,1)=="s") {
                                   substring(group_name,7,8)
                                 }else{
                                   if (substring(group_name,1,1)=="m") substring(group_name,9,10)
                                 }
                               }
                               group<-get_group(group_name)
                               i<-as.integer(group)
                               # browser()
                               if (i%%2 ==0){
                                 #multiple
                                 group_index<-c(field_group==i)
                                 assign(group_name,Multiple_Mptbl$new(mapping_table=self$mapping_table[,group_index,drop=F]))
                                 self$field_index_transform[4,group_index]<-c(1:sum(group_index))
                               }else{
                                 #single
                                 group_index<-c(field_group==i)
                                 assign(group_name,Single_Mptbl$new(mapping_table=self$mapping_table[,group_index,drop=F]))
                                 self$field_index_transform[4,group_index]<-c(1:sum(group_index))
                               }
                               get(group_name)
                             },

                             #内部方法——将子表格的内容赋值给mapping_table
                             set_mptabl = function(group_name,mptbl) {
                               #"Internal method: Use the value of the subtable to set the value of the"
                               # field_value<-field_groups[1,]
                               field_group<-as.integer(c(self$field_groups[2,]))
                               # field_display_group<-field_groups[3,]

                               get_group<-function(group_name){
                                 if (substring(group_name,1,1)=="s") {
                                   substring(group_name,7,8)
                                 }else{
                                   if (substring(group_name,1,1)=="m") substring(group_name,9,10)
                                 }
                               }
                               group<-as.integer(get_group(group_name))
                               group_index<-c(field_group==group)
                               self$mapping_table[,group_index]<-mptbl
                               self$mptbl_to_inf(self$mapping_table)
                             },

                             #内部方法——将mapping_table转换为inf格式
                             mptbl_to_inf= function(local_mapping_table) {
                               #"Internal method: Convert mapping_table to inf_of_mptbl"
                               # browser()
                               nr<-nrow(local_mapping_table)
                               nc<-ncol(local_mapping_table)
                               m<-matrix(as.numeric(rep(0,nr*nc*3)),nrow=nr*nc,ncol=3, byrow = TRUE)

                               colnames(m)<-c("row","col","value")
                               k<-1
                               for (i in seq_len(nr)) {
                                 for (j in seq_len(nc)) {
                                   m[k,1] = i
                                   m[k,2] = j
                                   m[k,3] = local_mapping_table[i,j]
                                   k<-k+1
                                 }
                               }
                               #给inf_of_mptbl赋值
                               self$inf_of_mptbl<-m
                             },

                             #内部方法——将field_groups转换为field_right_bound
                             set_field_right_bound = function(field_groups) {
                               #"Internal method: set field right bound"
                               # field_value<-field_groups[1,]
                               # field_group<-as.integer(c(field_groups[2,]))
                               field_display_group<-field_groups[3,]
                               v<-c()
                               for (i in 1: (length(field_display_group)-1)){
                                 if (field_display_group[i]!=field_display_group[i+1]){
                                   v<-c(v,i)
                                 }
                               }
                               self$field_right_bound<-v
                             },

                             #内部方法——将field_groups转换为DT_container
                             set_DT_container = function(field_groups) {
                               #"Internal method: set DT container"
                               field_value<-field_groups[1,]
                               # field_group<-as.integer(c(field_groups[2,]))
                               field_display_group<-field_groups[2,]

                               # field_name<-field_groups[1,]
                               # field_group<-as.integer(c(field_groups[2,]))
                               # field_display_group<-field_groups[3,]
                               group<-unique(field_display_group)
                               n_of_group<-length(group)

                               container_head<-c("<table class = 'display'> <thead>")
                               container_foot<-c("</thead></table>")

                               container_body1_element<-NULL
                               for (i in 1:length(group)){
                                 container_body1_element[i]<-paste(sep="",collapse = "",
                                                                   "<th ",
                                                                   "colspan =",sum(c(field_display_group==group[i]))," ",
                                                                   "class='sorting_disabled dt-center' ",
                                                                   "style='border-right: solid;'>",
                                                                   "",group[i],"",
                                                                   "</th>"
                                 )
                               }
                               container_body1_element<-c(c("<th rowspan = 2 ></th>"),container_body1_element)
                               container_body1_es<-paste(sep="",collapse = "",container_body1_element)
                               container_body1<-paste(sep="",collapse = "","<tr>",container_body1_es,"</tr>")

                               container_body2_element<-NULL
                               for (i in 1:length(field_value)){
                                 container_body2_element[i]<-paste(sep="",collapse = "","<th>","",field_value[i],"","</th>")
                               }
                               container_body2_es<-paste(sep="",collapse = "",container_body2_element)
                               container_body2<-paste(sep="",collapse = "","<tr>",container_body2_es,"</r>")

                               # browser()
                               container_value<-paste(sep="",collapse = "",container_head,container_body1,"",container_body2,container_foot)
                               self$DT_container<-container_value
                             }
                           )
)
