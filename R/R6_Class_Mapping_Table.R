#单选类
#A R6 Class to handle Single-choice events
#用于处理字段类型为单选型(该映射列表中的字段仅被允许映射一个变量)的一个R6类
Single_Mptbl<-R6Class("Single_Mptbl",
  #inherit=父类,
  public =list(
    field= NULL,
    variable= NULL,
    default_field= NULL,
    # field_group= NULL,
    preselection_field= NULL,
    # selection_inf= NULL,
    # fill_inf= NULL,
    mapping_table= NULL,

    ############################################
    # methods,方法
    #初始化方法，将参数传递给字段
    initialize = function(field=NULL,
                          variable=NULL,
                          default_field=NULL,
                          preselection_field=NULL,
                          mapping_table=NULL
    ) {
      self$field=field
      self$variable=variable
      self$default_field=default_field
      self$preselection_field=preselection_field
      self$mapping_table=mapping_table
    },

    #生成映射列表
    create_mptbl= function() {
      #"create a mapping_table"
      nr<-length(self$variable)
      nc<-length(self$field)

      local_mptbl<-matrix(as.integer(rep(0,nc*nr)),nrow=nr,ncol=nc, byrow = TRUE)
      colnames(local_mptbl)<-self$field
      rownames(local_mptbl)<-self$variable
      self$mapping_table <- local_mptbl
      local_mptbl<-NULL
    },

    #选中默认选项
    mptbl_select_default_field= function(default_field) {
      #"mapping_table select default field"
      #待编写错误捕获
      local_mptbl<-self$mapping_table
      nr<-nrow(local_mptbl)
      nc<-ncol(local_mptbl)
      #browser()
      for (i in seq_len(nr)){
        if(sum(local_mptbl[i,])!=1){
          local_mptbl[i,]<-rep(0,nc)
          local_mptbl[i,default_field]<- 1
        }
      }
      self$mapping_table <- local_mptbl
      local_mptbl<-NULL
    },

    #选择预选选项
    mptbl_select_preselection_field= function() {
      #"mapping_table select preselection field"
      },

    #选中事件
    mptbl_event_select= function(selection_inf) {
      #"Handle single-choice selection events"
      # browser()
      aaa<-selection_inf
      local_mptbl<-self$mapping_table
      for (i in 1:nrow(selection_inf)){
        n_row<-as.integer(selection_inf[i,1])
        n_col<-as.integer(selection_inf[i,2])
        if (selection_inf[i,3]==0 ) {
          local_mptbl[n_row,n_col]<-1
          selection_inf[i,3]<-1
          self$mptbl_event_fill(selection_inf)
        }else{
          if (selection_inf[i,3]==1 ) {
            local_mptbl[n_row,n_col]<-0
            selection_inf[i,3]<-0
            self$mptbl_event_fill(selection_inf)
          }
        }
      }
      # self$mapping_table <- local_mptbl
      local_mptbl<-NULL
    },

    #填充事件
    mptbl_event_fill= function(fill_inf) {
      #"Handle fill selection events"
      # browser()
      local_mptbl<-self$mapping_table
      select_row_fist<-fill_inf[1,1]
      select_row_end<-fill_inf[nrow(fill_inf),1]
      select_col<-as.integer(c(fill_inf[1,2]))
      select_mptal_range<-local_mptbl[select_row_fist:select_row_end,,drop=F]
      nr<-nrow(select_mptal_range)
      nc<-ncol(select_mptal_range)
      if (fill_inf[1,3]==0){
        select_mptal_range[,select_col]<-rep(0,nrow(select_mptal_range))
        local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
        self$mapping_table <- local_mptbl
        local_mptbl<-NULL
        # self$mapping_table<-mptbl_select_default_field(1)
        self$mptbl_select_default_field(self$default_field)
      }
      else
      {
        select_mptal_range[]<-matrix(rep(0,nr*nc),nrow=nr,ncol=nc)
        select_mptal_range[,select_col]<-rep(1,nr)
        local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
        self$mapping_table <- local_mptbl
      }
    }
  )
)

#多选类
#A R6 Class to handle multiple-choice events
#用于处理字段类型为多选型(该映射列表中的字段可同时映射多个变量)的一个R6类
Multiple_Mptbl<-R6Class("Multiple_Mptbl",
                        inherit =Single_Mptbl,
                        public  =list(
                               #选中事件
                               mptbl_event_select= function(selection_inf) {
                                 #"Handle multiple-choice selection events"
                                 # browser()
                                 aaa<-selection_inf
                                 local_mptbl<-self$mapping_table
                                 for (i in 1:nrow(selection_inf)){
                                   n_row<-as.integer(selection_inf[i,1])
                                   n_col<-as.integer(selection_inf[i,2])
                                   if (selection_inf[i,3]==0 ) {
                                     local_mptbl[n_row,n_col]<-1
                                     # selection_inf[i,3]<-1
                                     # mptbl_event_fill(selection_inf)
                                   }else{
                                     if (selection_inf[i,3]==1 ) {
                                       local_mptbl[n_row,n_col]<-0
                                       # selection_inf[i,3]<-0
                                       # mptbl_event_fill(selection_inf)
                                     }
                                   }
                                 }
                                 self$mapping_table <- local_mptbl
                                 local_mptbl<-NULL
                               },

                               #填充事件
                               mptbl_event_fill= function(fill_inf) {
                                 #"Handle fill selection events"
                                 # browser()
                                 local_mptbl<-self$mapping_table
                                 select_row_fist<-fill_inf[1,1]
                                 select_row_end<-fill_inf[nrow(fill_inf),1]
                                 select_col<-as.integer(c(fill_inf[1,2]))
                                 select_mptal_range<-local_mptbl[select_row_fist:select_row_end,,drop=F]
                                 nr<-nrow(select_mptal_range)
                                 nc<-ncol(select_mptal_range)
                                 if (fill_inf[1,3]==0){
                                   select_mptal_range[,select_col]<-rep(0,nrow(select_mptal_range))
                                   local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
                                   self$mapping_table <- local_mptbl
                                   local_mptbl<-NULL
                                   self$mptbl_select_default_field(self$default_field)
                                 }
                                 else
                                 {
                                   # select_mptal_range[]<-matrix(rep(0,nr*nc),nrow=nr,ncol=nc)
                                   select_mptal_range[,select_col]<-rep(1,nr)
                                   local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
                                   self$mapping_table <- local_mptbl
                                 }
                               }
                             )
                             )

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
                             self$default_field=default_field
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

                               #选中默认选项
                               self$mptbl_select_default_field(self$default_field)

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
                               self$mapping_table[,default_field]<-1L
                             },

                             #预选事件
                             mptbl_select_preselection_field = function(preselection_field) {
                               #"mapping_table select preselection field"
                               self$mptbl_event_select(preselection_field)
                             },


                             #选中事件
                             mptbl_event_select= function(selection_inf) {
                               #"mapping_table select event assign"
                               # browser()
                               for (j in 1:nrow(selection_inf)){
                                 i_num<-selection_inf[j,2]
                                 i<-as.integer(self$field_groups[2,i_num])
                                 if (i%%2 ==0){
                                   group_name<-paste("multiple",i,sep ="")
                                   assign(group_name,self$get_sub_group(group_name))
                                   selection_inf[j,2]<-self$field_index_transform[4,i_num]
                                   local_default_field<-as.integer(self$field_index_transform[4,self$default_field])
                                   local_obj<-get(group_name)
                                   local_obj$default_field<-local_default_field

                                   # browser()
                                   local_obj$mptbl_event_select(selection_inf[j,,drop=F])

                                   self$set_mptabl(group_name,local_obj$mapping_table)
                                 }else{
                                   group_name<-paste("single",i,sep ="")
                                   assign(group_name,self$get_sub_group(group_name))
                                   selection_inf[j,2]<-self$field_index_transform[4,i_num]
                                   local_default_field<-as.integer(self$field_index_transform[4,self$default_field])
                                   local_obj<-get(group_name)
                                   local_obj$default_field<-local_default_field

                                   # browser()
                                   local_obj$mptbl_event_select(selection_inf[j,,drop=F])

                                   self$set_mptabl(group_name,local_obj$mapping_table)
                                 }
                               }
                             },

                             #填充事件
                             mptbl_event_fill= function(fill_inf) {
                               #"mapping_table fill event assign"
                               # browser()
                                 fill_inf<-as.matrix(fill_inf,ncol=3)
                                 i_num<-fill_inf[1,2]
                                 i<-as.integer(self$field_groups[2,i_num])
                                 # browser()
                                 if (i%%2 ==0){
                                   group_name<-paste("multiple",i,sep ="")
                                   assign(group_name,self$get_sub_group(group_name))
                                   fill_inf[,2]<-self$field_index_transform[4,i_num]
                                   local_default_field<-as.integer(self$field_index_transform[4,self$default_field])
                                   local_obj<-get(group_name)
                                   local_obj$default_field<-local_default_field

                                   local_obj$mptbl_event_fill(fill_inf)

                                   self$set_mptabl(group_name,local_obj$mapping_table)
                                 }else{
                                   group_name<-paste("single",i,sep ="")
                                   assign(group_name,self$get_sub_group(group_name))
                                   fill_inf[,2]<-as.integer(self$field_index_transform[4,i_num])
                                   local_default_field<-as.integer(self$field_index_transform[4,self$default_field])
                                   local_obj<-get(group_name)
                                   local_obj$default_field<-local_default_field

                                   local_obj$mptbl_event_fill(fill_inf)

                                   self$set_mptabl(group_name,local_obj$mapping_table)
                                 }
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
                               field_display_group<-field_groups[3,]

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
