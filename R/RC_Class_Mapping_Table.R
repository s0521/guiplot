#单选类
Single_Mptbl<-setRefClass("Single_Mptbl",
  #contains=父类,
  fields=list(
    field="vector",
    variable="vector",
    default_field="numeric",
    # field_group="vector",
    preselection_field="matrix",
    # selection_inf="matrix",
    # fill_inf="matrix",
    mapping_table="matrix"
  ),
  methods=list(
    #生成映射列表
    create_mptbl= function() {
      nr<-length(variable)
      nc<-length(field)

      local_mptbl<-matrix(as.integer(rep(0,nc*nr)),nrow=nr,ncol=nc, byrow = TRUE)
      colnames(local_mptbl)<-field
      rownames(local_mptbl)<-variable
      mapping_table <<- local_mptbl
      local_mptbl<-NULL
    },

    #选中默认选项
    mptbl_select_default_field= function(default_field) {
      #待编写错误捕获
      local_mptbl<-mapping_table
      nr<-nrow(local_mptbl)
      nc<-ncol(local_mptbl)
      #browser()
      for (i in seq_len(nr)){
        if(sum(local_mptbl[i,])!=1){
          local_mptbl[i,]<-rep(0,nc)
          local_mptbl[i,default_field]<- 1
        }
      }
      mapping_table <<- local_mptbl
      local_mptbl<-NULL
    },

    #选择预选选项
    mptbl_select_preselection_field= function() { },

    #选中事件
    mptbl_event_select= function(selection_inf) {
      # browser()
      aaa<-selection_inf
      local_mptbl<-mapping_table
      for (i in 1:nrow(selection_inf)){
        n_row<-as.integer(selection_inf[i,1])
        n_col<-as.integer(selection_inf[i,2])
        if (selection_inf[i,3]==0 ) {
          local_mptbl[n_row,n_col]<-1
          selection_inf[i,3]<-1
          mptbl_event_fill(selection_inf)
        }else{
          if (selection_inf[i,3]==1 ) {
            local_mptbl[n_row,n_col]<-0
            selection_inf[i,3]<-0
            mptbl_event_fill(selection_inf)
          }
        }
      }
      # mapping_table <<- local_mptbl
      local_mptbl<-NULL
    },

    #填充事件
    mptbl_event_fill= function(fill_inf) {
      # browser()
      local_mptbl<-mapping_table
      select_row_fist<-fill_inf[1,1]
      select_row_end<-fill_inf[nrow(fill_inf),1]
      select_col<-as.integer(c(fill_inf[1,2]))
      select_mptal_range<-local_mptbl[select_row_fist:select_row_end,,drop=F]
      nr<-nrow(select_mptal_range)
      nc<-ncol(select_mptal_range)
      if (fill_inf[1,3]==0){
        select_mptal_range[,select_col]<-rep(0,nrow(select_mptal_range))
        local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
        mapping_table <<- local_mptbl
        local_mptbl<-NULL
        # mapping_table<<-mptbl_select_default_field(1)
        mptbl_select_default_field(default_field)
      }
      else
      {
        select_mptal_range[]<-matrix(rep(0,nr*nc),nrow=nr,ncol=nc)
        select_mptal_range[,select_col]<-rep(1,nr)
        local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
        mapping_table <<- local_mptbl
      }
    }
  )
)

#多选类
Multiple_Mptbl<-setRefClass("Multiple_Mptbl",
                             contains="Single_Mptbl",
                             methods=list(
                               #选中事件
                               mptbl_event_select= function(selection_inf) {
                                 # browser()
                                 aaa<-selection_inf
                                 local_mptbl<-mapping_table
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
                                 mapping_table <<- local_mptbl
                                 local_mptbl<-NULL
                               },

                               #填充事件
                               mptbl_event_fill= function(fill_inf) {
                                 # browser()
                                 local_mptbl<-mapping_table
                                 select_row_fist<-fill_inf[1,1]
                                 select_row_end<-fill_inf[nrow(fill_inf),1]
                                 select_col<-as.integer(c(fill_inf[1,2]))
                                 select_mptal_range<-local_mptbl[select_row_fist:select_row_end,,drop=F]
                                 nr<-nrow(select_mptal_range)
                                 nc<-ncol(select_mptal_range)
                                 if (fill_inf[1,3]==0){
                                   select_mptal_range[,select_col]<-rep(0,nrow(select_mptal_range))
                                   local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
                                   mapping_table <<- local_mptbl
                                   local_mptbl<-NULL
                                   mptbl_select_default_field(default_field)
                                 }
                                 else
                                 {
                                   # select_mptal_range[]<-matrix(rep(0,nr*nc),nrow=nr,ncol=nc)
                                   select_mptal_range[,select_col]<-rep(1,nr)
                                   local_mptbl[select_row_fist:select_row_end,]<-select_mptal_range
                                   mapping_table <<- local_mptbl
                                 }
                               }
                             )
                             )

#调度类，用于接收数据，并调用多选类和单选类处理数据
Mapping_Table<-setRefClass("Mapping_Table",
                           #contains=父类,
                           fields=list(
                             field_groups="matrix",
                             variable="vector",
                             default_field="numeric",
                             #field_group="vector",
                             preselection_field="matrix",
                             # selection_inf="matrix",
                             # fill_inf="matrix",
                             field_index_transform="matrix",
                             mapping_table="matrix",
                             inf_of_mptbl="matrix",
                             field_right_bound="vector",
                             DT_container="character"
                           ),
                           methods=list(
                             #生成映射列表
                             create_mptbl= function() {
                               nr<-length(variable)
                               nc<-length(field_groups[1,])

                               local_mptbl<-matrix(as.integer(rep(0,nc*nr)),nrow=nr,ncol=nc, byrow = TRUE)
                               colnames(local_mptbl)<-field_groups[1,]
                               rownames(local_mptbl)<-variable

                               #给mapping_table赋值
                               mapping_table <<- local_mptbl
                               local_mptbl<-NULL

                               #选中默认选项
                               mptbl_select_default_field(default_field)

                               #给field_right_bound赋值
                               set_field_right_bound(field_groups)
                               #给DT_container赋值
                               set_DT_container(field_groups)
                             },

                             #分别配字段通过字段组#该方法暂时没有用
                             assign_field_by_field_group=function() {
                               field_value<-field_groups[1,]
                               field_group<-as.integer(c(field_groups[2,]))
                               field_display_group<-field_groups[3,]
                               group<-unique(field_group)

                               for (i in group){
                                 if (i%%2 ==0){
                                   group_name<-paste("multiple",i,sep ="")
                                   group_index<-c(field_group==i)
                                   assign(group_name,Multiple_Mptbl$new(mapping_table=mapping_table[,group_index,drop=F]))
                                 }else{
                                   group_name<-paste("single",i,sep ="")
                                   group_index<-c(field_group==i)
                                   assign(group_name,Multiple_Mptbl$new(mapping_table=mapping_table[,group_index,drop=F]))
                                 }
                               }
                             },

                             #选中默认选项
                             mptbl_select_default_field = function(default_field) {
                               mapping_table[,default_field]<<-1L
                             },

                             #预选事件
                             mptbl_select_preselection_field = function(preselection_field) {
                               mptbl_event_select(preselection_field)
                             },


                             #选中事件
                             mptbl_event_select= function(selection_inf) {
                               # browser()
                               for (j in 1:nrow(selection_inf)){
                                 i_num<-selection_inf[j,2]
                                 i<-as.integer(field_groups[2,i_num])
                                 if (i%%2 ==0){
                                   group_name<-paste("multiple",i,sep ="")
                                   assign(group_name,get_sub_group(group_name))
                                   selection_inf[j,2]<-field_index_transform[4,i_num]
                                   local_default_field<-as.integer(field_index_transform[4,default_field])
                                   get(group_name)$initFields(default_field=local_default_field)

                                   # browser()
                                   get(group_name)$mptbl_event_select(selection_inf[j,,drop=F])

                                   set_mptabl(group_name,get(group_name)$mapping_table)
                                 }else{
                                   group_name<-paste("single",i,sep ="")
                                   assign(group_name,get_sub_group(group_name))
                                   selection_inf[j,2]<-field_index_transform[4,i_num]
                                   local_default_field<-as.integer(field_index_transform[4,default_field])
                                   get(group_name)$initFields(default_field=local_default_field)

                                   # browser()
                                   get(group_name)$mptbl_event_select(selection_inf[j,,drop=F])

                                   set_mptabl(group_name,get(group_name)$mapping_table)
                                 }
                               }
                             },

                             #填充事件
                             mptbl_event_fill= function(fill_inf) {
                               # browser()
                                 fill_inf<-as.matrix(fill_inf,ncol=3)
                                 i_num<-fill_inf[1,2]
                                 i<-as.integer(field_groups[2,i_num])
                                 # browser()
                                 if (i%%2 ==0){
                                   group_name<-paste("multiple",i,sep ="")
                                   assign(group_name,get_sub_group(group_name))
                                   fill_inf[,2]<-field_index_transform[4,i_num]
                                   local_default_field<-as.integer(field_index_transform[4,default_field])
                                   get(group_name)$initFields(default_field=local_default_field)

                                   get(group_name)$mptbl_event_fill(fill_inf)

                                   set_mptabl(group_name,get(group_name)$mapping_table)
                                 }else{
                                   group_name<-paste("single",i,sep ="")
                                   assign(group_name,get_sub_group(group_name))
                                   fill_inf[,2]<-as.integer(field_index_transform[4,i_num])
                                   local_default_field<-as.integer(field_index_transform[4,default_field])
                                   get(group_name)$initFields(default_field=local_default_field)

                                   get(group_name)$mptbl_event_fill(fill_inf)

                                   set_mptabl(group_name,get(group_name)$mapping_table)
                                 }
                             },

                             #内部方法——从mapping_table获取子表格
                             get_sub_group=function(group_name) {
                               # field_value<-field_groups[1,]
                               field_group<-as.integer(c(field_groups[2,]))
                               # field_display_group<-field_groups[3,]
                               aaab<-rbind(field_groups,NA)
                               field_index_transform<<-aaab

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
                                 assign(group_name,Multiple_Mptbl$new(mapping_table=mapping_table[,group_index,drop=F]))
                                 field_index_transform[4,group_index]<<-c(1:sum(group_index))
                               }else{
                                 #single
                                 group_index<-c(field_group==i)
                                 assign(group_name,Single_Mptbl$new(mapping_table=mapping_table[,group_index,drop=F]))
                                 field_index_transform[4,group_index]<<-c(1:sum(group_index))
                               }
                               get(group_name)
                             },

                             #内部方法——将子表格的内容赋值给mapping_table
                             set_mptabl = function(group_name,mptbl) {
                               # field_value<-field_groups[1,]
                               field_group<-as.integer(c(field_groups[2,]))
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
                               mapping_table[,group_index]<<-mptbl
                               mptbl_to_inf(mapping_table)
                             },

                             #内部方法——将mapping_table转换为inf格式
                             mptbl_to_inf= function(mapping_table) {
                               # browser()
                               nr<-nrow(mapping_table)
                               nc<-ncol(mapping_table)
                               m<-matrix(as.numeric(rep(0,nr*nc*3)),nrow=nr*nc,ncol=3, byrow = TRUE)

                               colnames(m)<-c("row","col","value")
                               k<-1
                               for (i in seq_len(nr)) {
                                 for (j in seq_len(nc)) {
                                   m[k,1] = i
                                   m[k,2] = j
                                   m[k,3] = mapping_table[i,j]
                                   k<-k+1
                                 }
                               }
                               #给inf_of_mptbl赋值
                               inf_of_mptbl<<-m
                             },

                             #内部方法——将field_groups转换为field_right_bound
                             set_field_right_bound = function(field_groups) {
                               # field_value<-field_groups[1,]
                               # field_group<-as.integer(c(field_groups[2,]))
                               field_display_group<-field_groups[3,]
                               v<-c()
                               for (i in 1: (length(field_display_group)-1)){
                                 if (field_display_group[i]!=field_display_group[i+1]){
                                   v<-c(v,i)
                                 }
                               }
                               field_right_bound<<-v
                             },

                             #内部方法——将field_groups转换为DT_container
                             set_DT_container = function(field_groups) {
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
                               DT_container<<-container_value
                             }


                           )
)


