//##一些基础的数组相关函数
//求两个数组的交集元素,并且取的元素即去重复
var tbl = $(table.table().node());
var id = tbl.closest('.datatables').attr('id');
console.log('#############');
console.log(tbl);
console.log(id);
console.log(table);
var intersection = function (nums1, nums2) {
	// 集合Set 实现方式
	// return [...new Set(nums1)].filter(item => nums2.includes(item))
	
	const map = new Map();
	nums1.forEach(n => {
		map.set(n, true)
	})
	const res = [];
	nums2.forEach(n => {
		if (map.get(n)) {
			res.push(n);
			map.delete(n);
		}
	})
	return res
};

//筛选函数，对一个一维数组的下标进行筛选，并返回数值
function vfilter(array,value) {
	zz=Array.from(array.entries())
	zzf=zz.filter((item,index,arr)=>value.includes(item[0]))
	zzfc=zzf.map(function(value,index) { return value[1]; });
	return zzfc;
}

//筛选函数，对一个一维数组的数值进行筛选，并返回下标
function nfilter(array,value) {
	zz=Array.from(array.entries())
	zzf=zz.filter((item,index,arr)=>item[1]==value)
	zzfc=zzf.map(function(value,index) { return value[0]; });
	return zzfc;
}

//dt表格单元格数值更新函数
function dt_edit_cell(dt,row,column,newValue) {
	ls_i= dt.cells()[0].find(o=> o.row===row&o.column==column)
	dt.cell(ls_i).data(newValue)// dt.cells()[0].find(o=> o.row===row&o.column==column)
}

//整数向量数组生成函数
function n2n (start, end) { 
	var aa=[]
	if (start>=end){
		for (var i=end;i<=start;i++){
			aa.unshift(i)//逐个向前增加元素
		}
	} else if ( start < end ) {
		for (var i=start;i<=end;i++){
			aa.push(i) //逐个向后增加元素
		}
	}
	return aa
}

table.column(1).search(1).draw()
table.column(1).visible(false)

delete $.fn.dataTable.AutoFill.actions.increment;
delete $.fn.dataTable.AutoFill.actions.fillHorizontal;
delete $.fn.dataTable.AutoFill.actions.fillVertical;
delete $.fn.dataTable.AutoFill.actions.fill;

DataTable.AutoFill.actions.names = {
	available: function (dt, cells) {

		const lscells =cells //下拉拖拽选中范围内的单元个
		const lsdt = Array.from(dt.data());//DT对象

		const sf = lscells[0][0]
		const sfr = sf.index.row //选中的起始单元格行号：整数
		const sfc = sf.index.column //选中的起始单元格列号：整数
		const sfd = sf.data //选中的起始单元格值：文本   

		const sl = lscells[lscells.length-1][lscells[0].length-1]
		const slr = sl.index.row //选中的终止单元格行号：整数
		const slc = sl.index.column //选中的终止单元格列号：整数
		const sld = sl.data //选中的终止单元格值：文本

		// console.log("##########Auto");
		// console.log("dt");
		// console.log(dt);
		console.log("DataTable.AutoFill.actions.names ## available ## lsdt");
		console.log(lsdt);
		let = Rrowset = lsdt[2] //通过读取表格中第1行，获取获取行组设置
		let = Rcolset = lsdt[3] //通过读取表格中第2行，获取获取列单选设置
		let = Rintset = lsdt[4] //通过读取表格中第3行，获取获取初始设置选中，一般是None
		let = Rcolhide = lsdt[5] //通过读取表格中第4行，获取获取列隐藏设置选中
		function get_dt_lsdt_sr(dt) {
			for (let i=0;i<dt.length;i++){
				if (dt[i][1]==1){
					return i;
				}
			}
		}
		const lsdt_sr = get_dt_lsdt_sr(lsdt);
		const lsdt_sc = Math.min(...nfilter(Rcolhide,1));
		// return true// cells[0].length === 1 && cells[0][0].index.column === 0;
		let Flag = sfr>=lsdt_sr && sfc>=lsdt_sc && slr>=lsdt_sr && slc>=lsdt_sc
		return Flag

	},
 
	option: function (dt, cells) {
		// Ask the user if they want to change the surname only
		return 'Fill only surname - retain first name';
	},
 
	execute: function (dt, cells, node) {
	//#1 开始
	//###############

	//#2、3、4、5、6 获取拖拽信息
	//###############
		const lscells =cells //下拉拖拽选中范围内的单元个
		// const lsdt=dt.data() //DT对象
		const lsdt = Array.from(dt.data());//DT对象

		const lsdt_r=dt.data().length //总行数
		const lsdt_c=dt.data()[0].length //总列数
		const lscells_r=cells.length //选中范围内的总行数
		const lscells_c=cells[0].length //选中范围内的总列数

		let = Rrowset = lsdt[2] //通过读取表格中第1行，获取获取行组设置
		let = Rcolset = lsdt[3] //通过读取表格中第2行，获取获取列单选设置
		let = Rintset = lsdt[4] //通过读取表格中第3行，获取获取初始设置选中，一般是None
		let = Rcolhide = lsdt[5] //通过读取表格中第4行，获取获取列隐藏设置选中
		function get_dt_lsdt_sr(dt) {
			for (let i=0;i<dt.length;i++){
				if (dt[i][1]==1){
					return i;
				}
			}
		}
		const lsdt_sr = get_dt_lsdt_sr(lsdt);//hide列中上往下数，第一个不隐藏的行
		const lsdt_sc = Math.min(...nfilter(Rcolhide,1));//Rcolhide行，从左到右数，第一个不隐藏的列

		const lsdt_intset =Math.max(...nfilter(Rintset,1),lsdt_sc)//在Rintset，从左到右数，第一个等于1的列；然后与lsdt_sc，取其中最大的，以此确保默认列是可显示的。
			
		const sf = lscells[0][0]
		const sfr = sf.index.row //选中的起始单元格行号：整数
		const sfc = sf.index.column //选中的起始单元格列号：整数
		const sfd = sf.data //选中的起始单元格值：文本      

		const sl = lscells[lscells.length-1][lscells[0].length-1]
		const slr = sl.index.row //选中的终止单元格行号：整数
		const slc = sl.index.column //选中的终止单元格列号：整数
		const sld = sl.data //选中的终止单元格值：文本

		const sxr =  Math.min(sfr,slr)//选中的左上行号
		const sxc =  Math.min(sfc,slc)//选中的左上列号
		const sdr =  Math.max(sfr,slr)//选中的右下行号
		const sdc =  Math.max(sfc,slc)//选中的右下列号

		const lsdt_ris=n2n(0,lsdt_r) //总行号下标数组
		const lsdt_cis=n2n(0,lsdt_c)//总列号下标数组
		const lscells_ris=n2n(sxr,sdr) //选中范围内的行号下标数组
		const lscells_cis=n2n(sxc,sdc)//选中范围内的列号下标数组     

		//识别选中时操作的的水平拖拽方向
		let rowTo =0
		if (sfc>slc) {
			rowTo = -1  //To Right 从左向右拖拽 max
		} else if(sfc<slc){
			rowTo = 1 //To Left 从左向右拖拽 min
		}

		//识别选中时操作的的水平拖拽方向
		let colTo =0
		if (sfr>slr) {
			colTo = -1  //To top 从下向上拖拽 max
		} else if(sfr<slr){
			colTo = 1 //To dowm 从上向下拖拽 min
		}

	// 进行逐行循环,由选中区域最上方的行号，向下方行号循环#7、8
	//###############
	for (var p_ri=sxr;p_ri<sdr+1;p_ri++){ 

		if (colTo==-1){//用于解决一个潜在的bug，此bug导致当行的规则应用为从上至下时，而拖拽方向为由右下至左上时，单选列与单选组嵌套的情况会先bug，表现为单选组都被选中。
			ri=sdr-p_ri+sxr
		}else {
			ri=p_ri
		}

		lsdt_row_i = lsdt[ri]
		//#7 将填充范围全部填充
		//###############
			lsdt_row_i = lsdt_row_i.fill(sfd,sxc,sdc+1);
			

		//#8 基于行规则+【起始单元格】值、方向+上一步执行的结果：判断所有单元格应该的值
		//###############
		if (sfd==1){ // 如果起始单元格值为1，即要填充1，则执行行规则，否则不执行行规则

			//#8 列规则执行
			if (sfd==1){// 如果起始单元格值为1，即要填充1，则执行列规则，否则不执行列规则
				let column_nis=nfilter(Rcolset,1) //获取设置为单选列的列号
				let af_sc=intersection(column_nis,lscells_cis)//获取'设置为单选列的列号'与选中区域列号的交集

				af_sc.forEach((item,index,array)=> {//遍历所有单选列
					let col_ni = item
							lsdt.forEach((item,index,array)=>{
								if (index>=lsdt_sr) {
									item[col_ni]="0"
								}
							});//全部填充为0
							lsdt[slr][col_ni]="1"
				})
			}
			//列规则执行

			//进行行规则
			let mapkey = []
			mapkey = intersection(Rrowset,Rrowset)//用于去重复行单选设置组别号
			mapkey = mapkey.filter(item=>(item%2)==1)//获取组别号中的奇数号
			//遍历所有单选，并根据从左向右还是从右向左拖拽，设置与填充，行单选的单元格
			mapkey.forEach(item => {
				let af=nfilter(Rrowset,item) //item是可能的行单选设置分组，af是此分组对应的列号
				let af_sc=intersection(af,lscells_cis)//计算item与当选选中的的区域的重叠区域，是否存在重叠？
				if (af_sc.length>0){//不重叠就不执行行规则，重叠则
					let ls=vfilter(lsdt[ri ],af)//获取单选值数组
					let sumvalue=ls.reduce((p,r,i,a)=>Number(p)+Number(r))
					if (!((sumvalue==0)||(sumvalue==1))) { //如果此单选分组中的选中项大于1个，则执行行规则
						let af_max_c = Math.max(...af)
						let af_min_c = Math.min(...af)
						if (rowTo==-1){//从右至左,则
							lsdt_row_i.fill("0",af_min_c,af_max_c+1);//全部填充为0
							lsdt_row_i.fill("1",Math.max(sxc,af_min_c),Math.max(sxc,af_min_c)+1)
							//识别出应当填充1的列号，即选出的'选区中最小列号'、'单选组最小列号'中'最大'的列号
						}
						else if(rowTo==1){//从左至右，则
							lsdt_row_i.fill("0",af_min_c,af_max_c+1);//全部填充为0
							lsdt_row_i.fill("1",Math.min(sdc,af_max_c),Math.min(sdc,af_max_c)+1)
							//识别出应当填充1的列号，即选出的'选区中最大列号'、'单选组最大列号'中'最小'的列号
						}else if(rowTo==1){///仅推拽一列数据，则rowTo=0,则
							lsdt_row_i.fill("0",af_min_c,af_max_c+1);//全部填充为0
							lsdt_row_i.fill("1",sfc,sfc+1);//之后在将拖拽的列设为1
						}
					}
				}
			})
			// 更新dt中单元格的值
			// console.log('dt.data()');
			// console.log(dt.data());
			// console.log('#############');
		}
	}

	// 逐行检测是否选中的列为0，如果是则选中默认起始列
	for (i = lsdt_sr; i < lsdt_r; i++) { 
		// let lsi =lsdt[i].slice(lsdt_sc).reduce((prev,cur)=> parseInt(prev)+parseInt(cur))
		if(lsdt[i].slice(lsdt_sc).reduce((prev,cur)=> Number(prev)+Number(cur))==0){lsdt[i].fill("1",lsdt_intset,lsdt_intset+1)}
	 }

		//#9 执行写入，将经过逻辑处理后的数据写入表格中以便可被用于显示
		//###############
		// 更新dt中单元格的值
		// console.log('dt.data() befer Edit');
		// console.log(dt.data());
		// console.log('#############');

		// for (var ri=sxr;ri<sdr+1;ri++){ 
		for (var ri=0;ri<lsdt_r;ri++){   
			for (var ci=0;ci<lsdt_c;ci++){
				dt_edit_cell(dt,ri,ci,lsdt[ri][ci])
			}
		}
		
		//#10 将更新后的数据传输给Shiny
		let id = $(dt.table().node()).closest('.datatables').attr('id')
		var lsdt01=JSON.stringify(lsdt)
		Shiny.setInputValue(id + '_table', {lsdt01});

		// 将整个个映射表格中，数值区域的内容传递给Shiny
		let lsdt_mp=lsdt.slice(lsdt_sr).map((item,index,array)=>item.slice(lsdt_sc))
		var lsdt_mp01=JSON.stringify(lsdt_mp)
		Shiny.setInputValue(id + '_table_mp', {lsdt_mp01});

		// 将整个个映射表格中，行名称区域的内容传递给Shiny
		let lsdt_mp_rowName=lsdt.slice(lsdt_sr).map((item,index,array)=>item.slice(0,1))
		var lsdt_mp_rowName01=JSON.stringify(lsdt_mp_rowName)
		Shiny.setInputValue(id + '_table_mp_rowName', {lsdt_mp_rowName01});

		// console.log("DataTable.AutoFill.actions.names ## execute ## lsdt");
		// console.log(lsdt);
		
		//#10 结束
		//###############
		// console.log('dt.data()');
		// console.log(dt.data());
		// console.log('#############');

	},
};

table.on( 'click', 'td', function () {
	// var table = $('#example').DataTable();  
	var table = $(['#',this.offsetParent.id].join('')).DataTable();
	let id = this.offsetParent.offsetParent.closest('.datatables').id;
	// var tt = this.offsetParent
	const lsdt = Array.from(table.data());
	const lsdt_r=table.data().length //总行数
	const lsdt_c=table.data()[0].length //总列数
	// var Rrowset = [1,1,0,0,3,3]
	// var Rcolset = [0,1,0,1,0,1]
	let cell = table.cell( this );
	let sf = table.cell( this )[0][0]
	let sfr = sf.row //选中的起始单元格行号：整数
	let sfc = sf.column //选中的起始单元格列号：整数
	let sfd = cell.data() //选中的起始单元格值：文本 
	let sfdn = Number(sfd) //选中的起始单元格值：文本 

	// const lsdt_sr = 2
	// const lsdt_sc = -1

	let = Rrowset = lsdt[2] //通过读取表格中第1行，获取获取行组设置
	let = Rcolset = lsdt[3] //通过读取表格中第2行，获取获取列单选设置
	let = Rintset = lsdt[4] //通过读取表格中第3行，获取获取初始设置选中，一般是None
	let = Rcolhide = lsdt[5] //通过读取表格中第4行，获取获取列隐藏设置选中

	function get_dt_lsdt_sr(dt) {
		for (let i=0;i<dt.length;i++){
			if (dt[i][1]==1){
				return i;
			}
		}
	}
	const lsdt_sr = get_dt_lsdt_sr(lsdt); //hide列中上往下数，第一个不隐藏的行
	const lsdt_sc = Math.min(...nfilter(Rcolhide,1));//Rcolhide行，从左到右数，第一个不隐藏的列

	const lsdt_intset =Math.max(...nfilter(Rintset,1),lsdt_sc)//在Rintset，从左到右数，第一个等于1的列；然后与lsdt_sc，取其中最大的，以此确保默认列是可显示的。

	if(sfr>=lsdt_sr&&sfc>=lsdt_sc){    // 检测是否是起始行与起始列后方的内容
		if (sfdn==0){
			// cell.data(1)
			//行规则
				let mapkey = []
				mapkey = intersection(Rrowset,Rrowset)//用于去重复行单选设置组别号
				mapkey = mapkey.filter(item=>(item%2)==1)//获取组别号中的奇数号
				mapkey.forEach(item => {//遍历所有奇数的单选组
					let af=nfilter(Rrowset,item) //获取单选组中包含的列的列号
					let af_sc1 = af.includes(sfc)//选中的列号是否在列号中

					if (af_sc1){    
						let af_max_c = Math.max(...af)//单选组中列号最大的下标
						let af_min_c = Math.min(...af)//  单选组中列号最小的下标 
						lsdt[sfr].fill("0",af_min_c,af_max_c+1);//将单选组全部填充为0
					}
					// cell.data(1)//将选中的单元格填充为1
				})  
			//列规则
				let column_nis=nfilter(Rcolset,1) //获取设置为单选列的列号
				let af_sc2 = column_nis.includes(sfc)
				if (af_sc2){    
					lsdt.forEach((item,index,array)=>{
						if (index>=lsdt_sr) {
							item[sfc]="0"
						}
					}
						
						);//全部填充为0
					// lsdt[sfr][sfc]=1
				}
				cell.data("1")//将选中的单元格填充为1
		}else {
			cell.data("0")//将选中的单元格填充为0
		}
	}

	// 逐行检测是否选中的列为0，如果是则选中默认起始列
	for (i = lsdt_sr; i < lsdt_r; i++) { 
		// let lsi =lsdt[i].slice(lsdt_sc).reduce((prev,cur)=> parseInt(prev)+parseInt(cur))
		if(lsdt[i].slice(lsdt_sc).reduce((prev,cur)=> Number(prev)+Number(cur))==0){lsdt[i].fill("1",lsdt_intset,lsdt_intset+1)}
	 }

	//写入数据
	for (var ri=0;ri<lsdt_r;ri++){   
		for (var ci=0;ci<lsdt_c;ci++){
			ls_i=lsdt[ri][ci]
			ls_t=table
			dt_edit_cell(table,ri,ci,ls_i)
		}
	};
	
	// 内容传递给Shiny
	var lsdt01=JSON.stringify(lsdt)
	Shiny.setInputValue(id + '_table', {lsdt01});
	
	// 将整个个映射表格中，数值区域的内容传递给Shiny
	let lsdt_mp=lsdt.slice(lsdt_sr).map((item,index,array)=>item.slice(lsdt_sc))
	var lsdt_mp01=JSON.stringify(lsdt_mp)
	Shiny.setInputValue(id + '_table_mp', {lsdt_mp01});
	
	// 将整个个映射表格中，行名称区域的内容传递给Shiny
	let lsdt_mp_rowName=lsdt.slice(lsdt_sr).map((item,index,array)=>item.slice(0,1))
	var lsdt_mp_rowName01=JSON.stringify(lsdt_mp_rowName)
	Shiny.setInputValue(id + '_table_mp_rowName', {lsdt_mp_rowName01});
	
	// console.log("table.on click ## lsdt");
	// console.log(lsdt);
} );