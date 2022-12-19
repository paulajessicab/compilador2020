; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @pcfmain()    {
entry0:
  %__r_1 = inttoptr i64 10 to i64* 
  %__r_2 = ptrtoint i64* %__r_1 to i64 
  %__r_3 = inttoptr i64 %__r_2 to i64* 
  store  i64* %__r_3, i64** @b 
  %__r_4 = load  i64*, i64** @b 
  %__r_5 = ptrtoint i64* %__r_4 to i64 
  %__r_6 =  call ccc  i64  @pcf_print(i64  %__r_5)  
  %_r0 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = inttoptr i64 0 to i64* 
  %__r_8 = ptrtoint i64* %__r_7 to i64 
  %_r1 = inttoptr i64 %__r_8 to i64* 
  %__r_10 = inttoptr i64 0 to i64* 
  %cond9 = icmp eq i64* %__r_10, %_r1 
  br i1 %cond9, label %then4, label %else4 
then4:
  %__r_11 = inttoptr i64 5 to i64* 
  %__r_12 = ptrtoint i64* %__r_11 to i64 
  %_r2 = inttoptr i64 %__r_12 to i64* 
  br label %cont4 
else4:
  %__r_13 = inttoptr i64 3 to i64* 
  %__r_14 = ptrtoint i64* %__r_13 to i64 
  %_r3 = inttoptr i64 %__r_14 to i64* 
  br label %cont4 
cont4:
  %_r10 = phi i64* [%_r2, %then4], [%_r3, %else4] 
  %__r_15 = inttoptr i64 3 to i64* 
  %__r_16 = ptrtoint i64* %__r_15 to i64 
  %_r11 = inttoptr i64 %__r_16 to i64* 
  %__r_17 = inttoptr i64 10 to i64* 
  %__r_18 = ptrtoint i64* %__r_17 to i64 
  %_r8 = inttoptr i64 %__r_18 to i64* 
  %__r_19 = ptrtoint i64* %_r10 to i64 
  %__r_20 = ptrtoint i64* %_r11 to i64 
  %__r_21 = add   i64 %__r_19, %__r_20 
  %_r9 = inttoptr i64 %__r_21 to i64* 
  %__r_22 = ptrtoint i64* %_r8 to i64 
  %__r_23 = ptrtoint i64* %_r9 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %_r6 = inttoptr i64 %__r_24 to i64* 
  %__r_25 = load  i64*, i64** @b 
  %__r_26 = ptrtoint i64* %__r_25 to i64 
  %_r7 = inttoptr i64 %__r_26 to i64* 
  %__r_27 = load  i64*, i64** @mult 
  %__r_28 = ptrtoint i64* %__r_27 to i64 
  %_r13 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %_r13 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %_r12 = load  i64*, i64** %addr29 
  %__r_31 = load  i64*, i64** @mult 
  %__r_32 = ptrtoint i64* %__r_31 to i64 
  %_r14 = inttoptr i64 %__r_32 to i64* 
  %__r_33 = inttoptr i64 4 to i64* 
  %__r_34 = ptrtoint i64* %__r_33 to i64 
  %_r15 = inttoptr i64 %__r_34 to i64* 
  %fun35 = bitcast i64* %_r12 to i64* (i64*, i64*)* 
  %_r17 =  call ccc  i64*  %fun35(i64*  %_r14, i64*  %_r15)  
  %__r_37 = bitcast i64* %_r17 to i64** 
  %addr36 = getelementptr  i64*, i64** %__r_37, i64 0 
  %_r16 = load  i64*, i64** %addr36 
  %fun38 = bitcast i64* %_r12 to i64* (i64*, i64*)* 
  %_r18 =  call ccc  i64*  %fun38(i64*  %_r14, i64*  %_r15)  
  %__r_39 = inttoptr i64 5 to i64* 
  %__r_40 = ptrtoint i64* %__r_39 to i64 
  %_r19 = inttoptr i64 %__r_40 to i64* 
  %__r_41 = ptrtoint i64* %_r6 to i64 
  %__r_42 = ptrtoint i64* %_r7 to i64 
  %__r_43 = add   i64 %__r_41, %__r_42 
  %_r4 = inttoptr i64 %__r_43 to i64* 
  %fun44 = bitcast i64* %_r16 to i64* (i64*, i64*)* 
  %_r5 =  call ccc  i64*  %fun44(i64*  %_r18, i64*  %_r19)  
  %__r_45 = ptrtoint i64* %_r4 to i64 
  %__r_46 = ptrtoint i64* %_r5 to i64 
  %__r_47 = add   i64 %__r_45, %__r_46 
  %__r_48 = inttoptr i64 %__r_47 to i64* 
  store  i64* %__r_48, i64** @a 
  %__r_49 = load  i64*, i64** @a 
  %__r_50 = ptrtoint i64* %__r_49 to i64 
  %__r_51 =  call ccc  i64  @pcf_print(i64  %__r_50)  
  %_r20 = inttoptr i64 %__r_51 to i64* 
  %__r_52 = inttoptr i64 0 to i64* 
  ret i64* %__r_52 
}


@b = internal   global i64* zeroinitializer


@a = internal   global i64* zeroinitializer