; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @pcfmain()    {
entry0:
  %__r_1 = inttoptr i64 18 to i64* 
  %__r_2 = ptrtoint i64* %__r_1 to i64 
  %__r_3 = inttoptr i64 %__r_2 to i64* 
  store  i64* %__r_3, i64** @resta 
  %__r_4 = load  i64*, i64** @resta 
  %__r_5 = ptrtoint i64* %__r_4 to i64 
  %__r_6 =  call ccc  i64  @pcf_print(i64  %__r_5)  
  %_r0 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = inttoptr i64 0 to i64* 
  ret i64* %__r_7 
}


@resta = internal   global i64* zeroinitializer