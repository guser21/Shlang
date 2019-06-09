int main(){
  fun(int->int) rscope=recScope();
  print(rscope(10));
  return 0;
}
fun(int->int) recScope(){
  fun(int ->int) fac = lambda(int a) :int {
    if(a==0) return 1;
    return a*fac(a-1);
  };
  fun(int ->int) rfac=fac;
  fac=lambda(int a) :int {
    return a+1;
  };
  return rfac;
}




  // int d(int a){
  //   return 0;
  // }
  // int f(int a){
  //   if(a==0) return 1;
  //   // print("hi");
  //   return f(a-1) * a;
  // }

  // fun(int->int) g=f;
  // print(g(10));