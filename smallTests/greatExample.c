int main(){
  fun(int ->int) fac = lambda(int a) :int {
    if(a==0) return 1;
    return a*fac(a-1);
  };
  fun(int ->int) rfac=fac;
  fac=lambda(int a) :int {
    return a+1;
  };
  print(rfac(10)); //should print 100

  return 0;
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