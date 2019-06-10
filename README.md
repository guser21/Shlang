# Shlang

### Take your Shlang in your hands!
<img src="shlang.png" width="250">
Features:

* Types: void, int, bool, string, fun(type1,type2,type3,... -> res_type ).

* Static typechecking 

* Arithmetics, assginement, variables and all basic features of any imperative languge.

* While break continue.

* Conditional branching: if , else statements. 

* for loops with constant iterator. 

* Functions with recursion. Nested functions with recursion. 

* Overshadowing variables 

* Runtime exceptions.

* Static typechecking. Type errors. 

* Final variables. 

* Lambdas, recursive lambdas, closures, functional variables etc.


### Examples: 

Ex. 1 
```
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

```
Ex. 2
```
int main(){
    int counter=0; 

    int innerCounter=0;
    int outerCounter=0;

    while(counter<1000){
        outerCounter++;
        counter++;
        
        while (true){
            innerCounter++;
            counter++;
            if(counter%10 == 0){
                break;
            }        
        }
        continue;
    }
    
    print(outerCounter);
    print(innerCounter);

    return 0;
}
```

Ex. 3

```
final int a =12;
int main(){
    int fib(int n){
        if(n==0) return 1;
        if(n==1) return 1;
        return fib(n-1)+fib(n-2);
    }
    for(int i=0 to a+1){
        print(fib(i));
    }
    return 0;
}
```

