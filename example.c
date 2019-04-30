
int main(){
	int f1=1, f2=1;
	int count=0;
	

	// print(f1);
	// print(gcd(13340,2412320));
	// print(factorial(12));
	return 0;
}

int gcd (int a,int b){
	print(a);
	print(b);
	if(a!=0){
		return gcd(b%a,a);
	}else{
		return b;
	}
}

