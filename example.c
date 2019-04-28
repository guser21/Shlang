
int main(){
	int f1=1, f2=1;
	int count=0;
	print(gcd(13340,2412320));
	return 0;
}

int gcd (int a,int b){
	if(a!=0){
		return gcd(b%a,a);
	}else{
		return b;
	}
}
