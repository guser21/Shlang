int main()
{
	finalTest(21);
	bcTest();
	bcComplex();
	overshadow();
	return 0;
}

final int finA=12;
void finalTest(){
	int finA=12;
	finA--;
	
}

void bcTest(){
	int a=0;
	int outerCount=0,innerCount;
	while(a<100){
		outerCount++;
		a++;
		while(a<100){
			innerCount++;
			a++;
			if(a%3==0){
				break;
			}
		}
	}
	if(outerCount!=34){
		print("ERROR 34");
	}
	if(innerCount!=66){
		print("ERROR 66");
	}
}


int bcComplex(){
	int x;
	
	x = 0;
	while(true)
		break;
	while(true) {
		if (x == 5) {
			break;
		}
		x = x + 1;
		continue;
	}
	while(true){
		if (x == 10) {
			break;
		}
		x = x + 1;
		continue;
	}
	while(true) {
		if (x == 15) {
			break;
		}
		x = x + 1;
		continue;
	} 
	return x - 15;
}

void overshadow(){
	int a=1;
	{
		string a="12";
		a=a+"hey";
	}
	{
		final int a=12;
	}
	a++;
	if(a!=2){
		print("ERROR")
	}
}

