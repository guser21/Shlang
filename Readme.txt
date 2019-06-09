Wszystkie podstawowe funkcjonalności do 20 punktów
Do tego
+ Typechecker
+ dowolnie zagnieżdżone definicje funkcji 
+ break, continue
+ słowo kluczowe final
+ leniwość operatorów OR AND
+ wykrywa stackoverflow


Konflikty:
1 shift/reduce z ifem
	if(one)
		if(two)
			smth();			
		esle 
			otherThing();


Edit: Wersja 2

+ funkcje jako parametry,zwracanie funkcji w wyniku, domknięcia à la JavaScript. funkcje anonimowe
+ poprawiłem statyczną widoczność 
+ poprawiłem komunikaty z Typecheckera

