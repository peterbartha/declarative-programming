#include "cekla.h"

/*
	intToConvertedList(A, N, L) == R, ahol R egy olyan lista, amelynek
	elemei az A szám N-alapú felírásából származó számjegyei sorrendben.
	(A>0 egész, N≥2 egész, L akkumulátor lista (inicializálás L=nil -el))
*/
list intToConvertedList(const int A, const int N, const list L) {
	if (A == 0) return L;
	return intToConvertedList(A/N, N, cons(A%N, L));
}

/*
	addStartZeros(L, N) = Az L lista elejéhez N darab 0-át fűz.
	(L egy lista, N≥0 egész)
*/
list addStartZeros(const list L, const int N) {
	if (N == 0) return L;
	return addStartZeros(cons(0, L), N-1);
}

/*
	listLength(L, N) = A lista hosszát adja vissza N eltolással.
	(L egy lista, N egész szám (praktikusan N=0))
*/
int listLength(const list L, const int N) {
	if (L == nil) return N;
	return listLength(tl(L), N+1);
}

/*
	compareLists(A, B, R) = eredménye 1, ha A és B lista elemei és azok
	sorrendje megegyezik, egyébként 0.
	(A lista, B lista, R boolean (inicializálás R=1-el))
*/
int compareLists(const list A, const list B, const int R) {
	if (listLength(A, 0) != listLength(B, 0)) return 0;
	if (A == nil) return R;
	return compareLists(tl(A), tl(B), (hd(A) == hd(B)) * R);
}

/*
	revapp(L, L0) = az L lista megfordítása L0 lista elé fűzve
	(L lista, L0 akkumulátor lista (inicializálás L0=nil -el))
*/
list revapp(const list L, const list L0) {
  if (L == nil) return L0;
  return revapp(tl(L), cons(hd(L), L0));
}

/*
	reverse(L) = eredménye az L lista megfordítva (revapp wrapper)
	O(N) (L lista)
*/
list reverse(const list L) {
  return revapp(L, nil);
}

/*
	createDissimilarityList(A, B, DL) = eredménye a KL különbözőségi lista
	egy k hosszúságú lista, amelynek i-edik eleme 0 ha Ai=Bi, és 1 egyébként.
	(A lista, B lista, DL különbözőségi lista (inicializálás: Dl=nil -el))
*/
int createDissimilarityList(const list A, const list B, const list DL) {
	if (A == nil) return reverse(DL); // itt már A és B lista mérete ua.
	return createDissimilarityList(tl(A), tl(B), cons(hd(A) != hd(B), DL));	
}

/*
	Definíció: Jelöljük k-val a max(A,B) szám N alapú számrendszerben való
	felírásához szükséges számjegyek számát. Írjuk fel az A és B számokat
	N alapú számrendszerben! A kapott számjegyeket jelölje rendre A1, ..., Ak
	és B1, ..., Bk, ahol a kisebbik számot vezető nullákkal k számjegyűre
	egészítjük ki, ha k-nál kevesebb számjegyből áll. A KL különbözőségi lista
	egy k hosszúságú lista, amelynek i-edik eleme 0 ha Ai=Bi, és 1 egyébként.

	klista(A, B, N, KL) == V, ahol V = 1, ha KL az A és B számok N alapú
	különbözőségi listája, a fenti definíció értelmében, egyébként pedig V = 0. 
	(A>0, B >0; N≥2 egész számok, KL egy lista).
*/
int klista(const int A, const int B, const int N, const list KL) {
	if (KL == nil) return 0;
	
	// A és B átírása N-alapú számrendszerbe
  	const list L1 = intToConvertedList(A, N, nil);
	const list L2 = intToConvertedList(B, N, nil);
	
	// Az N-alapú számrendszerbe átírt számok listáinak hossza
	const int lenL1 = listLength(L1, 0);
	const int lenL2 = listLength(L2, 0);
	
	// A rövidebbik listához vezető nullák fűzése, éppen annyi,
	// míg a két lista (L1 és L2) hossza meg nem egyezik (=L3)
	const int L1IsLonger = lenL1 > lenL2;
	const int d = L1IsLonger ? lenL1 - lenL2 : lenL2 - lenL1;
	const list L3 = addStartZeros(L1IsLonger ? L2 : L1, d);
	
	// A és B, N-alapú számrendszerbe átírt formáikból alkotott különbözőségi lista
	const list DL = createDissimilarityList(L1IsLonger ? L1 : L2, L3, nil);
	
	// A számított és a paraméterül kapott különbözőségi listák összehasonlítása
	return compareLists(DL, KL, 1);
}

/*
	Testesetek
*/
int main() {
	writeln("#   exp-res-->success");
	writeln("---------------------");
	
    write("1.   1 - ");
	write(klista(12, 23, 10, cons(1,cons(1,nil))));
	writeln(klista(12, 23, 10, cons(1,cons(1,nil))) == 1 ? " --> Good" : " --> Bad");

	write("2.   0 - ");
	write(klista(12, 23, 10, nil));
	writeln(klista(12, 23, 10, nil) == 0 ? " --> Good" : " --> Bad");
	
	write("3.   0 - ");
	write(klista(12, 22, 10, cons(1,cons(1,nil))));
	writeln(klista(12, 22, 10, cons(1,cons(1,nil))) == 0 ? " --> Good" : " --> Bad");
	
	write("4.   1 - ");
	write(klista(12, 22, 10, cons(1,cons(0,nil))));
	writeln(klista(12, 22, 10, cons(1,cons(0,nil))) == 1 ? " --> Good" : " --> Bad");
	
	write("5.   1 - ");
	write(klista(12, 2, 10, cons(1,cons(0,nil))));
	writeln(klista(12, 2, 10, cons(1,cons(0,nil))) == 1 ? " --> Good" : " --> Bad");
	
	write("6.   1 - ");
	write(klista(12, 13, 10, cons(0,cons(1,nil))));
	writeln(klista(12, 13, 10, cons(0,cons(1,nil))) == 1 ? " --> Good" : " --> Bad");

	write("7.   0 - ");
	write(klista(12, 13, 10, cons(1,nil)));
	writeln(klista(12, 13, 10, cons(1,nil)) == 0 ? " --> Good" : " --> Bad");

	write("8.   1 - ");
	write(klista(10234, 223, 10, cons(1,cons(0,cons(0,cons(1,cons(1,nil)))))));
	writeln(klista(10234, 223, 10, cons(1,cons(0,cons(0,cons(1,cons(1,nil)))))) == 1 ? " --> Good" : " --> Bad");
	
	write("9.   1 - ");
	write(klista(38, 44, 2, cons(0,cons(0,cons(1,cons(0,cons(1,cons(0,nil))))))));
	writeln(klista(38, 44, 2, cons(0,cons(0,cons(1,cons(0,cons(1,cons(0,nil))))))) == 1 ? " --> Good" : " --> Bad");
	
	write("10.  0 - ");
	write(klista(38, 44, 2, cons(1,cons(0,cons(1,cons(0,nil))))));
	writeln(klista(38, 44, 2, cons(1,cons(0,cons(1,cons(0,nil))))) == 0 ? " --> Good" : " --> Bad");
	
	write("11.  1 - ");
	write(klista(1, 1, 2, cons(0,nil)));
	writeln(klista(1, 1, 2, cons(0,nil)) == 1 ? " --> Good" : " --> Bad");
	
	write("12.  0 - ");
	write(klista(1, 1, 2, cons(0,cons(0,nil))));
	writeln(klista(1, 1, 2, cons(0,cons(0,nil))) == 0 ? " --> Good" : " --> Bad");
	
	write("13.  0 - ");
	write(klista(1, 1, 2, nil));
	writeln(klista(1, 1, 2, nil) == 0 ? " --> Good" : " --> Bad");
}
