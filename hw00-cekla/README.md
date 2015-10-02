# First Cekla program

## 0. kis házi feladat (Cekla)

 * 1.1 változat 
 * Utolsó módosítás: 2015-09-14
 * Kiadás: 2015-09-15
 * Beadási határidő: 2015-09-25, péntek, 23:59
 * Feladat linkje: [http://dp.iit.bme.hu/dp-current/dp15a-khf0.html](http://dp.iit.bme.hu/dp-current/dp15a-khf0.html)


### Definíciók
Legyenek A, B és N egész számok, ahol A>0, B>0 és N≥2. Az A és B számoknak egy N alapú (számrendszerben képzett) KL különbözőségi listáját a következőképpen definiáljuk:

Jelöljük k-val a max(A,B) szám N alapú számrendszerben való felírásához szükséges számjegyek számát. Írjuk fel az A és B számokat N alapú számrendszerben! A kapott számjegyeket jelölje rendre A1, ..., Ak és B1, ..., Bk, ahol a kisebbik számot vezető nullákkal k számjegyűre egészítjük ki, ha k-nál kevesebb számjegyből áll.
A KL különbözőségi lista egy k hosszúságú lista, amelynek i-edik eleme 0 ha Ai=Bi, és 1 egyébként.
Például, ha A=10234, B=223 és N=10 (ahol az itt leírt számok 10-es számrendszerben értelmezendők), akkor k=5, A1 A2 A3 A4 A5 = 1 0 2 3 4, B1 B2 B3 B4 B5 = 0 0 2 2 3, és így a különbözőségi lista KL = 1 0 0 1 1.

Egy másik példa: A=38, B=44 és N=2 (ahol az itt leírt számok 10-es számrendszerben értelmezendők). Ez esetben k=6, hiszen a nagyobbik szám, 44 bináris alakja 6-jegyű. A számjegylisták: 
A1 A2 A3 A4 A5 A6 = 1 0 0 1 1 0 és B1 B2 B3 B4 B5 B6 = 1 0 1 1 0 0, és így a különbözőségi lista KL = 0 0 1 0 1 0.

Vegyük észre, hogy N=2 esetén a különbözőségi lista úgy is előállítható, hogy az A és B számokra a bitenkénti "kizáró vagy" (XOR) műveletet alkalmazzuk, képezzük az így kapott szám bináris jegyeinek listáját, majd ezt a listát elölről 0 jegyekkel kiegészítjük szükség esetén úgy, hogy hossza megegyezzék a nagyobb szám bináris jegyeinek számával.


### A feladat
Írjon olyan programot Cékla nyelven (a C++ nyelv egy deklaratív résznyelvén), amelynek fő függvénye az

int klista(const int A, const int B, const int N, const list KL){...}
függvény. Ennek feladata annak ellenőrzése, hogy a megadott A, B, N, számok és a szintén megadott KL számlista között fennáll-e a fent definiált "az A és B számok N alapú különbözőségi listája a KL lista" kapcsolat. Ha ez fennáll, a függvény az igaz (1) értékkel kell visszatérjen, egyébként a hamis (0) értékkel.
/* klista(A, B, N, KL) == V, ahol V = 1, ha KL az A és B számok N alapú
   különbözőségi listája, a fenti definíció értelmében, egyébként pedig V = 0. 
   (A>0, B >0; N≥2 egész számok, KL egy lista).  */ 
int klista(const int A, const int B, const int N, const list KL)
   { ... }
A jobbrekurzív függvények kevesebb memóriát használnak, mint az egyéb rekurzív függvények, ezért a használatukat ajánljuk, azonban a pontozáskor csak azt vizsgáljuk, hogy a program a megadott (néhány másodperces) időkorláton belül előállítja-e a helyes megoldást.

### Mintamegoldás speciális esetre
Az alábbi mintamegoldás csak akkor működik helyesen, ha A, B < N2 és a két szám közül legalább az egyik ≥ N.
```c++
int klista(const int A, const int B, const int N, const list KL) {
  if (KL == nil) return 0;
  if (tl(KL) == nil) return 0;
  if (tl(tl(KL)) != nil) return 0;
  return (hd(KL) == (A/N!=B/N)) *
     (hd(tl(KL)) == (A%N!=B%N));
}
```

### Példák
```c++
|* klista(12, 23, 10, cons(1,cons(1,nil))).
1
|* klista(12, 23, 10, nil).
0
|* klista(12, 22, 10, cons(1,cons(1,nil))).
0
|* klista(12, 22, 10, cons(1,cons(0,nil))).
1
|* klista(12, 2, 10, cons(1,cons(0,nil))).
1
|* klista(12, 13, 10, cons(0,cons(1,nil))).
1
|* klista(12, 13, 10, cons(1,nil)).
0
|* klista(10234, 223, 10, cons(1,cons(0,cons(0,cons(1,cons(1,nil)))))).
1
|* klista(38, 44, 2, cons(0,cons(0,cons(1,cons(0,cons(1,cons(0,nil))))))).
1
|* klista(38, 44, 2, cons(1,cons(0,cons(1,cons(0,nil))))).
0
|* klista(1, 1, 2, cons(0,nil)).
1
|* klista(1, 1, 2, cons(0,cons(0,nil))).
0
|* klista(1, 1, 2, nil).
0
|* 
```
