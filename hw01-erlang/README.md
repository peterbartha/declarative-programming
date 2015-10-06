# Lista zsák-alakra hozása

## 1. kis házi feladat (Erlang)

 * 1.0 változat 
 * Utolsó módosítás: 2015-09-20
 * Kiadás: 2015-09-22
 * Beadási határidő: Erlang 2015-10-05, 23:59; Prolog 2015-10-09, 23:59
 * Feladat linkje: [https://dp.iit.bme.hu/dp-current/dp15a-khf1.html](https://dp.iit.bme.hu/dp-current/dp15a-khf1.html)


### A feladat
A feladat az ún. zsák (angolul bag) fogalmához kapcsolódik, amelyet multihalmaznak (multiset) is hívnak. A zsák a halmaz egy olyan általánosítása, amelyben egy elem többszörösen is előfordulhat. Egy adott zsák adott a elemének a multiplicitásán az a elem előfordulásainak számát értjük.

Egy zsákot legegyszerűbben egy listával ábrázolhatunk, ahol természetesen a listaelemek sorrendje érdektelen ([a,b,a] és [a,a,b] ugyanazt a zsákot jelöli). Egy ennél tömörebb ábrázolás az, amikor felsoroljuk a zsák különböző elemeit és mindegyik elem mellett megadjuk a multiplicitását. Nevezzük ez utóbbi ábrázolást zsák-alaknak. Az előbbi példa zsák-alakja Prologban [a-2,b-1], Erlangban [{a,2},{b,1}]. (A Prolog nyelv konvenciói közé tartozik, hogy a párokat Első-Második alakban írják, azaz a -/2 struktúrával ábrázolják, lásd pl. a keysort/2 beépített eljárást.)

A zsák-alakban is érdektelen a listaelemek sorrendje, tehát ha a fenti példában a két listaelemet megcseréljük, akkor is ugyanazt a zsákot kapjuk. A zsák-alak további fontos tulajdonságai: a párok első tagja nem ismétlődik, és a multiplicitások pozitív egészek.

Írjon olyan Prolog eljárást, illetve Erlang függvényt, amely egy listát zsák-alakra hoz!

### Erlang-specifikációk
Írjon Erlang-függvényt khf1:lista_zsak/1 néven egy tetszőleges lista zsák-alakjának az előállítására.
```erlang
%% @type zsak_alak() = [zsak_elem()].
%% @type zsak_elem() = {any(),integer()}.
%% @spec khf1:lista_zsak(L::[any()]) -> Zsak::zsak_alak().
%% Az L lista zsák-alakja Zsak.
```
A programot tartalmazó modul attribútumai ezek legyenek:
```erlang
-module(khf1).
-author('email@unit.org.hu').
-vsn('year-mm-dd').
-export([lista_zsak/1]).
%-compile(export_all).
```

### Példák
```erlang
| ?- lista_zsak([], ZS).
ZS = [] ? ;
no
| ?- lista_zsak([a,b,a,b,b], ZS).
ZS = [a-2,b-3] ? ;
no
| ?- lista_zsak([korte,alma,dio,alma,dio,dio,tok], ZS).
ZS = [korte-1,alma-2,dio-3,tok-1] ? ;
no
```

Természetesen mindkét nyelv esetén elfogadjuk azokat a programokat is, amelyek eredményei a fentiektől csak a listaelemek sorrendjében térnek el.
