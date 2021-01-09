foo1 :- bar1, car1.
bar1 :- true.
car1 :- true.

foo2 :- bar2, car2.
bar2 :- true.
car2 :- !, fail.

foo3 :- bar3 ; car3.
bar3 :- fail.
car3 :- true.

foo4 :- bar4(X), car4(X).
bar4(Y) :- Y = ghi.
car4(abc).
car4(def).
car4(ghi).