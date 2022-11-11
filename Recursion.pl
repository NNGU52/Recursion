% числа Фибоначчи (1, 1, 2, 3, 5, 8, 13, 21, ...) следующее число это сумма двух предыдущих 1+1=2
% числа Фибоначчи через нисходящую рекурсию:
fib_down(N,X):-rec_down(N, _, X).
rec_down(0, 0, 1):-!.
rec_down(N, W, X):-N1 is N-1, rec_down(N1, A, B), W is B, X is A+B.

% числа Фибоначчи через восходящую рекурсию:
fib_up(N,X):-rec_up(N, X, 0, 0, 1).
rec_up(N,X,N,_,X):-!.
rec_up(N,X,K,A,B):-K1 is K+1, C is A+B, rec_up(N,X,K1,B,C).

% Произведение двух целых положительных чисел:
% Пропишем условие что произведение на 0 всегда дает 0:
mult(0,_,0).
mult(_,0,0).
% Само произведение через восходящую рекурсию:
mult(A,B,X):-((A>0),(B>0),integer(A),integer(B))->(mult_up(A,B,X,1,B));
	      write('Ведите положительные целые числа.'), nl,
	      write('Первый множитель|: '), read(A1), nl, write('Второй множитель'), read(B1), nl, mult(A1,B1,X).
% Восходящая рекрсия:
mult_up(A,_,X,A,X):-!.
mult_up(A,B,X,A1,X1):-A2 is A1+1, X2 is X1+B, mult_up(A,B,X,A2,X2).

% Вычислить сумму ряда целых четных чисел
pow(N):-(integer(N)) -> (SS is N*2, pow_d(N,SS)); write('Введите целое число!').
pow_d(N,SS):- (N>0) -> (N1 is N-1, S1 is SS+2*N1, pow_d(N1,S1)); 
	      write('Cумма ряда '), write(SS).

% Организовать ввод целых положительных чисел и их суммирование до тех пор, пока сумма не превысит некоторого порогового значения.
% Введенные отрицательные целые числа суммироваться не должны.
sum(Summa, Predel):- read(Chislo), (Chislo>0, integer(Chislo)) -> (Summa1 is Chislo+Summa, (Summa1<Predel) -> (write('Текущая сумма - '), write(Summa1),nl,sum(Summa1, Predel)));
		     write('Ошибка! Ведите положительное целое число.'), nl,
		     write('Текущая сумма: '), write(Summa),nl,
		     sum(Summa, Predel).