% ����� ��������� (1, 1, 2, 3, 5, 8, 13, 21, ...) ��������� ����� ��� ����� ���� ���������� 1+1=2
% ����� ��������� ����� ���������� ��������:
fib_down(N,X):-rec_down(N, _, X).
rec_down(0, 0, 1):-!.
rec_down(N, W, X):-N1 is N-1, rec_down(N1, A, B), W is B, X is A+B.

% ����� ��������� ����� ���������� ��������:
fib_up(N,X):-rec_up(N, X, 0, 0, 1).
rec_up(N,X,N,_,X):-!.
rec_up(N,X,K,A,B):-K1 is K+1, C is A+B, rec_up(N,X,K1,B,C).

% ������������ ���� ����� ������������� �����:
% �������� ������� ��� ������������ �� 0 ������ ���� 0:
mult(0,_,0).
mult(_,0,0).
% ���� ������������ ����� ���������� ��������:
mult(A,B,X):-((A>0),(B>0),integer(A),integer(B))->(mult_up(A,B,X,1,B));
	      write('������ ������������� ����� �����.'), nl,
	      write('������ ���������|: '), read(A1), nl, write('������ ���������'), read(B1), nl, mult(A1,B1,X).
% ���������� �������:
mult_up(A,_,X,A,X):-!.
mult_up(A,B,X,A1,X1):-A2 is A1+1, X2 is X1+B, mult_up(A,B,X,A2,X2).

% ��������� ����� ���� ����� ������ �����
pow(N):-(integer(N)) -> (SS is N*2, pow_d(N,SS)); write('������� ����� �����!').
pow_d(N,SS):- (N>0) -> (N1 is N-1, S1 is SS+2*N1, pow_d(N1,S1)); 
	      write('C���� ���� '), write(SS).

% ������������ ���� ����� ������������� ����� � �� ������������ �� ��� ���, ���� ����� �� �������� ���������� ���������� ��������.
% ��������� ������������� ����� ����� ������������� �� ������.
sum(Summa, Predel):- read(Chislo), (Chislo>0, integer(Chislo)) -> (Summa1 is Chislo+Summa, (Summa1<Predel) -> (write('������� ����� - '), write(Summa1),nl,sum(Summa1, Predel)));
		     write('������! ������ ������������� ����� �����.'), nl,
		     write('������� �����: '), write(Summa),nl,
		     sum(Summa, Predel).