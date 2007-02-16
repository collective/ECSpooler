test('true') :-
	\+ model(_), \+ student(_).
test('true') :-
	model(X), student(Y),
	maplist(msort, X, Z), maplist(msort, Y, Z), !.
test('false').

% Main: compare model and student solution with test/1, find all model
% and student solutions and display the results.
:- test(E), findall(X, model(X), Ms), findall(X, student(X), Ss),
        display_res(Ms, Ss, E).

% Keep this comment at the end of the file

% Local variables:
% mode: prolog
% End:
