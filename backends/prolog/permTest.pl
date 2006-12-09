test('true') :-
	\+ model(_), \+ student(_).
test('true') :-
	model(X), student(Y),
	maplist(msort, X, Z), maplist(msort, Y, Z), !.
test('false').

% Keep this comment at the end of the file

% Local variables:
% mode: prolog
% End:
