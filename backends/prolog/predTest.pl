meta_test :- \+ model(_), \+ student(_).
% The [!] in the second [meta_test] is important.  It stops Prolog
% from searching for other solutions of [S] after [student(S)] has
% succeeded once.  If it's omitted, Prolog would try again and again
% to find an [S] such that [apply(pred, S)] is satisfied.  As
% [student(S)] might have an infinite number of solutions, this is not
% what we want.
meta_test :- model(_), student(S), !, apply(pred, S).
test('true') :- meta_test.
test('false').

% Main: compare model and student solution with test/1, find all model
% and student solutions and display the results.
:- test(E),
	first_solution_or_nil(model,   Ms),
	first_solution_or_nil(student, Ss),
	display_res(Ms, Ss, E).

% Keep this comment at the end of the file

% Local variables:
% mode: prolog
% End:
