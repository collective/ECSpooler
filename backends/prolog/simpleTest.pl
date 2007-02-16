% Return 'false' if there is a solution for model that is not a
% solution for student or the other way around.
test('false') :- model(M),   \+ student(M), !.
test('false') :- student(S), \+ model(S),   !.
% Return 'true' otherwise.
test('true').

% Main: compare model and student solution with test/1, find all model
% and student solutions and display the results.
:- test(E), findall(X, model(X), Ms), findall(X, student(X), Ss),
        display_res(Ms, Ss, E).

% Keep this comment at the end of the file

% Local variables:
% mode: prolog
% End:
