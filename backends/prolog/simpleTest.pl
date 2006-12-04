% Return 'false' if there is a solution for model that is not a
% solution for student or the other way around.
test('false') :- model(M),   \+ student(M), !.
test('false') :- student(S), \+ model(S),   !.
% Return 'true' otherwise.
test('true').

% Keep this comment at the end of the file

% Local variables:
% mode: prolog
% End:
