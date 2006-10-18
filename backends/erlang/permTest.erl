test([],[])  -> true;
test(L1, L2) -> if length(L1) /= length(L2) -> false;
    true -> lists:member(hd(L1), L2) andalso test(tl(L1), lists:delete(hd(L1),L2))
    end.
