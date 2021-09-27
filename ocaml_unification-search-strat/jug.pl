% the old problem of
% jug a with capacity 4, jug b with capacity 3, how to get 2 unit of water?
% we can solve it as a search problem.

use_modules(library(clpfd)).

% define jug (ID, Capacity, Fill)
% define state to be a list of jugs
% define move relates curren state Js0 to next state Js


% the default solution. prolog use depth frist search by deafult, in
% this case the search will not terminate because dfs is an incomplete
% search strategy.
move1(Js0) --> { member(jug(_, _, 2), Js0) }.
move1(Js0) --> [from_to(F, T)],
               { select(jug(F, FC, FF0), Js0, Js1),
                 select(jug(T, TC, TF0), Js1, Js),
                 FF0 #> 0, TF0 #< TC, M #= min(FF0, TC - TF0),
                 FF #= FF0 - M, TF #= TF0 _ M
               },
               move1([jug(F, FC, FF), jug(T, TC, TF)|Js]).

move(Js0) --> { member(jug(_, _, 2), Js0) }.
move(Js0) --> [from_to(F, T)],
              { select(jug(F, FC, FF0), Js0, Js1),
                select(jug(T, TC, TF0), Js1, Js),
                M #= min(FF0, TC - TF0),
                FF #= FF0 - M,
                TF #= TF0 + M
              },
              moves([jug(F, FC, FF), jug(T, TC, TF) | Js]).


main :-  phrase(move(Js), Ms).

% this repo contains some ocaml implementation for the same logic

% NOTE:
% I'm not particularly prolog savvy so this is a chance to learn some.

% here we use dcg notation. dcg: definite clause grammar defines a list.
% it's essentially let you write production rules.
% head --> { grammar goal}
%
% e.g a list that every elemnts is an atom a
% as --> []
% as --> [a], as.
