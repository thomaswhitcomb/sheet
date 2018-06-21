-module(stack).
-export([
  new/0
	,push/2
	,pop/1
	,empty/1
	,top/1
	,size/1
]).

new() -> [].
push(Stack,Value) ->
 [Value|Stack].

pop([Top|Stack]) ->
 {Top,Stack}.

empty([]) -> true;
empty(_) -> false.

size(L) -> length(L).

top([H|_]) -> H.
