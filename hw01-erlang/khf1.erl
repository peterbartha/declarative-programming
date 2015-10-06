-module(khf1).
-author('mail@peterbartha.com').
-vsn('2015-10-02').
-export([lista_zsak/1]).
%-compile(export_all).


%% @spec count_elem(E::term(), L::[term()]) -> N::integer().
%% E elem az L listában N-szer fordul elő.
count_elem(_, []) -> 0;
count_elem(Elem, [Elem|Tail]) -> 1 + count_elem(Elem, Tail);
count_elem(Elem, [_|Tail]) -> count_elem(Elem, Tail).


%% @spec is_member(E::term(), L::[term()]) -> true | false.
%% E elem szerepel-e az L listában.
is_member(_Elem, []) -> false;
is_member(Elem, List) ->
	case count_elem(Elem, List) of
		0 -> false;
		_ -> true
	end.

	
%% @spec filter_replicates(L::[term()], Acc::[term()]) -> F::[term()].
%% Acc tartalmaz E minden különböző eleméből pontosan egyet.
filter_replicates([], []) ->  [];
filter_replicates([], Acc) -> Acc;
filter_replicates([Head|Tail], Acc) ->
    case is_member(Head, Acc) of
        false -> filter_replicates(Tail, [Head|Acc]);
        true -> filter_replicates(Tail, Acc)
    end.

	
%% @type zsak_alak() = [zsak_elem()].
%% @type zsak_elem() = {any(),integer()}.
%% @spec bag_list(O::[term()], L::[term()], Acc::[term()]) -> F::zsak_alak().
%% Acc tartalmazza a zsak_alak()-ra hozott O listát
bag_list([], _, _) -> [];
bag_list(_, [], Acc) -> Acc;
bag_list(OriginalList, [Head|Tail], Acc) ->
	N = count_elem(Head, OriginalList),
	bag_list(OriginalList, Tail, [{Head, N}|Acc]).

	
%% @type zsak_alak() = [zsak_elem()].
%% @type zsak_elem() = {any(),integer()}.
%% @spec khf1:lista_zsak(L::[any()]) -> Zsak::zsak_alak().
%% Az L lista zsák-alakja Zsak.
lista_zsak([]) -> [];
lista_zsak(List) ->
	F = filter_replicates(List, []),
	bag_list(List, F, []).
