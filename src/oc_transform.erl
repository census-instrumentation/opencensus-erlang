%%%------------------------------------------------------------------------
%% Copyright 2017, OpenCensus Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc oc_transform provides a parse transform for wrapping a function in
%% a start and a finish of a span.
%% @end
%%%-----------------------------------------------------------------------

-module(oc_transform).

-export([parse_transform/2,
         format_error/1]).

parse_transform(Ast, _Options)->
    try lists:mapfoldl(fun form/2, {false, []}, Ast) of
        {Ast1, _} ->
            lists:flatten(lists:filter(fun(Node) -> Node =/= nil end, Ast1))
    catch
        throw:E ->
            E
    end.

form({attribute, _Line, trace, Args}, _) ->
    {nil, {true, Args}};
form(Node={function, _Line, _FuncName, _Arity, _Clauses}, {false, []}) ->
    {Node, {false, []}};
form(Node={function, _Line, _FuncName, _Arity, _Clauses}, {true, Args}) ->
    {trace(Node, Args), {false, []}};
form(Node, Trace) ->
    {Node, Trace}.

trace({function, Line, Name, Arity, Clauses}, Args) ->
    case args_proplist(Args) of
        {error, Reason} ->
            {error, {Line, ?MODULE, Reason}};
        ArgsPropList ->
            SpanName = proplists:get_value(name, ArgsPropList, Name),
            Clauses1 = trace_clauses(Clauses, SpanName),
            {function, Line, Name, Arity, Clauses1}
    end.

trace_clauses([], _) ->
    [];
trace_clauses([{clause, Line, H, G, B} | Cs], Name) ->
    StartSpan = [{call, Line,
                  {remote, Line, {atom, Line, ocp}, {atom, Line, start_span}},
                  [{string, Line, to_binary(Name)}]}],
    FinishSpan = [{call, Line,
                   {remote, Line, {atom, Line, ocp}, {atom, Line, finish_span}},
                   []}],
    Trace = StartSpan ++ [{'try', Line, B, [], [], FinishSpan}],
    [{clause, Line, H, G, Trace} | trace_clauses(Cs, Name)].

to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8).

args_proplist(A) when is_binary(A) ->
    [{name, A}];
args_proplist(A) when is_atom(A) ->
    [{name, A}];
args_proplist([]) ->
    [];
args_proplist(A) when is_list(A) ->
    try unicode:characters_to_nfc_binary(A) of
        B ->
            [{name, B}]
    catch
        %% must not be a string, use as a proplist
        error:function_clause ->
            A
    end;
args_proplist(A) ->
    {error, {bad_trace_args, A}}.

format_error({bad_trace_args, Args}) ->
    io_lib:format("Bad trace arguments. Must be binary, atom, list string or proplist. Got: ~p", [Args]);
format_error({bad_name, Args}) ->
    io_lib:format("Bad span name. Name must be an atom, binary or printable list. Got: ~p", [Args]).
