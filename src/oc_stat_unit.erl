-module(oc_stat_unit).

-export([is_comparable/2,
         must_convert/1,
         convert/3]).

%% ===================================================================
%% API
%% ===================================================================

is_comparable(VUnit, MUnit) ->
    {CVUnit, _} = canonical(VUnit),
    {CMUnit, _} = canonical(MUnit),

    case CMUnit of
        arbitrary ->
            false;
        CVUnit ->
            true;
        _ ->
            false
    end.

must_convert(native_time_unit) ->
    true;
must_convert(_) ->
    false.

convert(Value, From, To) ->
    case is_comparable(From, To) of
        false -> {error, {not_comparable_units, From, To}};
        true ->
            {_, FMult} = canonical(From),
            {_, TMult} = canonical(To),
            Value * FMult / TMult
    end.

%% ===================================================================
%% Private functions
%% ===================================================================

canonical(native_time_unit) ->
    {native_time_unit, 1};
canonical(nanosecond) ->
    {native_time_unit, erlang:convert_time_unit(1, nanosecond, native)};
canonical(microsecond) ->
    {native_time_unit, erlang:convert_time_unit(1, microsecond, native)};
canonical(millisecond) ->
    {native_time_unit, erlang:convert_time_unit(1, millisecond, native)};
canonical(second) ->
    {native_time_unit, erlang:convert_time_unit(1, second, native)};
canonical(minute) ->
    {native_time_unit, 60 * erlang:convert_time_unit(1, second, native)};
canonical(hour) ->
    {native_time_unit, 3600 * erlang:convert_time_unit(1, second, native)};
canonical(day) ->
    {native_time_unit, 3600 * 24 * erlang:convert_time_unit(1, second, native)};
canonical(_) ->
    {arbitrary, arbitrary}.
