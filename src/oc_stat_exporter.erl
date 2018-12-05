%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% Exporter exports the collected records as view data.
%% @end
-module(oc_stat_exporter).

-export([start_link/0,
         batch_register/1,
         register/1,
         register/2,
         deregister/1,
         registered/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export_types([exporter/0]).

-compile({no_auto_import, [register/2]}).

-type exporter() :: module().

-include("opencensus.hrl").
-include("oc_logger.hrl").

-callback export(ViewData, Config) -> ok when
      ViewData :: oc_stat_view:view_data(),
      Config  :: any().

-record(state, {exporters :: list(),
                export_interval_ms :: integer(),
                timer_ref :: reference()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc
%% Register many `Exporters' at once.
%% @end
-spec batch_register(Exporters) -> ok when
      Exporters :: [exporter()].
batch_register(Exporters) when is_list(Exporters) ->
    gen_server:call(?MODULE, {batch_register, Exporters}, infinity),
    ok.

%% @doc
%% @equiv register(Exporter, [])
%% @end
-spec register(Exporter) -> ok when
      Exporter :: exporter().
register(Exporter) ->
    register(Exporter, []),
    ok.

%% @doc
%% Registers an `Exporter' with `Config'.
%% Collected data will be reported via all the
%% registered exporters. Once you no longer
%% want data to be exported, invoke {@link deregister/1}
%% with the previously registered exporter.
%% @end
-spec register(Exporter, Config) -> ok when
      Exporter :: exporter(),
      Config  :: any().
register(Exporter, Config) ->
    gen_server:call(?MODULE, {register, Exporter, Config}, infinity),
    ok.

%% @doc
%% Deregisters an `Exporter'.
%% @end
-spec deregister(Exporter) -> ok when
      Exporter :: exporter().
deregister(Exporter) ->
    gen_server:call(?MODULE, {deregister, Exporter}, infinity),
    ok.

%% @doc
%% Checks whether `Exporter' is registered.
%% @end
-spec registered(Exporter) -> boolean() when
      Exporter :: exporter().
registered(Exporter) ->
    gen_server:call(?MODULE, {registered, Exporter}, infinity).

%% @doc
%% @private
%% Called by opencensus
%% @end
export(Exporters) ->
    Measurements = oc_stat:export(),

    [try
         Exporter:export(Measurements, Config)
     catch
         ?WITH_STACKTRACE(Class, Exception, Stacktrace)
         ?LOG_INFO("stat exporter ~p threw ~p:~p, stacktrace=~p",
                   [Exporter, Class, Exception, Stacktrace])
         end
     || {Exporter, Config} <- Exporters],
    ok.

init(_Args) ->
    ExportInterval = oc_stat_config:export_interval(),
    %% TODO: validate
    Exporters = oc_stat_config:exporters(),
    %% TODO: should we init exporters here, before exporting?
    Ref = erlang:send_after(ExportInterval, self(), export_stats),
    {ok, #state{exporters=init_exporters(Exporters),
                export_interval_ms=ExportInterval,
                timer_ref=Ref}}.

handle_call({batch_register, NewExporters}, _From, State=#state{exporters=Exporters}) ->
    {reply, ok, State#state{exporters=init_exporters(NewExporters) ++ Exporters}};
handle_call({register, Exporter, Config}, _From,  State=#state{exporters=Exporters}) ->
    {reply, ok, State#state{exporters=[{Exporter, maybe_call_exporter_init(Exporter, Config)} | Exporters]}};
handle_call({deregister, Exporter}, _From,  State=#state{exporters=Exporters}) ->
    {reply, ok, State#state{exporters=proplists:delete(Exporter, Exporters)}};
handle_call({registered, Exporter}, _From, State=#state{exporters=Exporters}) ->
    {reply, proplists:is_defined(Exporter, Exporters), State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(export_stats, State=#state{exporters=Exporters,
                                       export_interval_ms=ExportInterval,
                                       timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    Ref1 = erlang:send_after(ExportInterval, self(), export_stats),
    spawn(fun () ->
                  export(Exporters)
          end),
    {noreply, State#state{timer_ref=Ref1}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, #state{timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    ok.

init_exporters(Map) when is_map(Map) ->
    init_exporters(maps:to_list(Map));
init_exporters(List) when is_list(List) ->
    UList = proplists:unfold(List),
    [{Exporter, maybe_call_exporter_init(Exporter, EConfig)} || {Exporter, EConfig} <- UList];
init_exporters(Thing) ->
    erlang:error({invalid_exporters, Thing}).

maybe_call_exporter_init(Exporter, Config) ->
    case erlang:function_exported(Exporter, init, 1) of
        true ->
            try
                Exporter:init(Config)
            catch
                ?WITH_STACKTRACE(Class, Exception, Stacktrace)
                ?LOG_INFO("stat exporter ~p init/1 threw ~p:~p, stacktrace=~p",
                          [Exporter, Class, Exception, Stacktrace])
            end;
        _ -> Config
    end.
