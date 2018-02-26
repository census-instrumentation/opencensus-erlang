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
%%
%% The `export' callback should return quickly; if an
%% Exporter takes a significant amount of time to
%% process a ViewData, that work should be done on another process.
%% @end
-module(oc_stat_exporter).

-export([start_link/0,
         batch_register/1,
         register/1,
         register/2,
         deregister/1,
         registered/1]).

-export([export/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export(['__init_backend__'/0]).

-export_types([exporter/0]).

-compile({no_auto_import, [register/2]}).

-type exporter() :: module().

-include("opencensus.hrl").

-callback export(ViewData, Config) -> ok when
      ViewData :: oc_stat_view:view_data(),
      Config  :: any().

-record(state, {%% exporters :: [{module(), any}],
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
    [register(Exporter, Config) || {Exporter, Config} <- Exporters],
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
    ets:insert(?MODULE, {Exporter, Config}),
    ok.

%% @doc
%% Deregisters an `Exporter'.
%% @end
-spec deregister(Exporter) -> ok when
      Exporter :: exporter().
deregister(Exporter) ->
    ets:delete(?MODULE, Exporter),
    ok.

%% @doc
%% Checks whether `Exporter' is registered.
%% @end
-spec registered(Exporter) -> boolean() when
      Exporter :: exporter().
registered(Exporter) ->
    ets:lookup(?MODULE, Exporter) =/= [].

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {read_concurrency, true}]),
    ok.

%% @doc
%% @private
%% Called by opencensus
%% @end
export() ->
    Measurements = oc_stat:export(),
    Exporters = ets:tab2list(?MODULE),

    [try
         Exporter:export(Measurements, Config)
     catch
         Class:Exception ->
             error_logger:info_msg("stat exporter ~p threw ~p:~p, stacktrace=~p",
                                   [Exporter, Class, Exception, erlang:get_stacktrace()])
     end
     || {Exporter, Config} <- Exporters],
    ok.

init(_Args) ->
    ExportInterval = oc_stat_config:export_interval(),
    %% TODO: validate
    Exporters = oc_stat_config:exporters(),
    ets:insert(?MODULE, Exporters),
    %% TODO: should we init exporters here, before exporting?
    Ref = erlang:send_after(ExportInterval, self(), export_stats),
    {ok, #state{export_interval_ms=ExportInterval,
                timer_ref=Ref}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(export_stats, State=#state{export_interval_ms=ExportInterval,
                                       timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    Ref1 = erlang:send_after(ExportInterval, self(), export_stats),
    spawn(fun export/0),
    {noreply, State#state{timer_ref=Ref1}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, #state{timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    ok.
