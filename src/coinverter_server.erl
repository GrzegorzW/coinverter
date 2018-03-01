-module(coinverter_server).

-behavior(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3, handle_info/2]).

-export([convert/3, refresh_graph_worker/2]).

-define(REFRESH_INTERVAL, 60000).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  io:format("[~p] Initializing server~n", [self()]),
  Graph = build_graph(),
  spawn_link(?MODULE, refresh_graph_worker, [self(), ?REFRESH_INTERVAL]),
  io:format("[~p] Server initialized~n", [self()]),
  {ok, Graph}.

convert(Source, Target, Amount) when is_binary(Source) and is_binary(Target) ->
  gen_server:cast(?MODULE, {convert, self(), Source, Target, decimal_conv:number(Amount)}).

handle_cast({convert, From, Source, Target, Amount}, Graph) ->
  spawn(
    fun() ->
      try convert_pair(Graph, {Source, Target, Amount}) of
        {ok, Result} -> From ! {ok, Result}
      catch
        throw:Reason -> From ! {error, Reason}
      end
    end
  ),

  {noreply, Graph}.

handle_call({}, _From, Graph) ->
  {reply, true, Graph}.

handle_info({refresh_graph}, _Graph) ->
  io:format("[~p] Refreshing graph~n", [self()]),
  {noreply, build_graph()}.

build_graph() ->
  io:format("[~p] Building graph~n", [self()]),
  Products = get_products(),
  Graph = digraph:new(),
  build_graph(Products, Graph),
  io:format("[~p] Graph built~n", [self()]),
  Graph.

build_graph([], Graph) ->
  Graph;
build_graph([_Product = #{<<"price">> := Price, <<"product_id">> := ProductId} | Rest], Graph) ->
  [V1, V2] = string:split(ProductId, "-"),
  add_pair(Graph, {V1, V2, bin_to_num(Price)}),
  build_graph(Rest, Graph).

bin_to_num(Bin) ->
  N = binary_to_list(Bin),
  Num = case string:to_float(N) of
          {error, no_float} -> list_to_integer(N);
          {F, _Rest} -> F
        end,
  decimal_conv:number(Num).

get_products() ->
  {ok, Connection} = gun:open("api.abucoins.com", 443),
  {ok, _Protocol} = gun:await_up(Connection),

  io:format("[~p] Getting products~n", [self()]),
  StreamRef = gun:get(Connection, "/products/ticker"),

  case gun:await(Connection, StreamRef) of
    {response, fin, _Status, _Headers} ->
      no_data;
    {response, nofin, 200, _Headers} ->
      {ok, Body} = gun:await_body(Connection, StreamRef),
      jiffy:decode(Body, [return_maps]);
    {response, nofin, Status, _Headers} ->
      exit({http_error, Status})
  end.

add_pair(Graph, {V1, V2, Price}) ->
  digraph:add_vertex(Graph, V1),
  digraph:add_vertex(Graph, V2),
  digraph:add_edge(Graph, V1, V2, Price),
  digraph:add_edge(Graph, V2, V1, decimal_arith:divide(decimal_conv:number(1), Price, [{precision, 32}])).

convert_pair(Graph, {Source, Target, Amount}) ->
  case digraph:get_short_path(Graph, Source, Target) of
    false -> throw(path_not_found);
    Path ->
      Result = decimal_arith:multiply(Amount, convert_path(Graph, Path, decimal_conv:number(1)), [{precision, 32}]),
      {ok, Result}
  end.

convert_path(_Graph, [_], Multiplier) ->
  Multiplier;
convert_path(Graph, [V1, V2 | Rest], Multiplier) ->
  NewMultiplier = decimal_arith:multiply(Multiplier, get_price(Graph, {V1, V2}), [{precision, 32}]),
  convert_path(Graph, [V2 | Rest], NewMultiplier).

get_price(Graph, {EmanatingVertex, IncidentVertex}) ->
  get_price(Graph, EmanatingVertex, digraph:in_edges(Graph, IncidentVertex)).

get_price(_Graph, _EmanatingVertex, []) ->
  throw(price_not_found);
get_price(Graph, EmanatingVertex, [Edge | Edges]) ->
  case digraph:edge(Graph, Edge) of
    {_, EmanatingVertex, _, Price} -> Price;
    _ -> get_price(Graph, EmanatingVertex, Edges)
  end.

refresh_graph_worker(Server, Timeout) ->
  receive
  after Timeout ->
    Server ! {refresh_graph},
    refresh_graph_worker(Server, Timeout)
  end.
