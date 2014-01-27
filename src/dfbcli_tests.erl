%%%----------------------------------------------------------------------
%%% Diffbot API Erlang Client Library
%%% Test Suite
%%%----------------------------------------------------------------------

-module(dfbcli_tests).

-include("dfbcli.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Test environment
%%%----------------------------------------------------------------------

-define(TST_GURL, "http://www.xconomy.com/san-francisco/2012/07/25/diffbot-is-using-computer-vision-to-reinvent-the-semantic-web/").
-define(TST_PURL, "http://www.diffbot.com/our-apis/article").
% Obtain a token at http://diffbot.com
-define(TST_TOKEN, "DIFFBOT_TOKEN").
-define(TST_CONTENT, <<"Now is the time for all good robots to come to the aid of their-- oh never mind, run!">>).

gargs() ->
	#dfbargs{
		url = ?TST_GURL,
		token = ?TST_TOKEN,
		fields = ?DBFA_TEST_META,
		timeout = ?DBFA_TIMEOUT}.

pargs() ->
	#dfbargs{
		url = ?TST_PURL,
		token = ?TST_TOKEN,
		content = ?TST_CONTENT,
		timeout = ?DBFA_TIMEOUT}.

%%%----------------------------------------------------------------------
%%% Test cases
%%%----------------------------------------------------------------------

test_art_get() ->
	test_print0(<<"Article API (GET)">>, dfbcli:diffbot(gargs())).

test_art_post() ->
	test_print0(<<"Article API (POST)">>, dfbcli:diffbot_post(pargs())).

test_front_get() ->
	test_print0(<<"Frontpage API (GET)">>, dfbcli:diffbot_frontpage(gargs())).

test_image_get() ->
	test_print0(<<"Image API (GET)">>, dfbcli:diffbot_image(gargs())).

test_product_get() ->
	test_print0(<<"Product API (GET)">>, dfbcli:diffbot_product(gargs())).

test_analyze_get() ->
	test_print0(<<"Page Classifier API (GET)">>, dfbcli:diffbot_analyze(gargs())).

test_crawl_get() ->
	Name = "myjob",
	Sites = "http://en.wikipedia.org/wiki/Parma",
	ApiUrl = "http://api.diffbot.com/v2/product?fields=querystring,meta",
	Props = ["maxToCrawl=2", "maxToProcess=2"],
	% create crawlbot
	R1 = dfbcli:diffbot_crawl_new(gargs(), Name, Sites, ApiUrl, Props),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R1]),
	% view crawlbot
	R2 = dfbcli:diffbot_crawl_view(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R2]),
	% pause crawlbot
	R3 = dfbcli:diffbot_crawl_pause(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R3]),
	% resume crawlbot
	R4 = dfbcli:diffbot_crawl_resume(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R4]),
	% download results of crawlbot work
	R5 = dfbcli:diffbot_crawl_download(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R5]),
	% delete crawlbot
	R6 = dfbcli:diffbot_crawl_delete(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R6]).

test_bulk_get() ->
	Name = "myjob",
	Sites = "http://en.wikipedia.org/wiki/Parma http://en.wikipedia.org/wiki/Emilia-Romagna http://en.wikipedia.org/wiki/Romagna http://en.wikipedia.org/wiki/Modena",
	ApiUrl = "http://api.diffbot.com/v2/article?fields=title,date,author",
	Props = ["maxRounds=5"],
	% create bulk job
	R1 = dfbcli:diffbot_bulk_new(gargs(), Name, Sites, ApiUrl, Props),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R1]),
	% view bulk job
	R2 = dfbcli:diffbot_bulk_view(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R2]),
	% pause bulk job
	R3 = dfbcli:diffbot_bulk_pause(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R3]),
	% resume bulk job
	R4 = dfbcli:diffbot_bulk_resume(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R4]),
	% download results of bulk job work
	io:format("~n~p ~p: *** wait for 10 seconds ***~n", [?MODULE, ?LINE]),
	receive
	after 10000 -> ok
	end,
	R5 = dfbcli:diffbot_bulk_download(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R5]),
	% delete bulk job
	R6 = dfbcli:diffbot_bulk_delete(gargs(), Name),
	io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R6]).

test_custom_get() ->
	test_print0(<<"Custom API (GET)">>, dfbcli:diffbot_custom_get(gargs(), "unknown_custom_api")).

test_custom_post() ->
	test_print0(<<"Custom API (POST)">>, dfbcli:diffbot_custom_post(pargs(), "unknown_custom_api")).

%%%----------------------------------------------------------------------
%%% Utils
%%%----------------------------------------------------------------------

self_test() ->
	test_art_get(),
	test_art_post(),
	test_front_get(),
	test_image_get(),
	test_product_get(),
	test_analyze_get(),
	test_crawl_get(),
	test_bulk_get(),
	test_custom_get(),
	test_custom_post(),
	ok.

test_print0(Title, R) ->
	case R of
		{ok, Resp} ->
			io:format("~p: Ok. Json object is:~n~p~n", [Title, Resp]);
		{error, Why, Details} ->
			io:format("error (~p): ~p ~p: ~p: ~p~n", [Title, ?MODULE, ?LINE, Why, Details])
	end,
	ok.

%%-----------------------------------------------------------------------
