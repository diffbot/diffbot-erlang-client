%%%----------------------------------------------------------------------
%%% Diffbot API Erlang Client Library
%%% 
%%%----------------------------------------------------------------------

-module(dfbcli).
-include("dfbcli.hrl").

-export([diffbot/1, diffbot/2, diffbot_post/1, diffbot_post/2]).
-export([diffbot_frontpage/1, diffbot_image/1, diffbot_product/1, diffbot_analyze/1]).
-export([diffbot_crawl_new/5, diffbot_crawl_delete/2, diffbot_crawl_pause/2,
		 diffbot_crawl_resume/2, diffbot_crawl_download/2, diffbot_crawl_view/2]).
-export([diffbot_bulk_new/5, diffbot_bulk_delete/2, diffbot_bulk_pause/2,
		 diffbot_bulk_resume/2, diffbot_bulk_download/2, diffbot_bulk_view/2]).
-export([diffbot_custom_get/2, diffbot_custom_post/3, diffbot_custom_post/2]).
-export([make_api_url/3]).

%%%----------------------------------------------------------------------
%%% Article API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Article API using GET
%%  Params:
%%    Url   - url to analyze
%%    Token - developer token
%% @end
%%-----------------------------------------------------------------------
-spec diffbot(Url :: string(), Token :: string()) -> dbfa_resp().
diffbot(Url, Token) when is_list(Url), is_list(Token) ->
	diffbot(#dfbargs{url = Url, token = Token}).

%%-----------------------------------------------------------------------
%% @doc
%%  Article API using GET
%%  Params:
%%    Args  - options to use for the call
%% @end
%%-----------------------------------------------------------------------
-spec diffbot(Args :: #dfbargs{}) -> dbfa_resp().
diffbot(Args) when is_record(Args, dfbargs) ->
	call_api(article, Args#dfbargs{method = get}, [fields]).

%%-----------------------------------------------------------------------
%% @doc
%%  Article API using POST
%%  Params:
%%    Args  - options to use for the call
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_post(Args :: #dfbargs{}) -> dbfa_resp().
diffbot_post(Args) when is_record(Args, dfbargs) ->
	call_api(article, Args#dfbargs{method = post}, [fields]).

%%-----------------------------------------------------------------------
%% @doc
%%  Article API using POST
%%  Params:
%%    Args    - options to use for the call
%%    Content - content to send
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_post(Content :: string() | binary(), Args :: #dfbargs{}) -> dbfa_resp().
diffbot_post(Content, Args) when is_record(Args, dfbargs) ->
	call_api(article, Args#dfbargs{method = post, content = Content}, [fields]).


%%%----------------------------------------------------------------------
%%% Frontpage API: http://diffbot.com/dev/docs/frontpage/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Frontpage API using GET
%%  Params:
%%    Args  - options to use for the call
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_frontpage(Args :: #dfbargs{}) -> dbfa_resp().
diffbot_frontpage(Args) when is_record(Args, dfbargs) ->
	call_api(front, Args#dfbargs{method = get}, [format]).


%%%----------------------------------------------------------------------
%%% Image API: http://diffbot.com/dev/docs/image/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Image API using GET
%%  Params:
%%    Args  - options to use for the call
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_image(Args :: #dfbargs{}) -> dbfa_resp().
diffbot_image(Args) when is_record(Args, dfbargs) ->
	call_api(image, Args#dfbargs{method = get}, [fields]).


%%%----------------------------------------------------------------------
%%% Product API: http://diffbot.com/dev/docs/product/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Product API using GET
%%  Params:
%%    Args  - options to use for the call
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_product(Args :: #dfbargs{}) -> dbfa_resp().
diffbot_product(Args) when is_record(Args, dfbargs) ->
	call_api(product, Args#dfbargs{method = get}, [fields]).


%%%----------------------------------------------------------------------
%%% Page Classifier API: http://diffbot.com/dev/docs/analyze/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Page Classifier using GET
%%  Params:
%%    Args  - options to use for the call
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_analyze(Args :: #dfbargs{}) -> dbfa_resp().
diffbot_analyze(Args) when is_record(Args, dfbargs) ->
	call_api(analyze, Args#dfbargs{method = get}, [fields, mode, stats]).


%%%----------------------------------------------------------------------
%%% Crawlbot API: http://diffbot.com/dev/docs/crawl/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Create a crawl bot.
%%  Params:
%%    Args   - options to use for the call
%%    Name   - job name
%%    Seeds  - urls to analyze (space separated)
%%    ApiUrl - API to use for each url
%%    Opts - (maybe empty) list of optional values to pass to a bot;
%%            each record is a string of form "field=value",
%%            to be inserted into final GET request as is
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_crawl_new(Args :: #dfbargs{}, Name :: string(), Seeds :: string(), 
					ApiUrl :: string(), Opts :: [string()]) -> dbfa_resp().
diffbot_crawl_new(Args, Name, Seeds, ApiUrl, Opts) when is_record(Args, dfbargs) ->
	call_api(crawl, Args#dfbargs{method = get},
			[{opts, Opts}, {name, Name}, {seeds, Seeds}, {apiUrl, ApiUrl}]).

%%-----------------------------------------------------------------------
%% @doc
%%  View a crawl bot.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_crawl_view(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_crawl_view(Args, Name) when is_record(Args, dfbargs) ->
	call_api(crawl, Args#dfbargs{method = get}, [{name, Name}]).

%%-----------------------------------------------------------------------
%% @doc
%%  Delete a crawl bot.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_crawl_delete(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_crawl_delete(Args, Name) when is_record(Args, dfbargs) ->
	diffbot_crawl_opt(Args, Name, ["delete=1"]).

%%-----------------------------------------------------------------------
%% @doc
%%  Pause a crawl bot.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_crawl_pause(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_crawl_pause(Args, Name) when is_record(Args, dfbargs) ->
	diffbot_crawl_opt(Args, Name, ["pause=1"]).

%%-----------------------------------------------------------------------
%% @doc
%%  Resume a crawl bot.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_crawl_resume(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_crawl_resume(Args, Name) when is_record(Args, dfbargs) ->
	diffbot_crawl_opt(Args, Name, ["pause=0"]).

%%-----------------------------------------------------------------------
%% @doc
%%  Download a crawl bot.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_crawl_download(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_crawl_download(Args, Name) when is_record(Args, dfbargs) ->
	Q = lists:concat(["http://api.diffbot.com/v2/crawl/download/",
					  http_uri:encode(Args#dfbargs.token), '-', 
					  http_uri:encode(Name), "_data.json"]),
	reply(httpc:request(get, {Q, []}, [{timeout, Args#dfbargs.timeout}], []), Args).

-spec diffbot_crawl_opt(Args :: #dfbargs{}, Name :: string(), Opts :: [string()]) -> dbfa_resp().
diffbot_crawl_opt(Args, Name, Opts) when is_record(Args, dfbargs) ->
	call_api(crawl, Args#dfbargs{method = get},
			[{opts, Opts}, {name, Name}]).


%%%----------------------------------------------------------------------
%%% Bulk API: http://diffbot.com/dev/docs/bulk/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Create a bulk job.
%%  Params:
%%    Args   - options to use for the call
%%    Name   - job name
%%    Urls   - urls to analyze (space separated)
%%    ApiUrl - API to use for each url
%%    Opts - (maybe empty) list of optional values to pass to a bot;
%%            each record is a string of form "field=value",
%%            to be inserted into final GET request as is
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_bulk_new(Args :: #dfbargs{}, Name :: string(), Urls :: string(), 
					ApiUrl :: string(), Opts :: [string()]) -> dbfa_resp().
diffbot_bulk_new(Args, Name, Urls, ApiUrl, Opts) when is_record(Args, dfbargs) ->
	call_api(bulk, Args#dfbargs{method = get},
			[{opts, Opts}, {name, Name}, {urls, Urls}, {apiUrl, ApiUrl}]).

%%-----------------------------------------------------------------------
%% @doc
%%  View a bulk job.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_bulk_view(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_bulk_view(Args, Name) when is_record(Args, dfbargs) ->
	call_api(bulk, Args#dfbargs{method = get}, [{name, Name}]).

%%-----------------------------------------------------------------------
%% @doc
%%  Delete a bulk job.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_bulk_delete(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_bulk_delete(Args, Name) when is_record(Args, dfbargs) ->
	diffbot_bulk_opt(Args, Name, ["delete=1"]).

%%-----------------------------------------------------------------------
%% @doc
%%  Pause a bulk job.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_bulk_pause(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_bulk_pause(Args, Name) when is_record(Args, dfbargs) ->
	diffbot_bulk_opt(Args, Name, ["pause=1"]).

%%-----------------------------------------------------------------------
%% @doc
%%  Resume a bulk job.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_bulk_resume(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_bulk_resume(Args, Name) when is_record(Args, dfbargs) ->
	diffbot_bulk_opt(Args, Name, ["pause=0"]).

%%-----------------------------------------------------------------------
%% @doc
%%  Download a bulk job.
%%  Params:
%%    Args - options to use for the call
%%    Name - job name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_bulk_download(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_bulk_download(Args, Name) when is_record(Args, dfbargs) ->
	Q = lists:concat(["http://api.diffbot.com/v2/bulk/download/",
					  http_uri:encode(Args#dfbargs.token), '-', 
					  http_uri:encode(Name), "_data.json"]),
	reply(httpc:request(get, {Q, []}, [{timeout, Args#dfbargs.timeout}], []), Args).

-spec diffbot_bulk_opt(Args :: #dfbargs{}, Name :: string(), Opts :: [string()]) -> dbfa_resp().
diffbot_bulk_opt(Args, Name, Opts) when is_record(Args, dfbargs) ->
	call_api(bulk, Args#dfbargs{method = get},
			[{opts, Opts}, {name, Name}]).


%%%----------------------------------------------------------------------
%%% Custom APIs: http://diffbot.com/dev/docs/custom/
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Access to custom APIs using GET.
%%  Params:
%%    Args - options to use for the call
%%    Name - Custom API name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_custom_get(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_custom_get(Args, Name) when is_record(Args, dfbargs) ->
	Q = lists:concat(["http://www.diffbot.com/api/", Name, 
			"?token=", http_uri:encode(Args#dfbargs.token), 
			"&url=", http_uri:encode(Args#dfbargs.url)]),
	reply(httpc:request(get, {Q, []}, [{timeout, Args#dfbargs.timeout}], []), Args).

%%-----------------------------------------------------------------------
%% @doc
%%  Access to custom APIs using POST.
%%  Params:
%%    Content - content to send
%%    Args - options to use for the call
%%    Name - Custom API name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_custom_post(Content :: string() | binary(), Args :: #dfbargs{}, 
						  Name :: string()) -> dbfa_resp().
diffbot_custom_post(Content, Args, Name) when is_record(Args, dfbargs) ->
	diffbot_custom_post(Args#dfbargs{content = Content}, Name).

%%-----------------------------------------------------------------------
%% @doc
%%  Access to custom APIs using POST.
%%  Params:
%%    Args - options to use for the call
%%    Name - Custom API name
%% @end
%%-----------------------------------------------------------------------
-spec diffbot_custom_post(Args :: #dfbargs{}, Name :: string()) -> dbfa_resp().
diffbot_custom_post(Args, Name) when is_record(Args, dfbargs) ->
	Q = lists:concat(["http://www.diffbot.com/api/", Name, 
			"?token=", http_uri:encode(Args#dfbargs.token), 
			"&url=", http_uri:encode(Args#dfbargs.url)]),
	reply(httpc:request(post, {Q, [], "text/html", Args#dfbargs.content},
						[{timeout, Args#dfbargs.timeout}], []), Args).

%%-----------------------------------------------------------------------
%% @doc
%%  Construct a request.
%%  Params:
%%    ApiUrl - Diffbot API Url
%%    Args   - call Args
%%    Params - list of values, each represents param to pass to Diffbot API
%%             in addition to token and url params; each value is of type
%%             field_set() from dfbcli.hrl, and is either atom for most
%%             API calls, or {key, value} pair for Crawlbot/Bulk APIs
%% @end
%%-----------------------------------------------------------------------
-spec make_api_url(ApiUrl :: string(), Args :: #dfbargs{}, Params :: [field_set()]) -> string().
make_api_url(ApiUrl, Args, Params) ->
	Tail = ["token=", http_uri:encode(Args#dfbargs.token), "&url=", http_uri:encode(Args#dfbargs.url)],
	L = lists:foldl(fun (P, Acc) -> make_arg(P, Args, Acc) end, Tail, Params),
	%io:format("~p ~p: ~s~n", [?MODULE, ?LINE, lists:concat([ApiUrl, '?' | L])]),
	lists:concat([ApiUrl, '?' | L]).

%%%----------------------------------------------------------------------
%%% Internal api
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Map api ID to url
%% @end
%%-----------------------------------------------------------------------
-spec api_url(api_set()) -> string().
api_url(front) -> ?DBFA_FRONT_URL;
api_url(image) -> ?DBFA_IMAGE_URL;
api_url(product) -> ?DBFA_PRODUCT_URL;
api_url(analyze) -> ?DBFA_ANALYZE_URL;
api_url(crawl) -> ?DBFA_CRAWL_URL;
api_url(bulk) -> ?DBFA_BULK_URL;
api_url(article) -> ?DBFA_ART_URL.

%%-----------------------------------------------------------------------
%% @doc
%%  Fulfil a request.
%%  Params:
%%    ApiId  - one of api_set() from dfbcli.hrl
%%    Args   - call Args
%%    Params - list of atoms, each represents param to pass to Diffbot API
%%             in addition to token and url params (of field_set() from dfbcli.hrl)
%% @end
%%-----------------------------------------------------------------------
-spec call_api(ApiId :: api_set(), Args :: #dfbargs{}, Params :: [field_set()]) -> dbfa_resp().
call_api(ApiId, Args = #dfbargs{method = get}, Params) ->
	reply(httpc:request(get,  {make_api_url(api_url(ApiId), Args, Params), []}, 
						[{timeout, Args#dfbargs.timeout}], []), Args);

call_api(ApiId, Args = #dfbargs{method = post}, Params) ->
	reply(httpc:request(post, {make_api_url(api_url(ApiId), Args, Params), [], "text/html", Args#dfbargs.content}, 
						[{timeout, Args#dfbargs.timeout}], []), Args).

%%-----------------------------------------------------------------------
%% @doc
%%  Convert text reply into json object, or format error reason on fail.
%% @end
%%-----------------------------------------------------------------------
-spec reply({'ok', {{string(), integer(), string()}, [{string(), string()}], string() | binary()} | 
					{integer(), string() | binary()}} | 
			{error, term()}, Args :: #dfbargs{}) -> dbfa_resp().

reply({ok, {{_HttpVersion, 200, _}, _RespHeaders, Body}}, A) ->
	reply_parse(Body, A);
reply({ok, {200, Body}}, A) ->
	reply_parse(Body, A);
reply({ok, {StatusCode, _Body}}, _A) ->
	{error, http_code, StatusCode};
reply({ok, {{_HttpVersion, StatusCode, _}, _RespHeaders, _Body}}, _A) ->
	{error, http_code, StatusCode};
reply({ok, _RawResp}, _A) ->
	{error, syntax, _RawResp};
reply({error, Reason}, _A) ->
	{error, general, Reason}.

-spec reply_parse(Body :: string() | binary(), Args :: #dfbargs{}) -> dbfa_resp().
reply_parse(Body, #dfbargs{format = Fmt}) when Fmt =/= json ->
	{ok, raw, Body}; % raw response, do not try to parse
reply_parse(Body, A) when is_binary(Body) ->
	reply_parse(binary_to_list(Body), A);
reply_parse(Body, _A) ->
	try
		case json2:decode_string(Body) of
			{error, Why} -> {error, json_decode, Why};
			R -> R
		end
	catch
		error:Reason -> {error, json_decode, Reason}
	end.

-spec make_arg(field_set(), #dfbargs{}, [term()]) -> [term()].
make_arg({_, []}, _Args, Acc) -> Acc;
make_arg({P = name, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = seeds, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = urls, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = apiUrl, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({opts, V}, _Args, Acc) when is_list(V) -> [string:join(V, "&"), '&' | Acc];
make_arg(P = format, Args, Acc) -> [P, '=', Args#dfbargs.format, '&' | Acc];
make_arg(stats, _, Acc) -> [stats, '&' | Acc];
make_arg(mode, #dfbargs{mode = undefined}, Acc) -> Acc;
make_arg(P = mode, Args, Acc) -> [P, '=', Args#dfbargs.mode, '&' | Acc];
make_arg(fields, #dfbargs{fields = []}, Acc) -> Acc;
make_arg(P = fields, Args, Acc) -> [P, '=', http_uri:encode(string:join(Args#dfbargs.fields, ",")), '&' | Acc].

%%-----------------------------------------------------------------------
