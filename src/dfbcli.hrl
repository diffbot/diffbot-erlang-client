%%%----------------------------------------------------------------------
%%% Diffbot API Erlang Client Library
%%% 
%%%----------------------------------------------------------------------

-define(DBFA_TOKEN, "...").												% insert your default developer token here

-define(DBFA_TIMEOUT, 30000).											% 30 secs

% Target API Urls
-define(DBFA_ART_URL, "http://api.diffbot.com/v2/article").
-define(DBFA_FRONT_URL, "http://www.diffbot.com/api/frontpage").
-define(DBFA_IMAGE_URL, "http://api.diffbot.com/v2/image").
-define(DBFA_PRODUCT_URL, "http://api.diffbot.com/v2/product").
-define(DBFA_ANALYZE_URL, "http://api.diffbot.com/v2/analyze").
-define(DBFA_CRAWL_URL, "http://api.diffbot.com/v2/crawl").
-define(DBFA_BULK_URL, "http://api.diffbot.com/v2/bulk").

% Shortcuts for a set of fields:

% returns all fields available, including experimental fields
-define(DBFA_ALL, ["*"]).

% returns by default
-define(DBFA_DEFAULT, ["url", "resolved_url", "icon", "type", "title", "text", "html", "date", "author", "images", "videos"]).

% meta info
-define(DBFA_TEST_META, ["url", "resolved_url", "icon", "type", "title", "date", "author"]).

-record(dfbargs,
	{url					:: string(),								% url to analyze
	 token = ?DBFA_TOKEN	:: string(),								% developer token
	 fields = []			:: [string()],								% fields to request
	 version = 2			:: pos_integer(),							% api version
	 format = json			:: 'undefined' | 'xml' | 'json',			% response format used by Frontpage API
	 mode = undefined		:: atom(),									% response format used by Page Classifier API
	 method = undefined		:: 'undefined' | 'get' | 'post',			% method to use: GET or POST
	 content = <<>>			:: string() | binary(),						% content to send when using POST method
	 timeout = 'infinity'	:: non_neg_integer() | 'infinity'			% httpc timeout
	}).

-type(json() :: {struct, [{string() | binary(), term()}]} | {array, list()} | string() | number()).
-type(dbfa_resp() :: {ok, json()} | {ok, raw, string() | binary()} | {error, 'http_code' | 'syntax' | 'general', term()}).
-type(api_set() :: 'article' | 'front' | 'image' | 'product' | 'analyze' | 'crawl' | 'bulk').
-type(field_set() :: 'format' | 'fields' | 'mode' | 'stats' | {'opts', [string()]} |
					 {'name', string()} | {'seeds', string()} | {'urls', string()} | {'apiUrl', string()}
	 ).

%%%----------------------------------------------------------------------
