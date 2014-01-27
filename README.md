# Diffbot API Erlang client

## Installation

The project is a code base, not an application, so it assumes that you will place source files from the code base of the library to your Erlang project and use further according to examples found in dfbcli_tests.erl. Diffbot API Erlang client has no external dependencies, but it includes the 3rd party library to deal with JSON (json2.erl).

Diffbot API Erlang client comprises 3 core files:

* dfbcli.erl - api calls/wrappers
* dfbcli.hrl - defines
* json2.erl  - 3rd party json library

and an additional source file containing exhaustive samples for each section of Diffbot API:

* dfbcli_tests.erl

### HTTP Client

The project uses the httpc module coming with Erlang/OTP distribution as a HTTP library, so the HTTP Client API is available when the `inets` application is started. This means that after inserting source codes into your project you should add to your Erlang/OTP application dependency from `inets` application. The application resource file should list inets within applications which must be started before your application is allowed to be started, like in:

```erlang
  {application, YOUR_APP,
   [{description, "..."},
    ...
    {applications, [kernel, stdlib, sasl, inets]},
    ...
   ]}.
```

## Configuration

Diffbot API options, including the developer token, are set with help of `dfbargs` record, defined and described in dfbcli.hrl

* Minimal set of options includes fields `url` (the resource to analyze) and `token` (the developer token). As an option you may wish to redefine DBFA_TOKEN macros.
* You should also wish to tune `fields` to specify fields in response for those Diffbot APIs where it is applicable (Article API, Image API, Product API, Page Classifier API). You can use DBFA_ALL, DBFA_DEFAULT, DBFA_TEST_META macros and define yours for combination of fields which occur fairly often.
* For Diffbot APIs with support of method POST, a field `content` is used to pass a body of the request; alternatively you can pass the body as an additional parameter in the corresponding function call, that works actually as a wrapper above a `content` field.
* To use Diffbot Frontpage API you may want to use a `format` option (that is `json` by default).

Once you configure options record, a set of actual requests is available to use.

## Usage

Diffbot API Erlang client supports Article API, Frontpage API, Image API, Product API, Page Classifier API, Crawlbot API and Bulk API.

### Article API

The Diffbot API Erlang client has the following wrappers around Article API:

```erlang
-export([diffbot/1, diffbot/2, diffbot_post/1, diffbot_post/2]).
```

If we already have Args record configured, the query can be as simple as

```erlang
R = dfbcli:diffbot(Args),
```

with further check/print of response as

```erlang
case R of
    {ok, Resp} ->
        io:format("Json object is:~n~p~n", [Resp]);
    {error, Why, Details} ->
        io:format("error: ~p ~p: ~p: ~p~n", [?MODULE, ?LINE, Why, Details])
end,
```

A variable `Resp` is a parsed JSON object that has a structure described in `json2.erl`

You can also POST the request as

```erlang
R = dfbcli:diffbot_post(Args#dfbargs{content = Content, fields = ?DBFA_TEST_META}),
```

to obtain a meta info fields from the sent `Content`

### Analyze API

Page Classifier API is wrapped as `-export([diffbot_analyze/1]).` in the Diffbot API Erlang client. The request is quite similar to the previous example. In order to extract content only from that particular page-type and to choose the fields to be returned by the Diffbot:

```erlang
R = dfbcli:diffbot_analyze(Args#dfbargs{fields = ["meta", "tags"], mode = article}),
case R of
    {ok, Resp} ->
        io:format("Json object is:~n~p~n", [Resp]);
    {error, Why, Details} ->
        io:format("error: ~p ~p: ~p: ~p~n", [?MODULE, ?LINE, Why, Details])
end,
```

### Crawlbot and Bulk API

Crawlbot and Bulk API have quite similar set of functions:

```erlang
-export([diffbot_crawl_new/5, diffbot_crawl_delete/2, diffbot_crawl_pause/2,
		 diffbot_crawl_resume/2, diffbot_crawl_download/2, diffbot_crawl_view/2]).
```

and

```erlang
-export([diffbot_bulk_new/5, diffbot_bulk_delete/2, diffbot_bulk_pause/2,
		 diffbot_bulk_resume/2, diffbot_bulk_download/2, diffbot_bulk_view/2]).
```

and introduce a concept of raw key-value parameters to pass through the request and refine your crawl, e.g.:

```erlang
Name = "myjob",
Sites = "http://en.wikipedia.org/wiki/Parma http://en.wikipedia.org/wiki/Emilia-Romagna http://en.wikipedia.org/wiki/Romagna http://en.wikipedia.org/wiki/Modena",
ApiUrl = "http://api.diffbot.com/v2/product?fields=querystring,meta",
Props = ["maxToCrawl=2", "maxToProcess=2"],
% create crawlbot
R1 = dfbcli:diffbot_crawl_new(Args, Name, Sites, ApiUrl, Props),
io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R1]),
% view crawlbot
R2 = dfbcli:diffbot_crawl_view(Args, Name),
io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R2]),
% pause crawlbot
R3 = dfbcli:diffbot_crawl_pause(Args, Name),
io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R3]),
% resume crawlbot
R4 = dfbcli:diffbot_crawl_resume(Args, Name),
io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R4]),
% download results of crawlbot work
R5 = dfbcli:diffbot_crawl_download(Args, Name),
io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R5]),
% delete crawlbot
R6 = dfbcli:diffbot_crawl_delete(Args, Name),
io:format("~p ~p: ~p~n", [?MODULE, ?LINE, R6]).
```

In this example `Props` is a list (maybe empty) of optional values to pass to a bot, thus refining the crawl.

### More examples

Please see dfbcli_tests.erl for more examples.
