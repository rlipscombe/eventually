# eventually

Assertions in an asynchronous world.

## Background

In [this blog post](https://blog.differentpla.net/blog/2020/09/14/erlang-common-test/), I wrote about how Electric Imp
uses Erlang's Common Test for system testing.

Because the system is asynchronous, we needed a way to assert results without running into race conditions. To do this,
we invented `assert:eventually()`, which might look something like this:

```erlang
%...
assert:eventually(device:receives_code(Device, ExpectedCode)),
%...
```

The intent here is that we poll a condition in a loop, retrying until it passes, or until the number of attempts exceeds
some threshold. This allows us to avoid race conditions by giving the system under test a chance to catch up with the
test.

I decided to reimplement this mechanism in a new project, for two reasons:

1. We never open-sourced the `assert:eventually()` code from Electric Imp, and I no longer have access to it.
2. It was a bit of a mess in places. Here's hoping this project is less of a mess.

## Naming of the project

Because `assert` isn't a great project name, this project is called `eventually` instead. So it's `eventually:assert()`,
rather than `assert:eventually()`.

## Running self-tests

```sh
rebar3 eunit
```

## Simple conditions

For example:

```erlang
    eventually:assert(server_is_available(Host, Port)).
```

Because the condition needs to be polled in a loop, rather than evaluated only once, it must be a function. We call this
a "probe".

The `server_is_available/2` function returns a zero-arity function. This is sugar, to make the test above easier to
read:

```erlang
server_is_available(Host, Port) ->
    fun() ->
        {ok, Socket} = gen_tcp:connect(Host, Port, [], 1_000),
        gen_tcp:close(Socket),
        true
    end.
```

Any exception thrown by the probe is treated as a failure, which causes a retry. This simplifies writing the probe.

## Separate probe and matcher

Some tests are slightly more readable or reusable if you separate the probe from the condition (which we call a "matcher"):

```erlang
    eventually:assert(assert_http:get(Url), assert_http:has_body(<<"OK">>)).
```

```erlang
-module(assert_http).
-export([get/1]).
-export([has_body/1]).

get(Url) ->
    fun() ->
        httpc:request(Url),
    end.

has_body(Expected) ->
    fun({ok, {{_, 200, _}, _Headers, Body}}) -> Body =:= Expected end.
```

The above has the result from `httpc:request/1` being returned directly from the probe. Alternatively, we could have done a bit of reframing:

```erlang
get(Url) ->
    fun() ->
        {ok, {{_, 200, _}, Headers, Body}} = httpc:request(Url),
        #{headers => Headers, body => Body}
    end.

has_body(Expected) ->
    fun(#{body := Body}) -> Body =:= Expected end.
```

It's a style thing. Neither is particularly better than the other.

This allows you to avoid repetition of the HTTP client code, while matching on different results:

```erlang
    eventually:assert(assert_http:get(Url), assert_http:has_body(<<"Not found">>)).
```

## Accumulating probe

Sometimes you want to collect the results from the probe:

```erlang
    % Wait until three messages are received
    eventually:assert(messages_received(), has_count(3)).
```

The above can either be done by collecting the messages, or by incrementing a counter. Either way, you need to pass
state from one call to the probe to the next.

## Returning a value from the assertion

Once you've polled for a particular condition, you might want to perform more assertions. To enable this, you can return
a value from either the probe (if it's a simple probe) or from the matcher (if there is one). For example:

```erlang
    Body = eventually:assert(assert_http:get(Url), assert_http:has_status_code(200)).
    ?assertMatch(#{<<"user_roles">> := [<<"admin">>]}, jsx:decode(Body)).
```

This allows polling until you get a well-formed result, without retrying if it doesn't match the expectation.

## Future

- Those examples above ^^ ought to be tested, somehow.
- It would be nice if failure reports included (at least) the last value passed to the matcher.
- Do we want to allow configurable (exponential) backoff for retries?
- Combinators: `all` and `any`, e.g.
