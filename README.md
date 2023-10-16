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

## Future

- The probe might want to accumulate stuff, for example `messages_received()`.
- Do we want to allow configurable (exponential) backoff for retries?
- Labels/descriptions for the probe.
- Separating the probe from the condition/matcher.
- Returning a value from the assertion, for use later in the test.
