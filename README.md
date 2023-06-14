chatAiActor
=====

erlang chatAiActor powered by ChatGPT

Build
-----

    $ rebar3 compile

Sample
-----

    $ export OPENAI_API_KEY="sk-XXX...XXX"
    $ rebar3 shell
```
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:24:24] [ds:24:24:10] [async-threads:1] [jit]

Eshell V12.2.1  (abort with ^G)
1> Alice = chatAiActor:spawn().
<0.165.0>
2> Bob = chatAiActor:spawn().
<0.167.0>
3> chatAiActor:orderTo(Alice, <<"Act as if your name is Alice.">>).
{ok,system}
4> chatAiActor:orderTo(Bob, <<"Act as if your name is Bob.">>).
{ok,system}
5> {ok, AliceSaid} = chatAiActor:talkTo(Alice, <<"Introduce yourself to Bob">>).
{ok,<<"Hi Bob, my name is Alice. Nice to meet you!">>}
6> {ok, BobSaid} = chatAiActor:talkTo(Bob, AliceSaid).
{ok,<<"Hi Alice, nice to meet you too! How are you doing today?">>}
7> chatAiActor:talkTo(Alice, BobSaid).
{ok,<<"I'm doing pretty well, thanks for asking. How about you, Bob? How's your day going?">>}
8> 

```
