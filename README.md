chatAiActor
=====

erlang chatAiActor powered by ChatGPT

Build
-----

    $ rebar3 compile

Example
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


Function Calling Example
-----

```
Assistant = chatAiActor:spawn(),
chatAiActor:addFunctionTo(
  Assistant, % Pid
  <<"setColor">>, % name of function
  <<"set color of the room">>, % description of function
  [
    {
      <<"color">>, % name of property
      <<"string">>, % type of property
      <<"name of color. e.g. red, blue, white etc.">> % description
    }
  ], 
  fun(Args) -> % function to be called by ChatGPT
    {ok, Color} = maps:find(<<"color">>, Args),
    case Color of
      <<"blue">> ->
        #{
          <<"success">> => false,
          <<"detail">> => <<"Temporarily unavailable.">>
        };
      <<"red">> ->
        #{
          <<"success">> => false,
          <<"detail">> => <<"assistant is not in the sudoers file. This incident will be reported.">>
        };
      Any -> #{<<"success">> => true, <<"color">> => Any}
    end % end of case
  end % end of fun
).
```

```
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:24:24] [ds:24:24:10] [async-threads:1] [jit]

Eshell V12.2.1  (abort with ^G)
1> Assistant = chatAiActor:spawn(),
1> chatAiActor:addFunctionTo(
1>   Assistant, % Pid
1>   <<"setColor">>, % name of function
1>   <<"set color of the room">>, % description of function
1>   [
1>     {
1>       <<"color">>, % name of property
1>       <<"string">>, % type of property
1>       <<"name of color. e.g. red, blue, white etc.">> % description
1>     }
1>   ], 
1>   fun(Args) -> % function to be called by ChatGPT
1>     {ok, Color} = maps:find(<<"color">>, Args),
1>     case Color of
1>       <<"blue">> ->
1>         #{
1>           <<"success">> => false,
1>           <<"detail">> => <<"Temporarily unavailable.">>
1>         };
1>       <<"red">> ->
1>         #{
1>           <<"success">> => false,
1>           <<"detail">> => <<"assistant is not in the sudoers file. This incident will be reported.">>
1>         };
1>       Any -> #{<<"success">> => true, <<"color">> => Any}
1>     end % end of case
1>   end % end of fun
1> ).
ok
2> chatAiActor:talkTo(Assistant, <<"Can you change the room color to white?">>).
{ok,<<"The room color has been changed to white.">>}
3> chatAiActor:talkTo(Assistant, <<"Change the room color to blue.">>).
{ok,<<"I'm sorry, but it seems that changing the room color to blue is temporarily unavailable. Is there any other "...>>}
4> chatAiActor:talkTo(Assistant, <<"How about red?">>).
{ok,<<"I'm sorry, but I don't have the permission to change the room color to red. Is there any other color you wou"...>>}
5> 
```
