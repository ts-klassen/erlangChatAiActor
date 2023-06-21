-module(chatAiActor).

-export([spawn/0, talkTo/2, orderTo/2, addFunctionTo/5]).
-export([get/2, set/3, loop/4]).
-define(TIMEOUT, 600000).

spawn() -> spawn(chatAiActor, loop, [
  [], 
  <<"gpt-3.5-turbo-0613">>, 
  0.7, 
  {[],#{}}
]).

talkTo(Pid, Content) ->
  Pid ! {self(), chat, Content},
  receive
    {Pid, Reply} ->
      {ok, Reply}
    after ?TIMEOUT ->
      {error, timeout}
  end.

orderTo(Pid, Content) ->
  Pid ! {self(), system, Content},
  receive
    {Pid, ok} ->
      {ok, system}
    after ?TIMEOUT ->
      {error, timeout}
  end.

addFunctionTo(Pid, Name, Description, Parameters, Fun) ->
  Pid ! {self(), function, Name, Description, Parameters, Fun},
  receive
    {Pid, function, Res} ->
      Res
    after ?TIMEOUT ->
      {error, timeout}
  end.

get(Pid, Key) ->
  Pid ! {self(), getter},
  receive
    {Pid, getter, Value} -> 
      maps:find(Key, Value)
    after ?TIMEOUT ->
      {error, timeout}
  end.

set(Pid, Key, Value) ->
  Pid ! {self(), setter, Key, Value},
  receive
    {Pid, setter, Res} ->
      Res
    after ?TIMEOUT ->
      {error, timeout}
  end.

loop(Messages, Model, Temperature, Functions) ->
  receive
    {From, system, Content} ->
      From ! {self(), ok},
      NextMessages = addMessage(Messages, system, Content),
      loop(NextMessages, Model, Temperature, Functions);
    {From, chat, Content} ->
      SendMessage = addMessage(Messages, user, Content),
      {Reply, NextMessages} = openaiRequest(
        SendMessage, Model, Temperature, Functions
      ),
      From ! {self(), Reply},
      loop(NextMessages, Model, Temperature, Functions);
    {From, function, Name, Description, Parameters, Fun} ->
      NewFuns = addFunction(Functions, Name, Description, Parameters, Fun),
      From ! {self(), function, ok},
      loop(Messages, Model, Temperature, NewFuns);
    
    % getters and setters
    {From, getter} ->
      From ! {self(), getter, #{
        messages => Messages,
        model => Model,
        temperature => Temperature,
        functions => Functions
      }},
      loop(Messages, Model, Temperature, Functions);
    {From, setter, messages, NewMessages} ->
      From ! {self(), setter, ok},
      loop(NewMessages, Model, Temperature, Functions);
    {From, setter, model, NewModel} ->
      From ! {self(), setter, ok},
      loop(Messages, NewModel, Temperature, Functions);
    {From, setter, temperature, NewTemperature} ->
      From ! {self(), setter, ok},
      loop(Messages, Model, NewTemperature, Functions);
    {From, setter, functions, NewFunctions} ->
      From ! {self(), setter, ok},
      loop(Messages, Model, Temperature, NewFunctions);
    {From, setter, _Key, _Value} ->
      From ! {self(), setter, error},
      loop(Messages, Model, Temperature, Functions)
  end.

openaiRequest(Messages, Model, Temperature, {[],#{}}) ->
  Functions = addFunction(
    {[],#{}}, 
    <<"erlang_system_info">>,
    <<"get system info of erlang this is running on. returns the result of erlang:system_info(arg1)">>,
    [
      {<<"arg1">>, <<"string">>, <<"argument of system_info/1. e.g. erlang atom 'otp_release' to get the version of Erlang/OTP">>}
    ],
    fun(Args) -> 
      {ok, Arg1} = maps:find(<<"arg1">>, Args),
      Res = erlang:system_info(binary_to_atom(Arg1)),
      if
      is_list(Res) -> list_to_binary(Res);
      true -> Res
      end
    end
  ),
  openaiRequest(Messages, Model, Temperature, Functions);

openaiRequest(Messages, Model, Temperature, {FunData, Funs}) ->
  Authorization = "Bearer " ++ os:getenv("OPENAI_API_KEY"),
  application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open("api.openai.com", 443),
  StreamRef = gun:post(ConnPid, <<"/v1/chat/completions">>, [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"Authorization">>, Authorization}
  ]),
  Json = jsone:encode(#{
    <<"model">> => Model,
    <<"messages">> => lists:reverse(Messages),
    <<"temperature">> => Temperature,
    <<"functions">> => FunData
  }),
  ok = gun:data(ConnPid, StreamRef, fin, Json),
  ok = case gun:await(ConnPid, StreamRef, ?TIMEOUT) of
    {response, nofin, 200, _Header} -> ok;
    {response, nofin, _Status, _Header} ->
      error = gun:await_body(ConnPid, StreamRef);
    {response, _, _, _} -> error
  end,
  {ok, Body} = gun:await_body(ConnPid, StreamRef),
  gun:close(ConnPid),
  {ok, [Choice]} = maps:find(<<"choices">>, jsone:decode(Body)),
  {ok, Message} = maps:find(<<"message">>, Choice),
  {ok, Content} = maps:find(<<"content">>, Message),
  case Content of
    null ->
      {ok, FunctionCall} = maps:find(<<"function_call">>, Message),
      {ok, Name} = maps:find(<<"name">>, FunctionCall),
      {ok, Args} = maps:find(<<"arguments">>, FunctionCall),
      {ok, Fun} = maps:find(Name, Funs),
      OnErrorRes = jsone:encode(#{
        <<"success">> => <<"false">>,
        <<"deail">> => <<"Function threw an error.">>
      }),
      Res = try jsone:encode(Fun(jsone:decode(Args))) of
        Any -> Any
      catch
        throw -> OnErrorRes;
        exit -> OnErrorRes;
        error -> OnErrorRes
      end,
      NextMessages = addMessage([Message|Messages], function, Name, Res),
      timer:sleep(1000),
      openaiRequest(NextMessages, Model, Temperature, {FunData, Funs});
    _any ->
      {Content, [Message|Messages]}
  end.

addMessage(Messages, user, Content) ->
  addMessage(Messages, <<"user">>, Content);
addMessage(Messages, assistant, Content) ->
  addMessage(Messages, <<"assistant">>, Content);
addMessage(Messages, system, Content) ->
  addMessage(Messages, <<"system">>, Content);
addMessage(Messages, Role, Content) ->
  [#{<<"role">> => Role, <<"content">> => Content} | Messages].
addMessage(Messages, function, Name, Content) ->
  [#{
    <<"role">> => <<"function">>, 
    <<"name">> => Name,
    <<"content">> => Content
  } | Messages].

addFunction({FunData, Funs}, Name, Description, Parameters, Fun) ->
  Data = #{
    <<"name">> => Name,
    <<"description">> => Description,
    <<"parameters">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => buildProperties(#{}, Parameters)
    }
  },
  {[Data | FunData], maps:put(Name, Fun, Funs)}.
  
  
buildProperties(Properties, []) -> Properties;
buildProperties(Properties, [{Name,Type,Description}|Parameters]) ->
  Property = #{
    <<"type">> => Type,
    <<"description">> => Description
  },
  NextProperties = maps:put(Name, Property, Properties),
  buildProperties(NextProperties, Parameters).
  
