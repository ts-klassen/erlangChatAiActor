-module(chatAiActor).

-export([spawn/0, talkTo/2, orderTo/2, loop/1]).

spawn() -> spawn(chatAiActor, loop, [[]]).

talkTo(Pid, Content) ->
  Pid ! {self(), Content},
  receive
    {Pid, Reply} ->
      {ok, Reply}
    after 5000 ->
      {error, timeout}
  end.

orderTo(Pid, Content) ->
  Pid ! {self(), Content, system},
  receive
    {Pid, ok} ->
      {ok, system}
    after 5000 ->
      {error, timeout}
  end.

loop(Messages) ->
  receive
    {From, Content, system} ->
      From ! {self(), ok},
      loop(addMessage(Messages, system, Content));
    {From, Content} ->
      SendMessage = addMessage(Messages, user, Content),
      {Reply, NextMessages} = openaiRequest(SendMessage),
      From ! {self(), Reply},
      loop(NextMessages)
  end.

openaiRequest(Messages) ->
  openaiRequest(Messages, <<"gpt-3.5-turbo">>, 0.7).

openaiRequest(Messages, Model, Temperature) ->
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
    <<"temperature">> => Temperature
  }),
  ok = gun:data(ConnPid, StreamRef, fin, Json),
  {response, nofin, 200, _headers} = gun:await(ConnPid, StreamRef),
  {ok, Body} = gun:await_body(ConnPid, StreamRef),
  gun:close(ConnPid),
  {ok, [Choice]} = maps:find(<<"choices">>, jsone:decode(Body)),
  {ok, Message} = maps:find(<<"message">>, Choice),
  {ok, Content} = maps:find(<<"content">>, Message),
  {Content, addMessage(Messages, assistant, Content)}.

addMessage(Messages, user, Content) ->
  addMessage(Messages, <<"user">>, Content);
addMessage(Messages, assistant, Content) ->
  addMessage(Messages, <<"assistant">>, Content);
addMessage(Messages, system, Content) ->
  addMessage(Messages, <<"system">>, Content);
addMessage(Messages, Role, Content) ->
  [#{<<"role">> => Role, <<"content">> => Content} | Messages].
