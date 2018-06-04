-module(esegment).

-include("segment.hrl").

%% API exports
-export([setup/1]).
-export([track/3, track/4, track/1]).
-export([identify/2, identify/3, identify/1]).
-export([alias/2, alias/3, alias/1]).
-export([page/3, page/4, page/1]).
-export([screen/3, screen/4, screen/1]).
-export([group/3, group/4, group/1]).


-export_type([track/0, identify/0, alias/0, page/0, screen/0, group/0]).
-export_type([context/0, app/0, campaign/0, device/0, library/0, location/0, network/0, os/0, page_ctx/0, referrer/0, screen_ctx/0]).


-type ok_resp() :: {ok, term()}.
-type error_resp() :: {error, term()}.
-type resp() :: ok_resp() | error_resp().
%%====================================================================
%% API functions
%%====================================================================
-spec setup(string()) -> ok.
setup(WriteKey) ->
    setup(WriteKey, <<"esegment">>, <<"0.11">>).

-spec setup(string(), binary(), binary()) -> ok.
setup(WriteKey, AppName, AppVersion) ->
    application:set_env(esegment, write_key, WriteKey),
    application:set_env(esegment, 'app.name', AppName),
    application:set_env(esegment, 'app.version', AppVersion).
.

%% Track API
-spec track(user_id(), event(), properties()) -> resp().
track(UserId, Event, Properties) ->
    track(UserId, Event, Properties, default_context()).

-spec track(user_id(), event(), properties(), context()) -> resp().
track(UserId, Event, Properties, Context) ->
    esegment_http:post(track, #{
        userId => UserId,
        event => Event,
        properties => Properties,
        context => Context
    }, write_key()).

-spec track(track()) -> resp().
track(TrackObject) ->
    esegment_http:post(track, TrackObject, write_key()).

%% Identify API
-spec identify(user_id(), traits()) -> resp().
identify(UserId, Traits) ->
    identify(UserId, Traits, default_context()).

-spec identify(user_id(), traits(), context()) -> resp().
identify(UserId, Traits, Context) ->
    esegment_http:post(identify, #{
        userId => UserId,
        traits => Traits,
        context => Context
    }, write_key()).

-spec identify(identify()) -> resp().
identify(Identify) ->
    esegment_http:post(identify, Identify, write_key()).

%% Alias API
-spec alias(user_id(), previous_id()) -> resp().
alias(UserId, PreviousId) ->
    alias(UserId, PreviousId, default_context()).

-spec alias(user_id(), previous_id(), context()) -> resp().
alias(UserId, PreviousId, Context) ->
    esegment_http:post(alias, #{
        userId => UserId,
        previousId => PreviousId,
        context => Context
    }, write_key()).

-spec alias(alias()) -> resp().
alias(AliasObj) ->
    esegment_http:post(alias, AliasObj, write_key()).

%% Page API
-spec page(user_id(), name(), properties()) -> resp().
page(UserId, Name, Properties) ->
    page(UserId, Name, Properties, default_context()).

-spec page(user_id(), name(), properties(), context()) -> resp().
page(UserId, Name, Properties, Context) ->
    esegment_http:post(page, #{
        userId => UserId,
        name => Name,
        properties => Properties,
        context => Context
    }, write_key()).

-spec page(page()) -> resp().
page(Page) ->
    esegment_http:post(page, Page, write_key()).

%% Screen API
-spec screen(user_id(), name(), properties()) -> resp().
screen(UserId, Name, Properties) ->
    screen(UserId, Name, Properties, default_context()).

-spec screen(user_id(), name(), properties(), context()) -> resp().
screen(UserId, Name, Properties, Context) ->
    esegment_http:post(screen, #{
        userId => UserId,
        name => Name,
        properties => Properties,
        context => Context
    }, write_key()).

-spec screen(screen()) -> resp().
screen(Screen) ->
    esegment_http:post(screen, Screen, write_key()).

%% Group API
-spec group(user_id(), group_id(), traits()) -> resp().
group(UserId, GroupId, Traits) ->
    group(UserId, GroupId, Traits, default_context()).

-spec group(user_id(), group_id(), traits(), context()) -> resp().
group(UserId, GroupId, Traits, Context) ->
    esegment_http:post(group, #{
        userId => UserId,
        groupId => GroupId,
        traits => Traits,
        context => Context
    }, write_key()).

-spec group(group()) -> resp().
group(Group) ->
    esegment_http:post(group, Group, write_key()).


%%====================================================================
%% Internal functions
%%====================================================================
write_key() ->
    application:get_env(esegment, write_key).

default_context() ->
    #{
        app => default_app()
    }.


default_app() ->
    #{
        name => application:get_env(esegment, 'app.name'),
        version => application:get_env(esegment, 'app.version')
    }.