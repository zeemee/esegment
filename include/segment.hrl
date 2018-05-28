

-type user_id()         :: binary().
-type group_id()        :: binary().
-type iso8601_binary()  :: binary().
-type event()           :: binary().
-type name()            :: binary().
-type previous_id()     :: binary().

-type ip()              :: binary().
-type locale()          :: binary().
-type timezone()        :: binary(). %% Example: America/New_York



-type properties() :: #{ atom() | binary() => binary() }.
-type traits() :: #{ atom() | binary() => binary() }.
-type integrations() :: #{ atom() | binary() => binary() }.

-opaque track() :: #{
    userId          => user_id(),
    event           => event(),
    properties      => properties(),

    context         => context(),           %% optional | default
    timestamp       => iso8601_binary(),    %% optional
    integrations    => integrations(),      %% optional
    anonymousId     => binary()             %% optional
}.

-opaque identify() :: #{
    userId          => user_id(),
    traits          => traits(),

    context         => context(),           %% optional | default
    timestamp       => iso8601_binary(),    %% optional
    integrations    => integrations(),      %% optional
    anonymousId     => binary()             %% optional
}.

-opaque alias() :: #{
    userId          => user_id(),
    previousId      => previous_id(),

    context         => context(),           %% optional | default
    timestamp       => iso8601_binary(),    %% optional
    integrations    => integrations(),      %% optional
    anonymousId     => binary()             %% optional
}.

-opaque page() :: #{
    userId          => user_id(),
    name            => name(),
    properties      => properties(),

    context         => context(),           %% optional | default
    timestamp       => iso8601_binary(),    %% optional
    integrations    => integrations(),      %% optional
    anonymousId     => binary()             %% optional
}.

-opaque screen() :: #{
    userId          => user_id(),
    name            => name(),
    properties      => properties(),

    context         => context(),           %% optional | default
    timestamp       => iso8601_binary(),    %% optional
    integrations    => integrations(),      %% optional
    anonymousId     => binary()             %% optional
}.

-opaque group() :: #{
    userId          => user_id(),
    groupId          => group_id(),
    traits          => traits(),

    context         => context(),           %% optional | default
    timestamp       => iso8601_binary(),    %% optional
    integrations    => integrations(),      %% optional
    anonymousId     => binary()             %% optional
}.


-opaque app() :: properties().          %% Dictionary of information about the current application, containing name, version and build.
-opaque campaign() :: properties().     %% Dictionary of information about the campaign that resulted in the API call, containing name, source, medium, term and content
-opaque device() :: properties().       %% Dictionary of information about the device, containing id, manufacturer, model, name, type and version
-opaque library() :: properties().      %% Dictionary of information about the library making the requests to the API, containing name and version
-opaque location() :: properties().     %% Dictionary of information about the user’s current location, containing city, country, latitude, longitude, region and speed
-opaque network() :: properties().      %% Dictionary of information about the current network connection, containing bluetooth, carrier, cellular and wifi
-opaque os() :: properties().           %% Dictionary of information about the operating system, containing name and version
-opaque page_ctx() :: properties().     %% Dictionary of information about the current page in the browser, containing hash, path, referrer, search, title and url
-opaque referrer() :: properties().     %% Dictionary of information about the way the user was referred to the website or app, containing type, name, url and link
-opaque screen_ctx() :: properties().   %% Dictionary of information about the device’s screen, containing density, height and width

-opaque context() :: #{
    active          => boolean(),   %% Whether a user is active. This is usually used to flag an .identify() call to just update the traits but not “last seen.”
    app             => app(),
    campaign        => campaign(),
    device          => device(),
    ip              => ip(),
    library         => library(),
    locale          => locale(),
    location        => location(),
    network         => network(),
    os              => binary(),
    page            => page_ctx(),
    referrer        => referrer(),
    screen          => screen_ctx(),
    timezone        => timezone(),
    traits          => traits(),
    userAgent       => binary()
}.


-record(segment, {
    write_key   :: binary()
}).