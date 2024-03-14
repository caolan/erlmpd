%% @author Caolan McMahon <caolan@caolanmcmahon.com>

-module(erlmpd).
-include("erlmpd.hrl").

%% Exported functions not part of the MPD API
-export([connect/0, connect/2, connect/3, command/2, command/3, command/4,
         commandlist/3, commandlist/2, version/1]).

%% Querying MPD's status
-export([clearerror/1, currentsong/1, idle/1, idle/2, status/1, stats/1]).

%% Playback options
-export([consume/2, crossfade/2, random/2, repeat/2, setvol/2, single/2]).

%% Controlling playback
-export([next/1, pause/2, play/1, play/2, playid/1, playid/2, previous/1,
         seek/3, seekid/3, stop/1]).

%% The current playlist
-export([add/2, addid/2, addid/3, clear/1, delete/2, deleteid/2,
         deleteids/2, move/3, moveid/3, playlist/1, playlistfind/3,
         playlistid/1, playlistid/2, playlistinfo/1, playlistinfo/2,
         playlistsearch/3, plchanges/2, plchangesposid/2, shuffle/2,
         shuffle/1, swap/3, swapid/3]).

%% Stored playlists
-export([listplaylist/2, listplaylistinfo/2, listplaylists/1, load/2,
         playlistadd/3, playlistclear/2, playlistdelete/3, playlistmove/4,
         rename/3, rm/2, save/2]).

%% The music database
-export([count/3, find/3, list/2, list/3, listall/1, listall/2, listallinfo/1,
         listallinfo/2, lsinfo/1, lsinfo/2, search/3, update/1, update/2]).

%% Stickers
-export([sticker/4, sticker/5, sticker/6]).

%% Connection settings
-export([close/1, kill/1, password/2, ping/1]).

%% Audio output devices
-export([disableoutput/2, enableoutput/2, outputs/1]).

%% Reflection
-export([commands/1, notcommands/1, tagtypes/1, urlhandlers/1]).

%% Default timeout when waiting for a response from MPD
-define(TIMEOUT, 5000).


%%==================================================================
%% Data types
%%==================================================================
%% @type mpd_conn() = #mpd_conn{port=port(), version=string()}.
%% @type mpd_error() = #mpd_error{errorid=string(), position=string(), description=string(), reason=string()}.
%% @type tag() = artist | albumartist | album | title | track | genre | disc | date

%%===================================================================
%% Exported functions not part of the MPD API
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (Addr::string(), Port::integer()) -> {ok, mpd_conn()}
%% @doc
%% Connects to an MPD server and returns the connected socket and
%% the version number of the server.
%% @end
%%-------------------------------------------------------------------
connect(Addr, Port) ->
    Resp = gen_tcp:connect(
        Addr, Port, [{active,false}, {packet,line}], ?TIMEOUT
    ),
    case Resp of
        {ok ,Sock} ->
            case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
                {ok, "OK MPD " ++ A} ->
                    {ok, #mpd_conn{port=Sock, version=A--"\n"}};
                {error, Error} -> {error, Error}
            end;
        {error, Error} -> {error, Error}
    end.

%%-------------------------------------------------------------------
%% @spec () -> {ok, mpd_conn()}
%% @doc
%% Attempts to connect using default settings. Same as calling
%% connect("localhost", 6600).
%% @end
%%-------------------------------------------------------------------
connect() -> connect("localhost", 6600).

%%-------------------------------------------------------------------
%% @spec (Addr::string(), Port::integer(), Pass::string) -> {ok, mpd_conn()}
%% @doc
%% Convenience function which connects to an MPD server using connect/2,
%% then calls password/2.
%% @end
%%-------------------------------------------------------------------
connect(Addr, Port, []) -> connect(Addr, Port);
connect(Addr, Port, Pass) ->
    case connect(Addr, Port) of
        {ok, C} ->
            case Pass of
                [] -> ok;
                _  -> password(C, Pass)
            end;
        X -> X
    end.

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Command::string(), Args::list(), Timeout::integer()) -> list()
%% @doc
%% Sends a command to the server and retreives the response.
%% You should not need to use this directly, apart from where
%% parts of the API have not been implemented by this module
%% @end
%%-------------------------------------------------------------------
command(C=#mpd_conn{}, Command, Args, Timeout) ->
    CommandStr = format_command(Command, Args),
    %io:format("SENDING: ~p~n", [lists:flatten(ArgStr)]),
    gen_tcp:send(C#mpd_conn.port, CommandStr),
    receive_lines(C#mpd_conn.port, Timeout).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Command::string(), Args::list()) -> list()
%% @doc
%% Same as calling command(C=#mpd_conn{}, Command, Args, ?TIMEOUT)
%% @end
%%-------------------------------------------------------------------
command(C=#mpd_conn{}, Command, Args) -> command(C, Command, Args, ?TIMEOUT).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Command::string()) -> list()
%% @doc
%% Same as calling command(C=#mpd_conn{}, Command, [], ?TIMEOUT)
%% @end
%%-------------------------------------------------------------------
command(C=#mpd_conn{}, Command) -> command(C, Command, [], ?TIMEOUT).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), CommandList::[{Command,Args}], Timeout) -> list()
%% @doc
%% To facilitate faster adding of files etc. you can pass a list of
%% commands all at once using a command list. The return value is whatever
%% the return for a list of commands is. If a command fails, no more
%% commands are executed and the appropriate mpd error is returned.
%% @end
%%-------------------------------------------------------------------
commandlist(C=#mpd_conn{}, CommandList, Timeout) ->
    CmdStrs = [format_command(Cmd,Args) || {Cmd,Args} <- CommandList],
    %io:format("SENDING: ~p~n", [lists:flatten(ArgStr)]),
    gen_tcp:send(C#mpd_conn.port, io_lib:format("command_list_begin~n", [])),
    [gen_tcp:send(C#mpd_conn.port, CmdStr) || CmdStr <- CmdStrs],
    gen_tcp:send(C#mpd_conn.port, io_lib:format("command_list_end~n", [])),
    receive_lines(C#mpd_conn.port, Timeout).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), CommandList::[{Command,Args}]) -> list()
%% @doc
%% Same as calling commandlist(C=#mpd_conn{}, CommandList, ?TIMEOUT)
%% @end
%%-------------------------------------------------------------------
commandlist(C=#mpd_conn{}, CommandList) ->
    commandlist(C, CommandList, ?TIMEOUT).

%%-------------------------------------------------------------------
%% @spec (Connection::mpd_conn()) -> string()
%% @doc
%% Returns a string representing the MPD version number for Connection
%% @end
%%-------------------------------------------------------------------
version(C=#mpd_conn{}) -> C#mpd_conn.version.


%%===================================================================
%% Querying MPD's status
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Clears the current error message in status (this is also accomplished
%% by any command that starts playback).
%% @end
%%-------------------------------------------------------------------
clearerror(C=#mpd_conn{}) -> parse_none(command(C, "clearerror")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Displays the song info of the current song (same song that is
%% identified in status).
%% @end
%%-------------------------------------------------------------------
currentsong(C=#mpd_conn{}) ->
    convert_song(parse_pairs(command(C, "currentsong"))).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Subsystems::[atom()]) -> [atom()]
%% @doc
%% Waits until there is a noteworthy change in one or more of MPD's
%% subsystems. As soon as there is one, it lists all changed subsystems
%% where a subsystem is one of the following:
%% <ul>
%%   <li>database: the song database has been modified after update.</li>
%%   <li>update: a database update has started or finished. If the
%%       database was modified during the update, the database event is
%%       also emitted.</li>
%%   <li>stored_playlist: a stored playlist has been modified, renamed,
%%       created or deleted </li>
%%   <li>playlist: the current playlist has been modified</li>
%%   <li>player: the player has been started, stopped or seeked</li>
%%   <li>mixer: the volume has been changed</li>
%%   <li>output: an audio output has been enabled or disabled</li>
%%   <li>options: options like repeat, random, crossfade</li>
%% </ul>
%% Available since MPD 0.14.
%% @end
%%-------------------------------------------------------------------
idle(C=#mpd_conn{}, Subsystems) ->
    case C#mpd_conn.version >= "0.14" of
        true ->
            Subs = [atom_to_list(X) || X <- Subsystems],
            Resp = get_all(changed, command(C, "idle", Subs, infinity)),
            [binary_to_atom(X) || X <- Resp];
        false -> {error, mpd_version}
    end.

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> [atom()]
%% @doc
%% Waits until there is a noteworthy change in any of MPD's subsystems.
%% Same as calling idle(C=#mpd_conn{}, []).
%% @end
%%-------------------------------------------------------------------
idle(C=#mpd_conn{}) -> idle(C, []).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Reports the current status of the player and the volume level.
%% Returns a proplist containing a subset of the following:
%% <ul>
%%   <li>volume: integer: 0-100</li>
%%   <li>repeat: boolean</li>
%%   <li>random: boolean</li>
%%   <li>single: boolean</li>
%%   <li>consume: boolean</li>
%%   <li>playlist: integer: the playlist version number</li>
%%   <li>playlistlength: integer: the length of the playlist</li>
%%   <li>state: atom: play | stop | pause</li>
%%   <li>song: integer: playlist song number of the current song
%%       stopped on or playing</li>
%%   <li>songid: integer: playlist songid of the current song
%%       stopped on or playing </li>
%%   <li>time: float: total time elapsed (of current playing/paused song)</li>
%%   <li>elapsed: binary: Total time elapsed within the current song, but with
%%       higher resolution. </li>
%%   <li>bitrate: integer: instantaneous bitrate in kbps</li>
%%   <li>xfade: integer: crossfade in seconds</li>
%%   <li>audio: binary: sampleRate:bits:channels</li>
%%   <li>updatings_db: integer: job id</li>
%%   <li>error: binary: if there is an error, returns message here</li>
%% </ul>
%% @end
%%-------------------------------------------------------------------
status(C=#mpd_conn{}) ->
    convert_props([
            {time, [time]},
            {atom, [state]},
            {boolean, [repeat, random, single, consume]},
            {integer, [volume, playlist, playlistlength, song, songid,
                       bitrate, xfade, updating_db]}
        ],
        parse_pairs(command(C, "status"))).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Displays statistics.
%% Returns a proplist containing a subset of the following:
%% <ul>
%%   <li>artists: integer: number of artists</li>
%%   <li>albums: integer: number of albums</li>
%%   <li>songs: integer: number of albums</li>
%%   <li>uptime: integer: daemon uptime in seconds</li>
%%   <li>playtime: integer: time length of music played</li>
%%   <li>db_playtime: integer: sum of all song times in the db</li>
%%   <li>db_update: integer: last db update in UNIX time</li>
%% </ul>
%% @end
%%-------------------------------------------------------------------
stats(C=#mpd_conn{}) ->
    convert_props([
            {integer, [artists, albums, songs, uptime, playtime, db_playtime,
                    db_update]}
    ], parse_pairs(command(C, "stats"))).

%%===================================================================
%% Playback options
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn(), bool()) -> ok
%% @doc
%% Sets consume state. When consume is activated, each song played is
%% removed from playlist. Available since MPD 0.15.
%% @end
%%-------------------------------------------------------------------
consume(C=#mpd_conn{}, State)  ->
    case C#mpd_conn.version >= "0.15" of
        true when State == true  -> parse_none(command(C, "consume 1"));
        true when State == false -> parse_none(command(C, "consume 0"));
        false -> {error, mpd_version}
    end.

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Secs::integer()) -> ok
%% @doc
%% Sets crossfading between songs to Secs.
%% @end
%%-------------------------------------------------------------------
crossfade(C=#mpd_conn{}, Secs) ->
    parse_none(command(C, "crossfade", [integer_to_list(Secs)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), bool()) -> ok
%% @doc
%% Sets random state.
%% @end
%%-------------------------------------------------------------------
random(C=#mpd_conn{}, true)  -> parse_none(command(C, "random 1"));
random(C=#mpd_conn{}, false) -> parse_none(command(C, "random 0")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), bool()) -> ok
%% @doc
%% Sets repeat state.
%% @end
%%-------------------------------------------------------------------
repeat(C=#mpd_conn{}, true)  -> parse_none(command(C, "repeat 1"));
repeat(C=#mpd_conn{}, false) -> parse_none(command(C, "repeat 0")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Vol::integer()) -> ok
%% @doc
%% Sets volume to Vol, the range of volume is 0-100.
%% @end
%%-------------------------------------------------------------------
setvol(C=#mpd_conn{}, Vol) ->
    parse_none(command(C, "setvol", [integer_to_list(Vol)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), bool()) -> ok
%% @doc
%% Sets single state. When single is activated, playback is stopped after
%% current song, or song is repeated if the 'repeat' mode is enabled.
%% Available since MPD 0.15.
%% @end
%%-------------------------------------------------------------------
single(C=#mpd_conn{}, State)  ->
    case C#mpd_conn.version >= "0.15" of
        true when State == true  -> parse_none(command(C, "single 1"));
        true when State == false -> parse_none(command(C, "single 0"));
        false -> {error, mpd_version}
    end.


%%===================================================================
%% Controlling playback
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Plays next song in the playlist
%% @end
%%-------------------------------------------------------------------
next(C=#mpd_conn{})  -> parse_none(command(C, "next")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), bool()) -> ok
%% @doc
%% Sets pause state. Toggles pause/resumes playing.
%% @end
%%-------------------------------------------------------------------
pause(C=#mpd_conn{}, true)  -> parse_none(command(C, "pause 1"));
pause(C=#mpd_conn{}, false) -> parse_none(command(C, "pause 0")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Begins playling playlist.
%% @end
%%-------------------------------------------------------------------
play(C=#mpd_conn{})  -> parse_none(command(C, "play")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Pos::integer()) -> ok
%% @doc
%% Begins playling playlist at song number Pos.
%% @end
%%-------------------------------------------------------------------
play(C=#mpd_conn{}, Pos) ->
    parse_none(command(C, "play", [integer_to_list(Pos)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Begins playling playlist.
%% @end
%%-------------------------------------------------------------------
playid(C=#mpd_conn{}) -> parse_none(command(C, "playid")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Id::integer()) -> ok
%% @doc
%% Begins playling playlist at song with songid Id.
%% @end
%%-------------------------------------------------------------------
playid(C=#mpd_conn{}, Id) ->
    parse_none(command(C, "playid", [integer_to_list(Id)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Plays previous song in the playlist.
%% @end
%%-------------------------------------------------------------------
previous(C=#mpd_conn{}) -> parse_none(command(C, "previous")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), PlaylistPos::integer(), SeekSecs::integer()) -> ok
%% @doc
%% Seeks to the position SeekSecs (in seconds) of entry PlaylistPos in
%% the playlist.
%% @end
%%-------------------------------------------------------------------
seek(C=#mpd_conn{}, PlaylistPos, SeekSecs) ->
    parse_none(command(C, "seek", [
        integer_to_list(PlaylistPos), integer_to_list(SeekSecs)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), SongId::integer(), SeekSecs::integer()) -> ok
%% @doc
%% Seeks to the position SeekSecs (in seconds) of song SongId.
%% @end
%%-------------------------------------------------------------------
seekid(C=#mpd_conn{}, SongId, SeekSecs) ->
    parse_none(command(C, "seekid", [
        integer_to_list(SongId), integer_to_list(SeekSecs)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Stops playing.
%% @end
%%-------------------------------------------------------------------
stop(C=#mpd_conn{}) -> parse_none(command(C, "stop")).


%%===================================================================
%% The current playlist
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string()) -> ok
%% @doc
%% Adds the file Uri to the playlist (directories add recursively).
%% Uri can also be a single file.
%% @end
%%-------------------------------------------------------------------
add(C=#mpd_conn{}, Uri) -> parse_none(command(C, "add", [Uri])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string()) -> string()
%% @doc
%% Adds a song to the playlist (non-recursive) and returns the song id.
%% Uri is always a single file or URL.
%% @end
%%-------------------------------------------------------------------
addid(C=#mpd_conn{}, Uri) ->
    convert_to_integer(parse_value('Id', command(C, "addid", [Uri]))).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string(), Pos::integer()) -> string()
%% @doc
%% Adds a song to the playlist (non-recursive) and returns the song id.
%% Uri is always a single file or URL. Pos is the position in the playlist
%% to add it, a negative number means it is relative to the currently
%% playing song in the playlist (if there is one).
%% @end
%%-------------------------------------------------------------------
addid(C=#mpd_conn{}, Uri, Pos) ->
    convert_to_integer(
        parse_value('Id', command(C, "addid", [Uri, integer_to_list(Pos)]))).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Clears the current playlist.
%% @end
%%-------------------------------------------------------------------
clear(C=#mpd_conn{}) -> parse_none(command(C, "clear")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), PlaylistPos::integer()) -> ok
%% @doc
%% Deletes the song from the playlist at position PlaylistPos.
%% @end
%%-------------------------------------------------------------------
delete(C=#mpd_conn{}, PlaylistPos) ->
    parse_none(command(C, "delete", [integer_to_list(PlaylistPos)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), SongId::integer()) -> ok
%% @doc
%% Deletes the song SongId from the playlist.
%% @end
%%-------------------------------------------------------------------
deleteid(C=#mpd_conn{}, SongId) ->
    parse_none(command(C, "deleteid", [integer_to_list(SongId)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), SongIds::[integer()]) -> ok
%% @doc
%% Deletes the SongIds from the playlist, more efficient than multiple
%% calls to deleteid.
%% @end
%%-------------------------------------------------------------------
deleteids(C=#mpd_conn{}, SongIds) ->
    Commands = [{"deleteid", [integer_to_list(Id)]} || Id <- SongIds],
    parse_none(commandlist(C, Commands)).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), {Start::integer(), End::integer()}, To::integer()) -> ok
%% @doc
%% Moves the songs in the range Start:End (referring to position in the
%% playlist) to position To in the playlist. Available since MPD 0.15.
%% @end
%%-------------------------------------------------------------------
move(C=#mpd_conn{}, {Start, End}, To) ->
    case C#mpd_conn.version >= "0.15" of
        true ->
            parse_none(command(C, "move", [
                integer_to_list(Start) ++ ":" ++ integer_to_list(End),
                integer_to_list(To)
            ]));
        false -> {error, mpd_version}
    end;

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), From::integer(), To::integer()) -> ok
%% @doc
%% Moves the song at position From to position To in the playlist.
%% @end
%%-------------------------------------------------------------------
move(C=#mpd_conn{}, From, To) ->
    parse_none(command(C, "move", [
        integer_to_list(From), integer_to_list(To)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), From::integer(), To::integer()) -> ok
%% @doc
%% Moves the song with From (songid) to To (playlist index) in the
%% playlist. If To is negative, it is relative to the current song in
%% the playlist (if there is one).
%% @end
%%-------------------------------------------------------------------
moveid(C=#mpd_conn{}, From, To) ->
    parse_none(command(C, "moveid", [
        integer_to_list(From), integer_to_list(To)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> [binary()]
%% @doc
%% Displays the current playlist.
%% <strong>Note:</strong> Do not use this, instead use playlistinfo.
%% @end
%%-------------------------------------------------------------------
playlist(C=#mpd_conn{}) ->
    L = command(C, "playlist"),
    [lists:last(re:split(X, ":", [{parts,2}, {return,binary}])) || X <- L].

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Tag::tag(), X::string()) -> list()
%% @doc
%% Finds songs in the current playlist with strict matching.
%% Tag can be artist | album | title | track | genre | disc | date ...
%% consiting of an atom representing most song meta tags (including
%% MUSICBRAINZ data)
%% @end
%%-------------------------------------------------------------------
playlistfind(C=#mpd_conn{}, Tag, X) ->
    parse_song(command(C, "playlistfind", [atom_to_list(Tag), X])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Displays a list of songs in the playlist.
%% @end
%%-------------------------------------------------------------------
playlistid(C=#mpd_conn{}) ->
    parse_songs(command(C, "playlistid")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Id::integer()) -> list()
%% @doc
%% Displays info for song in playlist with specified id.
%% @end
%%-------------------------------------------------------------------
playlistid(C=#mpd_conn{}, Id) ->
    parse_song(command(C, "playlistid", [integer_to_list(Id)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Displays a list of all songs in the playlist.
%% @end
%%-------------------------------------------------------------------
playlistinfo(C=#mpd_conn{}) ->
    parse_songs(command(C, "playlistinfo")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), {Start::integer(), End::integer()}) -> list()
%% @doc
%% Displays a list of songs between Start and End positions in the
%% playlist (exclusive of end position). Available since MPD 0.15.
%% @end
%%-------------------------------------------------------------------
playlistinfo(C=#mpd_conn{}, {Start, End}) ->
    case C#mpd_conn.version >= "0.15" of
        true ->
            parse_songs(command(C, "playlistinfo", [
                integer_to_list(Start) ++ ":" ++ integer_to_list(End)]));
        false -> {error, mpd_version}
    end;

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), PlaylistPos::integer()) -> list()
%% @doc
%% Displays information for the song at position PlaylistPos
%% @end
%%-------------------------------------------------------------------
playlistinfo(C=#mpd_conn{}, PlaylistPos) ->
    parse_song(command(C, "playlistinfo", [integer_to_list(PlaylistPos)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Tag::tag(), X::string()) -> list()
%% @doc
%% Searches case-sensitively for partial matches in the current playlist.
%% @end
%%-------------------------------------------------------------------
playlistsearch(C=#mpd_conn{}, Tag, X) ->
    parse_songs(command(C, "playlistsearch", [atom_to_list(Tag), X])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Ver::integer()) -> list()
%% @doc
%% Displays changed songs currently in the playlist since Ver.
%% To detect songs that were deleted at the end of the playlist,
%% use playlistlength returned by status command.
%% @end
%%-------------------------------------------------------------------
plchanges(C=#mpd_conn{}, Ver) ->
    parse_songs(command(C, "plchanges", [integer_to_list(Ver)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Ver::integer()) -> list()
%% @doc
%% Displays changed songs currently in the playlist since Ver.
%% This function only returns the position and the id of the changed
%% song, not the complete metadata. This is more bandwidth efficient.
%% To detect songs that were deleted at the end of the playlist,
%% use playlistlength returned by status command.
%% @end
%%-------------------------------------------------------------------
plchangesposid(C=#mpd_conn{}, Ver) ->
    parse_changes(
        command(C, "plchangesposid", [integer_to_list(Ver)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Shuffles the current playlist.
%% @end
%%-------------------------------------------------------------------
shuffle(C=#mpd_conn{}) ->
    parse_none(command(C, "shuffle")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), {Start::integer(), End::integer()}) -> ok
%% @doc
%% Shuffles the songs between the Start and End positions in the current
%% playlist. Available since MPD 0.15.
%% @end
%%-------------------------------------------------------------------
shuffle(C=#mpd_conn{}, {Start, End}) ->
    case C#mpd_conn.version >= "0.15" of
        true ->
            parse_none(command(C, "shuffle", [
                integer_to_list(Start) ++ ":" ++ integer_to_list(End)
            ]));
        false -> {error, mpd_version}
    end.

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), PlaylistPos1::integer(), PlaylistPos2::integer()) -> ok
%% @doc
%% Swaps the positions of songs at PlaylistPos1 and PlaylistPos2.
%% @end
%%-------------------------------------------------------------------
swap(C=#mpd_conn{}, PlaylistPos1, PlaylistPos2) ->
    parse_none(command(C, "swap", [
        integer_to_list(PlaylistPos1), integer_to_list(PlaylistPos2)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), SongId1::integer(), SongId2::integer()) -> ok
%% @doc
%% Swaps the positions of songs SongId1 and SongId2.
%% @end
%%-------------------------------------------------------------------
swapid(C=#mpd_conn{}, SongId1, SongId2) ->
    parse_none(command(C, "swapid", [
        integer_to_list(SongId1), integer_to_list(SongId2)
    ])).


%%===================================================================
%% Stored playlists
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string()) -> [binary()]
%% @doc
%% Lists the files in the playlist Name.m3u.
%% @end
%%-------------------------------------------------------------------
listplaylist(C=#mpd_conn{}, Name) ->
    get_all(file, command(C, "listplaylist", [Name])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string()) -> list()
%% @doc
%% Lists the songs in the playlist Name.m3u.
%% @end
%%-------------------------------------------------------------------
listplaylistinfo(C=#mpd_conn{}, Name) ->
    parse_songs(command(C, "listplaylist", [Name])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Prints a list of the playlist directory.
%% After each playlist name the server sends its last modification time
%% as "Last-Modified" in ISO 8601 format. To avoid problems due to clock
%% differences between clients and the server, clients should not compare
%% this value with their local clock.
%% @end
%%-------------------------------------------------------------------
listplaylists(C=#mpd_conn{}) ->
    parse_playlists(command(C, "listplaylists")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string()) -> ok
%% @doc
%% Loads the playlist Name.m3u from the playlist directory and appends
%% all tracks to the end of the current playlist.
%% @end
%%-------------------------------------------------------------------
load(C=#mpd_conn{}, Name) ->
    parse_none(command(C, "load", [Name])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string(), Uri::string()) -> ok
%% @doc
%% Adds URI to the playlist Name.m3u. Name.m3u will be created if it
%% does not exist.
%% @end
%%-------------------------------------------------------------------
playlistadd(C=#mpd_conn{}, Name, Uri) ->
    parse_none(command(C, "playlistadd", [Name, Uri])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string()) -> ok
%% @doc
%% Clears the playlist Name.m3u.
%% @end
%%-------------------------------------------------------------------
playlistclear(C=#mpd_conn{}, Name) ->
    parse_none(command(C, "playlistclear", [Name])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string(), PlaylistPos::integer()) -> ok
%% @doc
%% Deletes PlaylistPos from the playlist Name.m3u.
%% @end
%%-------------------------------------------------------------------
playlistdelete(C=#mpd_conn{}, Name, PlaylistPos) ->
    parse_none(command(C, "playlistdelete", [
        Name, integer_to_list(PlaylistPos)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(),Name::string(),SongId::string(),PlaylistPos::integer()) -> ok
%% @doc
%% Moves SongId in the playlist Name.m3u, to the postion PlaylistPos.
%% @end
%%-------------------------------------------------------------------
playlistmove(C=#mpd_conn{}, Name, SongId, PlaylistPos) ->
    parse_none(command(C, "playlistmove", [
        Name, integer_to_list(SongId), integer_to_list(PlaylistPos)
    ])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string(), NewName::string()) -> ok
%% @doc
%% Renames the playlist Name.m3u to NewName.m3u.
%% @end
%%-------------------------------------------------------------------
rename(C=#mpd_conn{}, Name, NewName) ->
    parse_none(command(C, "rename", [Name, NewName])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string()) -> ok
%% @doc
%% Removes the playlist NAME.m3u from the playlist directory.
%% @end
%%-------------------------------------------------------------------
rm(C=#mpd_conn{}, Name) ->
    parse_none(command(C, "rm", [Name])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Name::string()) -> ok
%% @doc
%% Saves the current playlist to Name.m3u in the playlist directory.
%% @end
%%-------------------------------------------------------------------
save(C=#mpd_conn{}, Name) ->
    parse_none(command(C, "save", [Name])).


%%===================================================================
%% The music database
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Tag::tag(), X::string()) -> list()
%% @doc
%% Counts the number of songs and their total playtime in the db
%% matching value X for Tag exactly.
%% @end
%%-------------------------------------------------------------------
count(C=#mpd_conn{}, Tag, X) ->
    convert_props([
            {integer, [songs, playtime]}
        ], parse_pairs(command(C, "count", [atom_to_list(Tag), X]))).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Tag::tag(), X::string()) -> list()
%% @doc
%% Finds songs in the db that are exactly What. Tag should be album,
%% artist, or title. X is what to find.
%% @end
%%-------------------------------------------------------------------
find(C=#mpd_conn{}, Tag, X) ->
    parse_songs(command(C, "find", [atom_to_list(Tag), X])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Tag::tag()) -> list()
%% @doc
%% Lists all values of the specified tag.
%% @end
%%-------------------------------------------------------------------
list(C=#mpd_conn{}, Tag) ->
    Results = parse_pairs(command(C, "list", [atom_to_list(Tag)])),
    [Val || {_Key,Val} <- Results].

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), album, Artist::string()) -> list()
%% @doc
%% Lists all tags of type album. Artist specifies the artist to list
%% albums by.
%% @end
%%-------------------------------------------------------------------
list(C=#mpd_conn{}, album, Artist) ->
    get_all('Album', command(C, "list album", [Artist])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Lists all songs and directories.
%% @end
%%-------------------------------------------------------------------
listall(C=#mpd_conn{}) ->
    parse_database(command(C, "listall")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string()) -> list()
%% @doc
%% Lists all songs and directories in Uri.
%% @end
%%-------------------------------------------------------------------
listall(C=#mpd_conn{}, Uri) ->
    parse_database(command(C, "listall", [Uri])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Same as listall/1, except it also returns metadata info in the same
%% format as lsinfo.
%% @end
%%-------------------------------------------------------------------
listallinfo(C=#mpd_conn{}) ->
    parse_database(command(C, "listallinfo")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string()) -> list()
%% @doc
%% Same as listall/2, except it also returns metadata info in the same
%% format as lsinfo.
%% @end
%%-------------------------------------------------------------------
listallinfo(C=#mpd_conn{}, Uri) ->
    parse_database(command(C, "listallinfo", [Uri])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Lists the contents of the root directory. This currently returns the
%% list of stored playlists. This behavior is deprecated; use
%% listplaylists instead.
%% @end
%%-------------------------------------------------------------------
lsinfo(C=#mpd_conn{}) ->
    parse_database(command(C, "lsinfo")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string()) -> list()
%% @doc
%% Lists the contents of the directory Uri.
%% @end
%%-------------------------------------------------------------------
lsinfo(C=#mpd_conn{}, Uri) ->
    parse_database(command(C, "lsinfo", [Uri])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Tag::tag(), What::string()) -> list()
%% @doc
%% Searches for any song with a the specified tag's value containing What.
%% @end
%%-------------------------------------------------------------------
search(C=#mpd_conn{}, Tag, What) ->
    parse_songs(command(C, "search", [atom_to_list(Tag), What])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> integer()
%% @doc
%% Updates the music database.
%% Returns the job id requested for your update, which is displayed in
%% status, while the requested update is happening.
%% @end
%%-------------------------------------------------------------------
update(C=#mpd_conn{}) ->
    [{_Key, Val}] = parse_pairs(command(C, "update")),
    convert_to_integer(Val).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Uri::string()) -> integer()
%% @doc
%% Updates the music database. Uri is a particular directory or
%% song/file to update.
%% Returns the job id requested for your update, which is displayed in
%% status, while the requested update is happening.
%% @end
%%-------------------------------------------------------------------
update(C=#mpd_conn{}, Uri) ->
    [{_Key, Val}] = parse_pairs(command(C, "update", [Uri])),
    convert_to_integer(Val).


%%===================================================================
%% Stickers
%%===================================================================
sticker(C=#mpd_conn{}, delete, Type, Uri) ->
        command(C, "sticker delete", [Type, Uri]);
sticker(C=#mpd_conn{}, list, Type, Uri) ->
        command(C, "sticker list", [Type, Uri]).

sticker(C=#mpd_conn{}, get, Type, Uri, Name) ->
        command(C, "sticker get", [Type, Uri, Name]);
sticker(C=#mpd_conn{}, find, Type, Uri, Name) ->
        command(C, "sticker find", [Type, Uri, Name]);
sticker(C=#mpd_conn{}, delete, Type, Uri, Name) ->
        command(C, "sticker delete", [Type, Uri, Name]).

sticker(C=#mpd_conn{}, set, Type, Uri, Name, Value) ->
        command(C, "sticker set", [Type, Uri, Name, Value]).


%%===================================================================
%% Connection settings
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Closes the connection to MPD.
%% @end
%%-------------------------------------------------------------------
close(C=#mpd_conn{}) ->
    {error,closed} = command(C, "close"), ok.

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Kills MPD.
%% @end
%%-------------------------------------------------------------------
kill(C=#mpd_conn{}) ->
    {error,closed} = command(C, "kill"), ok.

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), Password::string()) -> ok
%% @doc
%% This is used for authentication with the server. Password is simply
%% the plaintext password.
%% @end
%%-------------------------------------------------------------------
password(C=#mpd_conn{}, Password) ->
    parse_none(command(C, "password", [Password])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> ok
%% @doc
%% Does nothing but return ok.
%% @end
%%-------------------------------------------------------------------
ping(C=#mpd_conn{}) -> parse_none(command(C, "ping")).


%%===================================================================
%% Audio output devices
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn(), OutputId::integer()) -> ok
%% @doc
%% Turns off output with id OutputId
%% @end
%%-------------------------------------------------------------------
disableoutput(C=#mpd_conn{}, OutputId) ->
    parse_none(command(C, "disableoutput", [integer_to_list(OutputId)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn(), OutputId::integer()) -> ok
%% @doc
%% Turns on output with id OutputId
%% @end
%%-------------------------------------------------------------------
enableoutput(C=#mpd_conn{}, OutputId) ->
    parse_none(command(C, "enableoutput", [integer_to_list(OutputId)])).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> list()
%% @doc
%% Shows information about all outputs.
%% @end
%%-------------------------------------------------------------------
outputs(C=#mpd_conn{}) -> parse_outputs(command(C, "outputs")).


%%===================================================================
%% Reflection
%%===================================================================
%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> [binary()]
%% @doc
%% Shows which commands the current user has access to
%% @end
%%-------------------------------------------------------------------
commands(C=#mpd_conn{}) ->
    get_all(command, command(C, "commands")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> [binary()]
%% @doc
%% Shows which commands the current user does not have access to
%% @end
%%-------------------------------------------------------------------
notcommands(C=#mpd_conn{}) ->
    get_all(command, command(C, "notcommands")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> [binary()]
%% @doc
%% Shows a list of available song metadata.
%% @end
%%-------------------------------------------------------------------
tagtypes(C=#mpd_conn{}) ->
    get_all(tagtype, command(C, "tagtypes")).

%%-------------------------------------------------------------------
%% @spec (mpd_conn()) -> [binary()]
%% @doc
%% Gets a list of available URL handlers.
%% @end
%%-------------------------------------------------------------------
urlhandlers(C=#mpd_conn{}) ->
    get_all(handler, command(C, "urlhandlers")).




%%===================================================================
%% Internal Functions
%%===================================================================

escape_quotes(X) -> escape_quotes(X, []).
escape_quotes([H|T],S) ->
    case H of
        $" -> escape_quotes(T, S ++ ["\\\""]);
        X  -> escape_quotes(T, S ++ [X])
    end;
escape_quotes([], X) -> X.

receive_lines(Sock, Timeout) -> receive_lines(Sock, Timeout, []).
receive_lines(Sock, Timeout, L) ->
    R = gen_tcp:recv(Sock, 0, Timeout),
    %io:format("RECEIVED: ~p~n", [R]),
    case R of
        {ok, "OK\n"} -> L;
        {ok, "ACK " ++ AckResp} -> {error, parse_mpd_error(AckResp)};
        {ok, Resp} -> receive_lines(Sock, Timeout, L ++ [Resp -- "\n"]);
        {error, Error} -> {error, Error}
    end.

parse_mpd_error(Str) ->
    {match, List} = re:run(Str,
        "\\[(\\d+)@(\\d+)\\] \\{(\\w*)\\} ?(.*)\\n",
        [{capture, all_but_first, list}]
    ),
    list_to_tuple([mpd_error|List]).

pass_errors(List, Fun) ->
    case List of
        {error, Error} -> {error, Error};
        _ -> Fun(List)
    end.

parse_pairs(List) ->
    pass_errors(List, fun(L) ->
        lists:map(fun(X) ->
            [Key,Val] = re:split(X, ": ", [{parts,2}, {return,binary}]),
            {binary_to_atom(Key), Val}
        end, L)
    end).


parse_group(Delimiters, List) ->
    pass_errors(List, fun(L) ->
        parse_group(Delimiters, parse_pairs(L), [], [])
    end).
parse_group(_Delimiters, [], [], []) -> [];
parse_group(Delimiters, [H|T], X, S) ->
    {D,_} = H,
    IsDelimiter = lists:member(D, Delimiters),
    case IsDelimiter of
        true when X /= []  -> parse_group(Delimiters, T, [H], S ++ [X]);
        false when X == [] -> parse_group(Delimiters, T, [H], S);
        _ -> parse_group(Delimiters, T, X ++ [H], S)
    end;
parse_group(_Delimiters, [], X, S) -> S ++ [X].

parse_value(Key, List) ->
    pass_errors(parse_pairs(List), fun(L) -> proplists:get_value(Key,L) end).

parse_songs(List) ->
    pass_errors(List, fun(L) ->
        [convert_song(X) || X <- parse_group([file], L)]
    end).

parse_song(List) -> convert_song(parse_pairs(List)).

parse_changes(List) ->
    pass_errors(List, fun(L) -> parse_group([cpos], L) end).

parse_playlists(List) ->
    pass_errors(List, fun(L) -> parse_group([playlist], L) end).

parse_database(List) ->
    pass_errors(List, fun(L) ->
        lists:map(fun(X) ->
            case proplists:is_defined('file', X) of
                true -> convert_song(X);
                false -> X
            end
        end, parse_group([file, directory, playlist], L))
    end).

parse_outputs(List) ->
    pass_errors(List, fun(L) ->
        lists:map(
            fun(X) -> convert_props([
                    {integer, [outputid]},
                    {boolean, [outputenabled]}
                ], X)
            end, parse_group([outputid], L))
    end).

parse_none(List) -> pass_errors(List, fun(L) -> [] = L, ok end).

get_all(Key, List) ->
    pass_errors(List, fun(L) ->
        proplists:get_all_values(Key, parse_pairs(L))
    end).

convert_to_integer(Val) ->
    if
        is_binary(Val) -> convert_to_integer(binary_to_list(Val));
        is_list(Val)   -> try list_to_integer(Val)
                          catch error:_X -> list_to_binary(Val) end;
        true           -> Val
    end.

convert_to_time(Val) ->
    list_to_float(re:replace(Val, ":", ".", [{return,list}])).

convert_to_boolean(Val) ->
    case Val of
        X when X == <<"0">>; X == "0", X == 0 -> false;
        X when X == <<"1">>; X == "1", X == 1 -> true
    end.

convert_props_to(Type, Keys, Data) -> convert_props_to(Type, Keys, Data, []).
convert_props_to(Type, Keys, [{Key,Val}|Data], Converted) ->
    case lists:member(Key, Keys) of
        true ->
            case Type of
                integer -> NewVal = convert_to_integer(Val);
                time    -> NewVal = convert_to_time(Val);
                boolean -> NewVal = convert_to_boolean(Val);
                atom    -> NewVal = binary_to_atom(Val)
            end,
            convert_props_to(Type, Keys, Data, Converted ++ [{Key, NewVal}]);
        false ->
            convert_props_to(Type, Keys, Data, Converted ++ [{Key, Val}])
    end;
convert_props_to(_Type, _Keys, [], Converted) -> Converted.

convert_props(Map, Data) ->
    pass_errors(Data, fun(L) ->
        lists:foldl(
            fun({Type,Keys},Acc) -> convert_props_to(Type, Keys, Acc) end,
            L, Map)
    end).

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).

convert_song(Data) ->
    convert_props([{integer, ['Id', 'Pos', 'Time', 'Track', 'Disc']}], Data).

format_command(Command, Args) ->
    Args2 = [io_lib:format("\"~s\"", [escape_quotes(X)]) || X <- Args],
    io_lib:format("~s ~s~n", [Command, string:join(Args2, " ")]).
