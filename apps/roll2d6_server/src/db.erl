-module(db).

-export([ create_person/2
        , get_person/2
        , create_new_game/4
        , get_games_for_player/1
        , get_game/1
        , add_player_to_game/2
        , get_game_access/2
        , get_players/1
        , remove_player_from_game/2
        , get_player_for_game/2
        , get_player_access_for_game/2
        , update_player_access/3
        ]).

-define(DATABASE, "data/data.sqlite3").


%%====================================================================
%% Data transformations
%%====================================================================

-spec to_player({Jid, Username, Access}) -> Player when
      Jid      :: binary(),
      Username :: binary(),
      Access   :: binary(),
      Player   :: map().

to_player({Jid, Username, Access})->
    #{ <<"id">>       => Jid, 
       <<"username">> => Username,
       <<"access">>   => Access
      }.

%%--------------------------------------------------------------------

-spec to_game({Id, Title, GameType, CreatedAt}) -> Game when
      Id        :: binary(),
      Title     :: binary(),
      GameType  :: binary(),
      CreatedAt :: binary(),
      Game      :: map().

to_game({Id, Title, GameType, CreatedAt})->
    #{ <<"id">>        => Id,
       <<"title">>     => Title,
       <<"gameType">>  => GameType,
       <<"createdAt">> => CreatedAt
      }.

%%====================================================================
%% Queries
%%====================================================================

create_person(Jid, Auth) ->
    #{ <<"username">> := Username
     , <<"email">>    := Email
     , <<"password">> := HashedPassword
     } = Auth,
    {ok, Conn} = esqlite3:open(?DATABASE),
    [] = esqlite3:q(
        "INSERT INTO person (id, username, email, password) 
         VALUES ($1, $2, $3, $4);", 
        [Jid, string:lowercase(Username), Email, HashedPassword], 
        Conn),
    esqlite3:close(Conn),
    ok.

%%--------------------------------------------------------------------

get_person(Username, HashedPassword) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [{Jid}] = esqlite3:q( 
        "SELECT id FROM person WHERE username = $1 AND password = $2;", 
        [string:lowercase(Username), HashedPassword],
        Conn),
    esqlite3:close(Conn),
    Jid.

%%--------------------------------------------------------------------

create_new_game(Jid, GameId, Title, GameType) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [] = esqlite3:q(
        "INSERT INTO game 
           (id, title, game_type) 
           VALUES ($1, $2, $3);",
        [GameId, Title, GameType],
        Conn),
    [] = esqlite3:q(
        "INSERT INTO person_game_relation
           (game_id, person_id, access)
           VALUES ($1, $2, 'owner');",
        [GameId, Jid],
        Conn),
    esqlite3:close(Conn),
    ok.

%%--------------------------------------------------------------------

get_games_for_player(Jid) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    Games = esqlite3:q(
        "SELECT game.id, game.title, game.game_type, game.created_at
         FROM person_game_relation as rel
         INNER JOIN game
         ON game.id = rel.game_id
         WHERE rel.person_id = $1
         ORDER BY game.created_at ASC;",
        [Jid],
        Conn),
    esqlite3:close(Conn),
    true = is_list(Games),
    lists:map(fun to_game/1, Games).

%%--------------------------------------------------------------------

get_game(GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [{Game}] = esqlite3:q(
        "SELECT id, title, game_type, created_at
         FROM game
         WHERE id = $1;",
        [GameId], 
        Conn),
    esqlite3:close(Conn),
    to_game(Game).

%%--------------------------------------------------------------------

add_player_to_game(NewPlayerJid, GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [] = esqlite3:q(
        "INSERT INTO person_game_relation 
         (person_id, game_id, access)
         VALUES ($1, $2, 'player');",
        [NewPlayerJid, GameId],
        Conn),
    esqlite3:close(Conn),
    ok.

%%--------------------------------------------------------------------

get_game_access(Jid, GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    Result = esqlite3:q(
        "SELECT access
         FROM person_game_relation
         WHERE person_id = $1 AND game_id = $2;", 
        [Jid, GameId], 
        Conn),
    esqlite3:close(Conn),
    case Result of
        [{Access}] -> {ok, Access};
        _ -> forbidden
    end.

%%--------------------------------------------------------------------

get_players(GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    Players = esqlite3:q(
        "SELECT person.id, person.username, rel.access
         FROM person_game_relation as rel INNER JOIN person
         ON person.id = rel.person_id
         WHERE rel.game_id = $1
         ORDER BY rel.created_at ASC;",
        [GameId], 
        Conn),
    esqlite3:close(Conn),
    true = is_list(Players),
    lists:map(fun to_player/1, Players).

%%--------------------------------------------------------------------

-spec remove_player_from_game(PlayerId, GameId) -> Result when
      PlayerId :: binary(),
      GameId   :: binary(),
      Result   :: ok.

remove_player_from_game(PlayerId, GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [] = esqlite3:q(
        "DELETE FROM person_game_relation 
         WHERE person_id = $1
         AND game_id = $2",
        [PlayerId, GameId], 
        Conn),
    esqlite3:close(Conn),
    ok.

%%--------------------------------------------------------------------

-spec get_player_for_game(PlayerId, GameId) -> Player when
      PlayerId :: binary(),
      GameId   :: binary(),
      Player   :: map().

get_player_for_game(PlayerId, GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [Player] = esqlite3:q(
        "SELECT person.id, person.username, rel.access
         FROM person_game_relation as rel
         INNER JOIN person
         ON person.id = rel.person_id
         WHERE rel.person_id = $1
         AND rel.game_id = $2;",
        [PlayerId, GameId],
        Conn),
    esqlite3:close(Conn),
    to_player(Player).

%%--------------------------------------------------------------------

-spec get_player_access_for_game(PlayerId, GameId) -> Access when
      PlayerId :: binary(),
      GameId   :: binary(),
      Access   :: binary().

get_player_access_for_game(PlayerId, GameId) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [Access] = esqlite3:q(
        "SELECT rel.access
         FROM person_game_relation as rel
         WHERE rel.person_id = $1
         AND rel.game_id = $2;",
        [PlayerId, GameId],
        Conn),
    esqlite3:close(Conn),
    Access.

%%--------------------------------------------------------------------

-spec update_player_access(PlayerId, GameId, NewAccess) -> Result when
      PlayerId  :: binary(),
      GameId    :: binary(),
      NewAccess :: binary(),
      Result    :: ok.

update_player_access(PlayerId, GameId, NewAccess) ->
    {ok, Conn} = esqlite3:open(?DATABASE),
    [] = esqlite3:q(
        "UPDATE person_game_relation
         SET access = $1
         WHERE person_id = $2
         AND game_id = $3;",
        [NewAccess, PlayerId, GameId],
        Conn),
    esqlite3:close(Conn),
    ok.

%%--------------------------------------------------------------------
