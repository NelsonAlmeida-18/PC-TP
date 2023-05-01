-module(server).
-import(users_manager, [create_account/4, delete_account/4, login/4, logout/4]).
-export([start/0]).

