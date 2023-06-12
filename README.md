# PC-TP

To run the server you must compile the game, file_manager, user_manager and server files
then execute server:start(PORT).


In order to run the clients you must edit the Game.pde files and switch the IP by your own,
you can find it out by running "ifconfig | grep "inet " | grep -Fv 127.0.0.1 | awk '{print $2}'"
and update the port for the one used to start the server above.

Keep in mind that in order to play there must be two or more players in the lobby with the same level.
