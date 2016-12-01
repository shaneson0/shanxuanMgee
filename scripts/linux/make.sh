
#!/bin/sh

cd ../../ebin
sudo rm *.beam

cd ..
erl -make

cd ebin

erl -noshell -s protobuffs_compile scan_file "../proto/game.proto" -s erlang halt
mv game_pb.hrl ../include/

