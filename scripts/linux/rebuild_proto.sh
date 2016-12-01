
#!/bin/sh

#Erlang

as_path=/mnt/hgfs/workdir/mgee/mgee_connect/mgee_connect/src

cd ../../ebin

erlc ../src/proto/protobuffs.erl
erlc ../src/proto/protobuffs_compile.erl
erlc ../src/proto/protobuffs_parser.erl
erlc +debug_info ../src/proto/pokemon_pb.erl

erl -noshell -s protobuffs_compile scan_file "../proto/game.proto" -s erlang halt
mv  game_pb.hrl ../include/


#As3
#/uprotoc --as3_out=/mnt/hgfs/workdir/mgee/mgee_connect/mgee_connect/ ../proto/game.proto

cd ../proto
/usr/local/protobuf/bin/protoc --as3_out=${as_path} game.proto


