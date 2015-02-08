%% Copyright 2011,  Filipe David Manana  <fdmanana@apache.org>
%% Web:  http://github.com/fdmanana/snappy-erlang-nif
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(lzo_tests).
-include_lib("eunit/include/eunit.hrl").

lzo1_test_() ->
    {timeout, 60, [fun lzo1/0]}.

lzo2_test_() ->
    {timeout, 60, [fun lzo2/0]}.

lzo1() ->
    {ok, Data} = file:read_file("../README.md"),
    io:fwrite("~p~n", [file:get_cwd()]),
    Compressed1 = lzo1:zip(Data),
    Decompressed1 = lzo1:unzip(Compressed1, size(Data) * 256),
    Decompressed2 = lzo1:unzip(Compressed1, size(Data)),
    Decompressed3 = lzo1:unzip(Compressed1),
    {ok, Compressed4} = lzo1:compress(Data),
    {ok, Decompressed4} = lzo1:decompress(Compressed4),
    ?assertEqual(Data, Decompressed1),
    ?assertEqual(Data, Decompressed2),
    ?assertEqual(Data, Decompressed3),
    ?assertEqual(Data, Decompressed4).

lzo2() ->
    {ok, Data} = file:read_file("../README.md"),
    Compressed1 = lzo2:zip(Data),
    Decompressed1 = lzo2:unzip(Compressed1, size(Data) * 256),
    Decompressed2 = lzo2:unzip(Compressed1, size(Data)),
    Decompressed3 = lzo2:unzip(Compressed1),
    {ok, Compressed4} = lzo2:compress(Data),
    {ok, Decompressed4} = lzo2:decompress(Compressed4),
    ?assertEqual(Data, Decompressed1),
    ?assertEqual(Data, Decompressed2),
    ?assertEqual(Data, Decompressed3),
    ?assertEqual(Data, Decompressed4).
