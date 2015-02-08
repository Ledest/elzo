%% lzo_test.erl -- unit test for wrapper of the elzo library
%%
%% This file is part of the erlang elzo data compression library.
%% Copyright (C) 2015 Oleksandr Chumachenko
%% All Rights Reserved.
%%
%% The elzo library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License as
%% published by the Free Software Foundation; either version 2 of
%% the License, or (at your option) any later version.
%%
%% The elzo library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License
%% along with the elzo library; see the file COPYING.
%% If not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
%%
%% Oleksandr Chumachenko
%% <ledest@gmail.com>
%% https://github.com/Ledest/elzo/

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
