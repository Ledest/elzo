%% lzo1.erl -- wrapper of the LZO1x algorithm
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

-module(lzo1).

-export([zip/1, unzip/1, unzip/2, compress/1, decompress/1]).

-on_load(load_nif/0).
-define(LZO1_NIF_VSN, 1).

load_nif() ->
    P = case code:priv_dir(lzo) of
            {error, bad_name} ->
                D1 = filename:join([".", "priv", "lib"]),
                case filelib:is_dir(D1) of
                    true -> D1;
                    _ ->
                        D2 = [$.|D1],
                        case filelib:is_dir(D2) of
                            true -> D2;
                            _ -> "."
                        end
                end;
            D -> D
        end,
    E = file:native_name_encoding(),
    L = filename:join(P, "lzo1"),
    erlang:load_nif(L, {?LZO1_NIF_VSN, unicode:characters_to_binary(L, E, E)}).

-spec zip(Data::binary()|iodata()) -> binary().
zip(_Data) -> exit(lzo1_nif_not_loaded).

-spec unzip(Data::binary()|iodata(), MaxSize::integer()) -> binary().
unzip(_Data, _MaxSize) -> exit(lzo1_nif_not_loaded).

-spec unzip(Data::binary()|iodata()) -> binary().
unzip(_Data) -> exit(lzo1_nif_not_loaded).

-spec compress(Data::binary()|iodata()) -> binary().
compress(_Data) -> exit(lzo1_nif_not_loaded).

-spec decompress(Data::binary()|iodata()) -> binary().
decompress(_Data) -> exit(lzo1_nif_not_loaded).
