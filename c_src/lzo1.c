/* lzo1.c -- wrapper of the LZO1x algorithm

   This file is part of the erlang elzo data compression library.

   Copyright (C) 2015 Oleksandr Chumachenko
   All Rights Reserved.

   The elzo library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   The elzo library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with the elzo library; see the file COPYING.
   If not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

   Oleksandr Chumachenko
   <ledest@gmail.com>
   https://github.com/Ledest/elzo/
 */

#include <erl_nif.h>
#include <lzo/lzo1x.h>
#include <stdint.h>

static inline ERL_NIF_TERM make_ok(ErlNifEnv *env, ERL_NIF_TERM msg)
{
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), msg);
}

static inline ERL_NIF_TERM make_error(ErlNifEnv *env, const char *msg)
{
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, msg));
}

static ERL_NIF_TERM lzo1_zip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	lzo_uint os;
	unsigned char wm[LZO1X_1_MEM_COMPRESS];

	if (!enif_inspect_binary(env, argv[0], &i) && !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(i.size + i.size / 16 + 64 + 3, &o))
		return make_error(env, "insufficient_memory");

	lzo1x_1_compress(i.data, i.size, o.data, &os, &wm);
	if (os != o.size)
		enif_realloc_binary(&o, os);

	return enif_make_binary(env, &o);
}

static ERL_NIF_TERM lzo1_unzip_error(ErlNifEnv* env, int code)
{
	const char *s;

	switch (code) {
	case LZO_E_INPUT_NOT_CONSUMED:
		s = "input_not_consumed";
		break;
	case LZO_E_INPUT_OVERRUN:
		s = "input_overrun";
		break;
	case LZO_E_OUTPUT_OVERRUN:
		s = "output_overrun";
		break;
	case LZO_E_LOOKBEHIND_OVERRUN:
		s = "lookbehind_overrun";
		break;
	case LZO_E_EOF_NOT_FOUND:
		s = "eof_not_found";
		break;
	case LZO_E_ERROR:
		s = "lzo_unknown";
		break;
	default:
		s = "unknown";
		break;
	}
	return make_error(env, s);
}

static ERL_NIF_TERM lzo1_unzip(ErlNifEnv* env, ErlNifBinary *i, size_t os)
{
	ErlNifBinary o;
	int c;

	if (!enif_alloc_binary(os, &o))
		return make_error(NULL, "insufficient_memory");

	c = lzo1x_decompress_safe(i->data, i->size, o.data, &o.size, NULL);
	return c != LZO_E_OK
	       ? lzo1_unzip_error(env, c)
	       : (os == o.size || enif_realloc_binary(&o, o.size))
	         ? enif_make_binary(env, &o)
	         : make_error(env, "insufficient_memory");
}

static ERL_NIF_TERM lzo1_unzip_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i;
	size_t ms;

	return (enif_get_ulong(env, argv[1], &ms) && enif_inspect_iolist_as_binary(env, argv[0], &i))
	       ? lzo1_unzip(env, &i, ms)
	       : enif_make_badarg(env);
}

static ERL_NIF_TERM lzo1_unzip_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i;

	if (!enif_inspect_binary(env, argv[0], &i) && !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	return lzo1_unzip(env, &i, i.size * 256);
}

static ERL_NIF_TERM lzo1_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	lzo_uint os;
	unsigned char wm[LZO1X_1_MEM_COMPRESS];
	const char *s = "insufficient_memory";

	if (!enif_inspect_binary(env, argv[0], &i) || !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(i.size + i.size / 16 + 64 + 3 + sizeof(uint32_t), &o))
		return make_error(env, s);

	*(uint32_t*)o.data = (uint32_t)i.size;
	lzo1x_1_compress(i.data, i.size, o.data + sizeof(uint32_t), &os, &wm);

	return (os != o.size && !enif_realloc_binary(&o, os + sizeof(uint32_t)))
	       ? make_error(env, s)
	       : make_ok(env, enif_make_binary(env, &o));
}

static ERL_NIF_TERM lzo1_decompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	int c;

	if (!enif_inspect_binary(env, argv[0], &i) || !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(*(uint32_t*)i.data, &o))
		return make_error(NULL, "insufficient_memory");

	c = lzo1x_decompress_safe(i.data + sizeof(uint32_t), i.size - sizeof(uint32_t), o.data, &o.size, NULL);
	return c == LZO_E_OK
	       ? make_ok(env, enif_make_binary(env, &o))
	       : lzo1_unzip_error(env, c);
}

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{
#if LZO_E_OK == 0
	return lzo_init();
#else
	int r = lzo_init();

	return r == LZO_E_OK ? 0 : r;
#endif
}

static int on_reload(ErlNifEnv *env, void**priv, ERL_NIF_TERM info)
{
	return 0;
}

static int on_upgrade(ErlNifEnv *env, void **priv, void** old_priv, ERL_NIF_TERM info)
{
	return 0;
}


static ErlNifFunc nif_functions[] = {
	{"zip", 1, lzo1_zip},
	{"unzip", 2, lzo1_unzip_2},
	{"unzip", 1, lzo1_unzip_1},
	{"compress", 1, lzo1_compress},
	{"uncompress", 1, lzo1_decompress},
	{"decompress", 1, lzo1_decompress}
};

ERL_NIF_INIT(lzo1, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);
