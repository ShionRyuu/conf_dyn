#conf_dyn - dynamic config generate

[![Build Status](https://secure.travis-ci.org/genesislive/Sep.png?branch=master)](http://travis-ci.org/genesislive/Sep)

conf_dyn use dynamic_compile to generate beam file from erlang consult file.

## Usage

```sh
$erl -pa ./ebin -pa ./deps/*/ebin -root_dir `pwd`
1> conf_dyn:reload_all().
ok
2> conf_dyn:find(mora, invite_times).
[5]
3> mora:get(invite_times).
5
4> conf_dyn:list(mora).
[{invite_times,5},{gain_times,10}]
5> mora:list(mora).
[{invite_times,5},{gain_times,10}]
6> conf_dyn:find(mora, not_exist_key).
[]
7> mora:get(not_exist_key).
undefined
```

## Test

```sh
$ERL_FLAGS="-pa ./ebin -pa ./deps/*/ebin -root_dir `pwd`" rebar compile eunit
```

## Config 

1. kv_con, one value for one key, declare as key-value
```
{invite_times, 5}.

{gain_times, 10}.
```

2. kv_list, multiple values for one key, declare as key-value


3. rec_con, one value for one key, declare as erlang tuple
```
{sale_store, 1, {2013, 8, 20}, {2013, 8, 22}, []}.
{sale_store, 2, {2013, 8, 23}, {2013, 8, 24}, []}.
```

4. rec_list, multiple values for one key, declare as erlang tuple


## Authors

- Shion Ryuu <genesislive@outlook.com>

## Todo


