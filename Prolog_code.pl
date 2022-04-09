check_policy(_, []) :- !.
check_policy(S, [S | _]) :- !, fail.
check_policy(S, [_ | Str]):- !, check_policy(S, Str).

check_car_final(Symbol, [Symbol | _], Terminal) :-
    !, atom_chars(Symbol, Terminal).
check_car_final(Symbol, [_ | Ss], T) :- check_car_final(Symbol, Ss, T).

check_ids([]) :- !.
check_ids(['.', '.' | _]) :- !, fail.
check_ids([_ | String]) :- check_ids(String).

check_slash([]) :- !.
check_slash(['/', '/' | _]) :- !, fail.
check_slash([_ | String]) :- check_slash(String).

check_num([A, B, C]) :-
    char_code(A, X), char_code(B, Y), char_code(C, Z),
    number_codes(H, [X, Y ,Z]),

    H =< 225,
    H >= 0.

not_empty(List) :-
    length(List, L),
    L > 0.

is_empty([]) :- !.

parse([Starter_char, I | Input],[I | Unit], Rest, Starter_char,
      Final_char_list, Policy, Terminal) :-
    !,
    parse_aux([I | Input], [I | Unit], Rest, Final_char_list, Policy, Terminal).

parse([I | Input], [I | Unit], Rest, Final_char_list, Policy, Terminal) :-
    check_policy(I, Final_char_list),
    !,
    parse_aux([I | Input], [I | Unit], Rest, Final_char_list, Policy, Terminal).


parse_aux([I | Input], [], Input, Final_char_list, _, Terminal) :-
    check_car_final(I, Final_char_list, Terminal),
    !.

parse_aux([I | Input], [I | Unit], Rest, Final_char_list, Policy, Terminal) :-
    check_policy(I, Policy),
    !,
    parse_aux(Input, Unit, Rest, Final_char_list, Policy, Terminal).

parse_aux([], [], [], Final_char_list, _, Terminal) :-
    check_car_final('', Final_char_list, Terminal),
    !.

uri_parse(UriString, uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)):-
    uri_string(UriString, S, U, H, Port, P, Q, F),
    string_to_atom(S, Scheme),
    string_to_atom(U, Userinfo),
    string_to_atom(H, Host),
    string_to_atom(P, Path),
    string_to_atom(Q, Query),
    string_to_atom(F, Fragment).

uri_display(URI) :-
    functor(URI, uri, 7),
    uri_display(URI, current_output).


uri_display(URI, Stream) :-
    functor(URI, uri, 7),

    arg(1, URI, Scheme),
    write(Stream, "Schema: "),
    write(Stream, Scheme),
    nl(Stream),
    nl(Stream),

    arg(2, URI, Userinfo),
    write(Stream, "Userinfo: "),
    write(Stream, Userinfo),
    nl(Stream),
    nl(Stream),

    arg(3, URI, Host),
    write(Stream, "Host: "),
    write(Stream, Host),
    nl(Stream),
    nl(Stream),

    arg(4, URI, Port),
    write(Stream, "Port: "),
    write(Stream, Port),
    nl(Stream),
    nl(Stream),

    arg(5, URI, Path),
    write(Stream, "Path: "),
    write(Stream, Path),
    nl(Stream),
    nl(Stream),

    arg(6, URI, Query),
    write(Stream, "Query: "),
    write(Stream, Query),
    nl(Stream),
    nl(Stream),

    arg(7, URI, Fragment),
    write(Stream, "Fragment: "),
    write(Stream, Fragment),
    nl(Stream),
    nl(Stream),

    close(Stream).

uri_string(UriString, Scheme, Userinfo, Host, Port, Path, Query, Fragment) :-
    atom_chars(UriString, Input),
    scheme(Input, Scheme, Rest_scheme),
    uri2(Rest_scheme, Scheme, Userinfo, Host, Port, Path, Query, Fragment),
    !.

uri_string(UriString, Scheme, Userinfo, Host, Port, Path, Query, Fragment) :-
    atom_chars(UriString, Input),
    scheme(Input, Scheme, Rest_scheme),
    check_policy(Scheme, [[z,o,s], [m,a,i,l,t,o], [t,e,l], [f,a,x], [n,e,w,s]]),
    uri1(Rest_scheme, Userinfo, Host, Port, Path, Query, Fragment),
    !.

scheme(String, Scheme, Rest) :-
    parse(String, Scheme, Rest, [':'], ['/', '?', '#', '@'], _).


uri1(String, Userinfo, Host, Port, [], Query, Fragment) :-
    authority(String, Userinfo, Host, Port, Rest_authority),
    optional(Rest_authority, [], Query, Fragment),
    !.

uri1(Input, [], [], 80, Path, Query, Fragment) :-
    authority(Input, [], [], 80, ['/' | Rest_authority]),
    optional(Rest_authority, Path, Query, Fragment),
    !.

uri1(String, Userinfo, Host, Port, Path, Query, Fragment) :-
    authority(String, Userinfo, Host, Port, ['/' | Rest_authority]),
    optional(Rest_authority, Path, Query, Fragment),
    Path \= [],
    !.

uri1(String, Userinfo, Host, Port, Path, Query, Fragment) :-
    authority(String, Userinfo, Host, Port, String),
    optional(String, Path, Query, Fragment),
    !.

authority(['/', '/' | String], Userinfo, Host, Port, Rest) :-
    !,
    userinfo(String, Userinfo, Resto_userinfo),
    host(Resto_userinfo, Host, Resto_host),
    port(Resto_host, Port, Rest).

authority([S | String], [], [], Port,[S | String]) :-
    !,
    atom_number('80', Port).

authority([], [], [], Port,[]) :-
    !,
    atom_number('80',Port).

userinfo(String, Userinfo, Rest) :-
    parse(String, Userinfo, Rest, ['@'], ['/', '?', '#', ':'], _), !.

userinfo(String, Userinfo, Rest) :-
    parse(String, Rest, Userinfo, [''], [], _),
    !.

host(['.' | _], _, _) :- !, fail.

host(String, Host,  Rest) :-
    parse(String, Host, R, ['', '/', ':', '?', '#'], ['?', '#', '@'], Terminal),
    append(Terminal, R, Rest),
    !,
    check_ids(Host).

host([A, B, C, '.', D, E, F, '.', H, I, L, '.', M, N, O | Ss], Host, Ss) :-

    is_digit(A), is_digit(B), is_digit(C),
    is_digit(D), is_digit(E), is_digit(F),
    is_digit(H), is_digit(I), is_digit(L),

    check_num([A, B, C]),
    check_num([D, E, F]),
    check_num([M, N ,O]),
    !,

    Host = [A, B, C, '.', D, E, F, '.', H, I, L, '.', M, N, O].

port(String, Port, Rest) :-
    parse(String, Port_list, R, ':', ['','/', '?', '#'], [], Terminal),
    !,
    append(Terminal, R, Rest),

    string_to_atom(Port_list, P),
    atom_number(P, Port).

port([], Port, []) :-
    atom_number('80', Port).

port([S | Rest], Port, [S | Rest]) :-
    check_policy(S, [[':']]),
    atom_number('80',Port).

optional(String, Path, Query, Fragment) :-
    path(String, Path, Rest_path),
    query(Rest_path, Query, Rest_query),
    fragment(Rest_query, Fragment, _),
    !.

optional([], [], [], []) :-
    !.

path([ '/' | _], _, _) :-!, fail.
path(String, Path, Rest) :-
    parse(String, Path, R, ['', '?', '#'], ['@', ':'], Terminal),
    !,
    append(Terminal, R, Rest),
    check_slash(Path).

path(String, [], String) :- !.

query(String, Query, Rest) :-
    parse(String, Query, R, '?', ['', '#'], [], Terminal),
    !,
    append(Terminal, R, Rest).

query(String, [], String) :- !.

fragment(String, Fragment, Rest) :-
    parse(String, Fragment, Rest, '#', [''], [], _),
    !.

fragment([], [], []) :- !.

uri2(Input, [m,a,i,l,t,o], Userinfo, Host, [], [], [], []) :-
    mailto(Input, Userinfo, Host),
    !.

uri2(Input, [n,e,w,s], [], Host, [], [], [], []) :-
    news(Input, Host),
    !.

uri2(Input, [t, e, l], Userinfo, [], [], [], [], []) :-
    tel(Input, Userinfo),
    !.


uri2(Input, [z, o, s], Userinfo, Host, Port, [], Query, Fragment) :-
    authority(Input, Userinfo, Host, Port, Rest_authority),
    optional_zos(Rest_authority, [], Query, Fragment),
    !.

uri2(Input, [z, o, s], [], [], 80, Path, Query, Fragment) :-
    authority(Input, [], [], 80, ['/' | Rest_authority]),
    optional_zos(Rest_authority, Path, Query, Fragment),
    !.

uri2(Input, [z, o, s], Userinfo, Host, Port, Path, Query, Fragment) :-
    authority(Input, Userinfo, Host, Port, ['/' | Rest_authority]),
    optional_zos(Rest_authority, Path, Query, Fragment),
    Path \= [],
    !.

uri2(Input, [z, o, s], Userinfo, Host, Port, Path, Query, Fragment) :-
    authority(Input, Userinfo, Host, Port,  Input),
    optional_zos(Input, Path, Query, Fragment),
    !.

uri2(Input, [f,a,x], Userinfo, [], [], [], [], []) :-
    fax(Input, Userinfo),
    !.

mailto(String, Userinfo, Host) :-
    mailto_userinfo(String, Userinfo, Rest_userinfo),
    not_empty(Rest_userinfo),
    !,
    mailto_host(Rest_userinfo, Host, Rest_host),
    is_empty(Rest_host).

mailto(String, Userinfo, []) :-
    mailto_userinfo(String, Userinfo, Rest_userinfo),
    is_empty(Rest_userinfo).

mailto_userinfo(String, Userinfo, Rest) :-
    parse(String, Userinfo, R, ['', '@'], ['/', '?', '#', '@', ':'], Terminal),
    append(Terminal, R, Rest),
    !.

mailto_userinfo([], [], []) :- !.

mailto_host(['@' | String], Host, Rest) :-
    host(String, Host, Rest).

mailto_host([], [], []) :- !.

news(String, Host) :-
    host(String, Host, []),
    !.

news([], [], []) :- !.

tel( String,  Userinfo) :-
    parse(String, Userinfo, R, [''], ['/', '?', '#', '@', ':'], _),
    !,
    is_empty(R).

tel([], [] , []) :- !.

fax(String, Userinfo) :-
    parse(String, Userinfo, R, [''], ['/', '?', '#', '@', ':'], _),
    !,
    is_empty(R).

fax([], [] , []) :- !.

alnum([]) :- !.
alnum([S | String]) :-
    char_type(S, alnum),
    !,
    alnum(String).

id8(['(', I | Input], Id, Rest) :-
    char_type(I, alpha),
    parse(['(', I | Input], Id_prov, Rest, '(', [')'], [], Terminal),
    !,

    not_empty(Id_prov),
    alnum(Id_prov),
    length(Id_prov, L),
    L =< 8,

    append(['('], Id_prov, X),
    append(X, Terminal, Id).

id8([I | Input], [], [I | Input]) :-
    check_policy(I, [ ['('] ]),
    !.

id8([], [], []) :- !.

alnum_44([]) :- !.

alnum_44([S | String]) :-
    char_type(S, alnum),
    !,
    alnum_44(String).

alnum_44(['.' | String]) :-
    !,
    alnum_44(String).


id44([ I | Input], Id, Rest) :-
    char_type(I, alpha),
    parse([I | Input], Id, Rest_prov, ['(', '?', '#'], [], Terminal),
    !,

    append(Terminal, Rest_prov, Rest),

    last(Id, L),
    check_policy(L, [['.']]),

    length(Id, Le),
    Le =< 44,
    alnum_44(Id).

id44([ I | Input], Id, []) :-
    char_type(I, alpha),
    parse([I | Input], Id, [], [''], [], _),
    !,

    last(Id, L),
    check_policy(L, [['.']]),

    length(Id, Le),
    Le =< 44,
    alnum_44(Id).

path_zos([/ | _], _, _) :- !, fail.
path_zos(Input, Path, Rest) :-
    id44(Input, Id_44, Rest_44),
    id8(Rest_44, Id_8, Rest),
    append(Id_44, Id_8, Path),
    !.

path_zos([], [], []) :- !.

path_zos([I | String], [] , [I | String]) :-
    check_car_final(I, ['?', '#'], _),
    !.

optional_zos(String, Path, Query, Fragment) :-
    path_zos(String, Path, Rest_path),
    query(Rest_path, Query, Rest_query),
    fragment(Rest_query, Fragment, _),
    !.

optional_zos([], [], [], []) :-
    !.

