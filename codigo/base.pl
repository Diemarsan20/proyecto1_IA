% =====================
% Declaraciones
% =====================
:- discontiguous usuario/1.
:- discontiguous categoria/1.
:- discontiguous producto/2.
:- discontiguous compra/2.
:- discontiguous calificacion/3.

% ==== Usuarios ====
usuario(juan).
usuario(maria).
usuario(carlos).
usuario(laura).
usuario(andres).

% ==== Categorías ====
categoria(tecnologia).
categoria(hogar).
categoria(ropa).
categoria(libros).
categoria(deportes).

% ==== Productos poco convencionales ====
producto(impresora_3d, tecnologia).
producto(drone, tecnologia).
producto(teclado_mecanico, tecnologia).
producto(hamaca, hogar).
producto(maquina_de_cafe, hogar).
producto(abrigo_vintage, ropa).
producto(botines_cuero, ropa).
producto(libro_rareza, libros).
producto(manuscrito_antiguo, libros).
producto(raqueta_badminton, deportes).
producto(monociclo, deportes).

% ==== Compras (usuario, producto) ====
compra(juan, impresora_3d).
compra(juan, teclado_mecanico).
compra(maria, hamaca).
compra(maria, maquina_de_cafe).
compra(carlos, raqueta_badminton).
compra(carlos, monociclo).
compra(laura, abrigo_vintage).
compra(laura, botines_cuero).
compra(andres, libro_rareza).
compra(andres, manuscrito_antiguo).
compra(juan, raqueta_badminton).
compra(maria, drone).
compra(laura, impresora_3d).

% ==== Calificaciones (usuario, producto, valor 1-5) ====
calificacion(juan, impresora_3d, 5).
calificacion(juan, teclado_mecanico, 4).
calificacion(juan, raqueta_badminton, 3).
calificacion(maria, hamaca, 5).
calificacion(maria, maquina_de_cafe, 4).
calificacion(maria, drone, 2).
calificacion(carlos, raqueta_badminton, 5).
calificacion(carlos, monociclo, 4).
calificacion(laura, abrigo_vintage, 3).
calificacion(laura, botines_cuero, 5).
calificacion(laura, impresora_3d, 4).
calificacion(andres, libro_rareza, 5).
calificacion(andres, manuscrito_antiguo, 3).

% =====================
% Reglas
% =====================

% Registrar usuario
registrar_usuario(U) :-
    \+ usuario(U),
    assert(usuario(U)).

% Registrar producto
registrar_producto(P, C) :-
    categoria(C),
    \+ producto(P, C),
    assert(producto(P, C)).

% Registrar compra
registrar_compra(U, P) :-
    usuario(U),
    producto(P, _),
    \+ compra(U, P),
    assert(compra(U, P)).

% Registrar calificación
registrar_calificacion(U, P, V) :-
    usuario(U),
    producto(P, _),
    between(1, 5, V),
    \+ calificacion(U, P, V),
    assert(calificacion(U, P, V)).

% Categoría de interés
interes_en_categoria(U, C) :-
    compra(U, P),
    producto(P, C).

% Recomendación puntual
recomendar_uno(U, P) :-
    interes_en_categoria(U, C),
    producto(P, C),
    \+ compra(U, P).

% Recomendación en lista (sin duplicados)
recomendar_lista(U, Lista) :-
    setof(P, recomendar_uno(U, P), Lista), !.
recomendar_lista(_, []).

% Productos que gustaron (calificación > 3)
gusto(U, P) :-
    calificacion(U, P, V),
    V > 3.

% Acumular productos gustados de varios usuarios
productos_gustados([], []).
productos_gustados([U|Us], Lista) :-
    findall(P, gusto(U, P), ListaU),
    productos_gustados(Us, ListaResto),
    append(ListaU, ListaResto, Lista).

% Contar frecuencia
contar([], []).
contar([H|T], [H-N|Cont]) :-
    contar_aux(H, [H|T], N, Resto),
    contar(Resto, Cont).

contar_aux(_, [], 0, []).
contar_aux(E, [E|T], N, Resto) :-
    contar_aux(E, T, N1, Resto),
    N is N1 + 1.
contar_aux(E, [X|T], N, [X|Resto]) :-
    E \= X,
    contar_aux(E, T, N, Resto).

% Tomar hasta 10 elementos
take10(List, First10) :-
    length(First10, N),
    N =< 10,
    append(First10, _, List).

% Top 10 productos gustados
top10(Usuarios, Top10) :-
    productos_gustados(Usuarios, Lista),
    contar(Lista, Contados),
    sort(2, @>=, Contados, Ordenados),
    take10(Ordenados, Top10).

% Usuarios similares
usuario_similar(U1, U2) :-
    compra(U1, P),
    compra(U2, P),
    U1 \= U2.

% Recomendación recursiva
recomendar_recursivo(U, P) :-
    usuario_similar(U, Otro),
    compra(Otro, P),
    \+ compra(U, P).
recomendar_recursivo(U, P) :-
    usuario_similar(U, Otro),
    recomendar_recursivo(Otro, P),
    \+ compra(U, P).
