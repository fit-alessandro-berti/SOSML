import { IdentifierStatus } from '../basic';
import { State, DynamicBasis, StaticBasis } from '../state';
import { TypeVariable, TypeVariableBind, FunctionType, CustomType, TupleType } from '../types';
import { ValueConstructor } from '../values';
import { Module } from '../stdlib';

function addListLib(state: State): State {
    let dres = new DynamicBasis({}, {}, {}, {}, {});
    let sres = new StaticBasis({}, {}, {}, {}, {});

    sres.setType('list', new CustomType('list', [new TypeVariable('\'a')]), ['nil', '::'], 1, true);
    sres.setValue('nil', new TypeVariableBind('\'a',
        new CustomType('list', [new TypeVariable('\'a')])), IdentifierStatus.VALUE_CONSTRUCTOR);
    sres.setValue('::', new TypeVariableBind('\'a', new FunctionType(
        new TupleType([new TypeVariable('\'a'),
            new CustomType('list', [new TypeVariable('\'a')])]),
        new CustomType('list', [new TypeVariable('\'a')]))).simplify(),
        IdentifierStatus.VALUE_CONSTRUCTOR);

    dres.setType('list', ['nil', '::']);
    dres.setValue('nil', new ValueConstructor('nil'), IdentifierStatus.VALUE_CONSTRUCTOR);
    dres.setValue('::', new ValueConstructor('::', 1), IdentifierStatus.VALUE_CONSTRUCTOR);

    state.setDynamicStructure('List', dres);
    state.setStaticStructure('List', sres);
    return state;
}

export let LIST_LIB: Module = { /* complete */
    'native': addListLib,
    'code': `
        signature LIST = sig
            datatype 'a list = nil | :: of 'a * 'a list;
            exception Empty
            val null : 'a list -> bool
            val hd : 'a list -> 'a
            val tl : 'a list -> 'a list
            val last : 'a list -> 'a
            val getItem : 'a list -> ('a * 'a list) option
            val nth : 'a list * int -> 'a
            val take : 'a list * int -> 'a list
            val drop : 'a list * int -> 'a list
            val length : 'a list -> int
            val rev : 'a list -> 'a list
            val @ : 'a list * 'a list -> 'a list
            val concat : 'a list list -> 'a list
            val revAppend : 'a list * 'a list -> 'a list
            val app : ('a -> unit) -> 'a list -> unit
            val map : ('a -> 'b) -> 'a list -> 'b list
            val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
            val find : ('a -> bool) -> 'a list -> 'a option
            val filter : ('a -> bool) -> 'a list -> 'a list
            val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
            val permutations: 'a list -> 'a list list
            val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
            val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
            val exists : ('a -> bool) -> 'a list -> bool
            val all : ('a -> bool) -> 'a list -> bool
            val tabulate : int * (int -> 'a) -> 'a list
            val collate : ('a * 'a -> order) -> 'a list * 'a list -> order
        end;

        structure List : sig
            datatype 'a list = nil | :: of 'a * 'a list;
            val rev: 'a list -> 'a list;
        end  = struct
            open List;
            fun rev' nil ys     = ys
              | rev' (x::xs) ys = rev' xs (x::ys)
            fun rev xs = rev' xs nil;
        end;

        structure List = struct
            exception Empty;
            open List;

            fun hd nil = raise Empty
              | hd (x::xr) = x;

            fun tl nil = raise Empty
              | tl (x::xr) = xr;

            fun null nil = true
              | null (x::xr) = false;

            fun map f nil = nil
              | map f (x::xr) = (f x) :: (map f xr);

            infixr 5 @;
            fun [] @ ys = ys
              | (x::xr) @ ys = x :: (xr @ ys);

            fun length nil = 0
              | length (x::xr) = 1 + length xr;

            fun foldr f e []      = e
              | foldr f e (x::xr) = f(x, foldr f e xr);

            fun foldl f e []      = e
              | foldl f e (x::xr) = foldl f (f(x, e)) xr;

            fun app (f: 'a -> unit) [] = ()
              | app (f: 'a -> unit) (x::xs) = (f x; app f xs);
        end;
        open List;
        infixr 5 @;

        structure List : LIST = struct
            open List;

            fun concat nil = nil
              | concat (x::xr) = x @ concat xr;

            fun tabulate (n, f) = let
                fun h i = if i < n then f i :: h (i + 1) else []
            in
                if n < 0 then raise Size else h 0
            end;

            fun exists p []      = false
              | exists p (x::xr) = p x orelse exists p xr;

            fun all p []      = true
              | all p (x::xr) = p x andalso all p xr;

            fun filter p []      = []
              | filter p (x::xr) = if p x then x :: filter p xr else filter p xr;

            fun collate (compare : 'a * 'a -> order) p = case p of
                (nil, _::_)     => LESS
              | (nil, nil)      => EQUAL
              | (_::_, nil)     => GREATER
              | (x::xr, y::yr)  => case compare (x, y) of
                     EQUAL  => collate compare (xr, yr)
                   | s      => s;

            fun nth ([], _)    = raise Subscript
              | nth (x::xs, 0) = x
              | nth (x::xs, n) = nth (xs, n - 1);

            fun last [x] = x
              | last (x::xs) = last xs
              | last [] = raise Empty;

            fun getItem [] = NONE
              | getItem x = SOME (hd x, tl x);

            fun take (x, 0) = []
              | take ([], _) = raise Subscript
              | take (x::xs, n) = x :: take (xs, n - 1);

            fun drop (x, 0) = x
              | drop ([], _) = raise Subscript
              | drop (x::xs, n) = drop (xs, n - 1);

            fun revAppend (l1, l2) = (rev l1) @ l2;

            fun mapPartial f l
                = ((map valOf) o (filter isSome) o (map f)) l;

            fun permutations x = let
                fun p2 a [] = [ a ]
                  | p2 a [x] = map (fn y => x::y) ( p2 [] a )
                  | p2 a (x::xs) = ( map ( fn y => x::y ) (p2 [] (a@xs))) @ (p2 (a @ [x]) xs )
            in
                p2 [] x
            end;

            fun find f [] = NONE
              | find f (x::xs) = if f x then SOME x else find f xs;

            fun partition f [] = ([], [])
              | partition f (x::xs) = let
                val tmp = partition f xs
            in
                if f x then (x :: #1 tmp, #2 tmp)
                else (#1 tmp, x :: #2 tmp)
            end;
        end;
    `,
    'requires': ['Option']
};
