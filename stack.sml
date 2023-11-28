signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: unit -> 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option

    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool

    val list2stack : 'a list -> 'a Stack (* Convert a list into a stack *)
    val stack2list: 'a Stack -> 'a list (* Convert a stack into a list *)
    val toString: ('a -> string) -> 'a Stack -> string
end

structure FunStack :> STACK =
    struct
        exception EmptyStack
        exception Error of string
        type 'a Stack = 'a list

        fun create() :'a Stack = []

        fun empty(st : 'a Stack) : bool = (case st of
           [] => true
         | _ => false)

        fun push (x : 'a, s : 'a Stack) : 'a Stack = x::s

        fun pop (l:'a Stack):'a Stack = 
        (case l of 
           [] => raise EmptyStack
         | (x::xs) => xs)

        fun top (l:'a Stack):'a = 
        (case l of
           [] => raise EmptyStack
         | (x::xs) => x)
  
        fun poptop(l : 'a Stack) : ('a * 'a Stack) option = 
        (case l of [] => raise EmptyStack
        |   x::xs => SOME (x,xs))

        fun nth(l : 'a Stack, n) : 'a = 
        (case l of 
          [] => raise EmptyStack
         | (x::xs) => List.nth(x::xs,n))
        
        fun drop(l : 'a Stack, n) : 'a Stack = 
        (case l of 
          [] => raise EmptyStack
         | (x::xs) => List.drop(x::xs,n))

        fun depth(l : 'a Stack) : int = 
        length(l) 

        fun app f l  = List.app f l

        fun map f l = List.map f l

        fun mapPartial f l = List.mapPartial f l

        fun find f l = List.find f l

        fun filter f l = List.filter f l

        fun foldr f init l = List.foldr f init l

        fun foldl f init l = List.foldl f init l

        fun exists f l = List.exists f l

        fun all f l = List.all f l

        fun list2stack(l) = l

        fun stack2list(l) = l

        fun toString f [] =  ""
        | toString f (x::xs) = f(x)^toString f xs
end

