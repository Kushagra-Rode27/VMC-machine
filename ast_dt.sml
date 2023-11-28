structure AST =
struct
exception typeMismatch;
exception undeclaredVariableFound;
val MemAddTable : (string, int) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (100, Fail "Variable Not Declared")

val addr = ref 0;
fun addvar(x) = 
		let 
			val ins = HashTable.insert MemAddTable(x,!addr); 
			val p = (addr := !addr + 1);
		in 
			x 
		end;
fun getVarAdd(v)=HashTable.lookup MemAddTable (v);

datatype binop = PLUS | MINUS | DIV | MOD | TIMES | LT | LEQ | GT | GEQ | EQ | NEQ | AND | OR
and unop = TILDE | NOT

datatype basictype = INT | BOOL
type varlist = string list
type vlist = ( basictype * string) list;

datatype program = PROG of string * blk

and blk = BLK of decl * cmdseq
and decl = DEC of vlist * decl |  empty1
and cmdseq = SEQ of cmd * cmdseq |  empty
and cmd = SET of string * exp | READ of string | WRITE of exp | ITE of exp * cmdseq *  cmdseq | WH of exp * cmdseq
and exp = TT | FF
		| NUM of int
		| ID of string
		| Binexp of binop * exp * exp
		| Unexp of unop * exp
		| Paren of exp

(*Type checking is done in the section below so that some invalid expressions like tt + 1 etc. do not get parsed by the parser*)
val symbolTable : (string, basictype) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (20, Fail "Variable Not Declared")

fun addVar(v,typ) = 
		let 
			val ins = HashTable.insert symbolTable(v,typ); 
		in 
			v 
		end;

fun getVartype(v)=HashTable.lookup symbolTable (v);

val varFinalList = [] : vlist;
fun makeID(v :: [],ty) = (ty,addVar(addvar(v),ty)) :: varFinalList
  |makeID(v::vl : varlist,ty) = (ty,addVar(addvar(v),ty)) :: makeID(vl,ty)
  |makeID([],ty) = varFinalList;

fun sameType(x,y) = (x = INT andalso y = INT) orelse (x = BOOL andalso y = BOOL);

fun typeOfExp(Binexp(PLUS,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(MINUS,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(TIMES,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(DIV,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(MOD,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(LT,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(LEQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(GT,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(GEQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(EQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(NEQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(AND,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(OR,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Unexp(NOT,x))=if typeOfExp(x)=BOOL then BOOL else raise typeMismatch
 | typeOfExp(Paren(x))= typeOfExp(x)
 | typeOfExp(Unexp(TILDE,x))=if typeOfExp(x)=INT then INT else raise typeMismatch
 | typeOfExp(NUM(x))=INT
 | typeOfExp(ID(x))=getVartype(x)
 | typeOfExp(TT)=BOOL
 | typeOfExp(FF)=BOOL;


fun typecmdcheck(SET(x,y)) =  getVartype(x) = typeOfExp(y)
  | typecmdcheck(ITE(x,y,z))= 
	let 
		fun typecmdseqcheck(SEQ(x,y)) = typecmdcheck(x) andalso typecmdseqcheck(y)
			| typecmdseqcheck(empty)=true;
	in 
  		typeOfExp(x)=BOOL andalso typecmdseqcheck(y) andalso typecmdseqcheck(z)
	end
  | typecmdcheck(WH(x,y))=
  	let 
		fun typecmdseqcheck(SEQ(x,y)) = typecmdcheck(x) andalso typecmdseqcheck(y)
			| typecmdseqcheck(empty)=true;
	in 
  		typeOfExp(x)=BOOL andalso typecmdseqcheck(y)
	end
  | typecmdcheck(READ(x)) =  if HashTable.inDomain symbolTable(x) then true else raise undeclaredVariableFound
  | typecmdcheck(WRITE(x)) = typeOfExp(x) = INT;

fun typeseqcheck(SEQ(x,y)) = typecmdcheck(x) andalso typeseqcheck(y)
  | typeseqcheck(empty)=true

fun typeblkcheck(BLK(x,y)) = typeseqcheck(y);

fun typecheck(PROG(x,y))=typeblkcheck(y);

fun finalProgcheck(x,y)=if y then x else raise typeMismatch;

(*Code below is for converting the AST into postfix stack*)

datatype TypeOfStack = ite | set | seq | wh | plus | minus | Divi | modulo | times | lt | leq | gt | geq | eq | neq | And | Or | Tilde | Not  | Id of string | Numeral of int |
 writes | Exp of TypeOfStack FunStack.Stack | Bexp of TypeOfStack FunStack.Stack | CMd of TypeOfStack FunStack.Stack | CMdseq of TypeOfStack FunStack.Stack


fun PostFixExp(Binexp(PLUS,x,y),st)= PostFixExp(x,PostFixExp(y,FunStack.push(plus,st)))
  | PostFixExp(Binexp(MINUS,x,y),st)= PostFixExp(x,PostFixExp(y,FunStack.push(minus,st)))
 | PostFixExp(Binexp(TIMES,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(times,st)))
 | PostFixExp(Binexp(DIV,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(Divi,st)))
 | PostFixExp(Binexp(MOD,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(modulo,st)))

 | PostFixExp(Binexp(AND,x,y),st)= PostFixExp(x,PostFixExp(y,FunStack.push(And,st)))
 | PostFixExp(Binexp(OR,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(Or,st)))
 | PostFixExp(Unexp(NOT,x),st)=PostFixExp(x,FunStack.push(Not,st))

 | PostFixExp(Unexp(TILDE,x),st)=PostFixExp(x,FunStack.push(Tilde,st))
 | PostFixExp(NUM(x),st)= FunStack.push(Numeral(x),st)
 | PostFixExp(ID(x),st)= FunStack.push(Id(x),st)
 | PostFixExp(TT,st)= FunStack.push(Numeral(1),st)
 | PostFixExp(FF,st)= FunStack.push(Numeral(0),st)

 | PostFixExp(Binexp(LT,x,y),st)= PostFixExp(x,PostFixExp(y,FunStack.push(lt,st)))
 | PostFixExp(Binexp(LEQ,x,y),st)= PostFixExp(x,PostFixExp(y,FunStack.push(leq,st)))
 | PostFixExp(Binexp(GT,x,y),st)= PostFixExp(x,PostFixExp(y,FunStack.push(gt,st)))
 | PostFixExp(Binexp(GEQ,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(geq,st)))
 | PostFixExp(Binexp(EQ,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(eq,st)))
 | PostFixExp(Binexp(NEQ,x,y),st)=PostFixExp(x,PostFixExp(y,FunStack.push(neq,st)))
 | PostFixExp(Paren(x),st)= PostFixExp(x,st)


fun PostFixCmd(SET(x,y),st) = FunStack.push(Id(x),FunStack.push(Exp(PostFixExp(y,FunStack.create())),FunStack.push(set,st)))

  | PostFixCmd(ITE(x,y,z),st)= 
	let 
		fun  PostFixcmdseq(empty,st)= st
    | PostFixcmdseq(SEQ(x,y),st) = 
  
  FunStack.push(CMd(PostFixCmd(x,FunStack.create())), FunStack.push(CMdseq(PostFixcmdseq(y,FunStack.create())),FunStack.push(seq,st)))
  
	in 
  		FunStack.push(Bexp(PostFixExp(x,FunStack.create())),FunStack.push(CMdseq(PostFixcmdseq(y,FunStack.create())), FunStack.push(CMdseq(PostFixcmdseq(z,FunStack.create())),FunStack.push(ite,st))))
	end

  | PostFixCmd(WH(x,y),st)= 
	let 
		fun PostFixcmdseq(SEQ(x,y),st) = 
  FunStack.push(CMd(PostFixCmd(x,FunStack.create())), FunStack.push(CMdseq(PostFixcmdseq(y,FunStack.create())),FunStack.push(seq,st)))

			| PostFixcmdseq(empty,st)= st;
	in 
  		FunStack.push(Bexp(PostFixExp(x,FunStack.create())),FunStack.push(CMdseq(PostFixcmdseq(y,FunStack.create())),FunStack.push(wh,st)))
  end
  | PostFixCmd(READ(x),st) = st
  | PostFixCmd(WRITE(x),st) = PostFixExp(x,FunStack.push(writes,st))

fun PostFixSeq(empty,st) = st
    | PostFixSeq(SEQ(x,empty),st) =FunStack.push(CMd(PostFixCmd(x,FunStack.create())),st)
    | PostFixSeq(SEQ(x,(SEQ(y,z))),st) = FunStack.push(CMd(PostFixCmd(x,FunStack.create())), PostFixSeq(SEQ(y,z),FunStack.push(seq,st)));

fun PostFixBlk(BLK(DEC(x),y),st) =  PostFixSeq(y,st);

fun postfix(PROG(a,b)) = PostFixBlk(b,FunStack.create());

end



