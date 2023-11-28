exception NotBooleanValue
open FunStack
open AST

val maxMemSize = 20;
val mem = Array.array(maxMemSize,0);
fun getElem(ind) = Array.sub(mem,ind);
fun addVartoMem(i,ind) = Array.update(mem,ind,i);


fun isFalse(Numeral(0)) = true
  | isFalse(_) = false;
fun isTrue(Numeral(1)) = true
  | isTrue(_) = false;
fun isNum(Numeral(x)) = true
  | isNum(_) = false;
fun isVar(Id(x)) = true
  | isVar(_) = false;
fun isWhile(wh) = true
  | isWhile(_) = false;
fun isITE(ite) = true
  | isITE(_) = false;
fun isSEQ(seq) = true
  | isSEQ(_) = false;
fun isCMD(CMd(x)) = true
  | isCMD(_) = false;
fun isEXP(Exp(x)) = true
  |isEXP(_) = false;
fun isBexp(Bexp(x)) = true
  |isBexp(_) = false;
fun isCMDSEQ(CMdseq(x)) = true
  | isCMDSEQ(_) = false;
fun isSET(set) = true
  | isSET(_) = false;
fun isPLUS(plus) = true
  | isPLUS(_) =false;
fun isMINUS(minus) = true
  | isMINUS(_) =false;
fun isDIV(Divi) = true
  | isDIV(_) =false;
fun isTIMES(times) = true
  | isTIMES(_) =false;
fun isMOD(modulo) = true
  | isMOD(_) =false;
fun isTILDE(Tilde) = true
  | isTILDE(_) =false;
fun isNOT(Not) = true
  | isNOT(_) =false;
fun isAND(And) = true
  | isAND(_) =false;
fun isOR(Or) = true
  | isOR(_) =false;
fun isGT(gt) = true
  | isGT(_) =false;
fun isGEQ(geq) = true
  | isGEQ(_) =false;
fun isLT(lt) = true
  | isLT(_) =false;
fun isLEQ(leq) = true
  | isLEQ(_) =false;
fun isEQ(eq) = true
  | isEQ(_) =false;
fun isNEQ(neq) = true
  | isNEQ(_) =false;
fun isWrite(writes) = true
  | isWrite(_) = false;
(* fun isRead(reads) = true
  | isRead(_) = false; *)
fun valueInt(Numeral(x)) = x;

fun valueStr(Id(x)) = x;

fun valueBool(Numeral(0)) = false
  | valueBool(Numeral(1)) = true
  | valueBool(_) = raise NotBooleanValue; 

fun valueCMDSEQ(CMdseq(x)) = x;
fun valueCMD(CMd(x)) = x
fun valueEXP(Exp(x)) = x;
fun valueBoolExp(Bexp(x)) = x;

fun makeBool(x) = if x = true then Numeral(1) else Numeral(0);


fun PushElem(st,finalst) = FunStack.list2stack(FunStack.stack2list(st) @ FunStack.stack2list(finalst));

fun stElems2Str(ite)= "ITE, "
  | stElems2Str(wh) = "WHILE, "
  | stElems2Str(set) = "SET, "
  | stElems2Str(seq) = "SEQ, "
  | stElems2Str(plus) = "+, "
  | stElems2Str(minus) = "-, "
  | stElems2Str(times) = "*, "
  | stElems2Str(Divi) = "/, " 
  | stElems2Str(modulo) = "%, "
  | stElems2Str(lt) = "<, "
  | stElems2Str(leq) = "<=, "
  | stElems2Str(gt) = ">, "
  | stElems2Str(geq) = ">=, "
  | stElems2Str(eq) = "=, "
  | stElems2Str(neq) = "<>, "
  | stElems2Str(writes) = "Write, "
  (* | stElems2Str(reads) = "Read, " *)
  | stElems2Str(Tilde) = "~, "
  | stElems2Str(Not) = "!, "
  | stElems2Str(And) = "&&, "
  | stElems2Str(Or) = "||, "
  | stElems2Str(Id(x)) = x^", "
  | stElems2Str(Numeral(x)) = Int.toString(x)^", "

  | stElems2Str(Exp(x)) = FunStack.toString stElems2Str x

  | stElems2Str(Bexp(x)) = FunStack.toString stElems2Str x

  | stElems2Str(CMd(x)) = FunStack.toString stElems2Str x 

  | stElems2Str(CMdseq(x)) = FunStack.toString stElems2Str x;


fun arr2Str(arr,n,acc) = if n = Array.length(arr) then acc else arr2Str(arr,n+1,acc^Int.toString(Array.sub(arr,n))^", ");

signature VMC =
sig
  val rules : TypeOfStack FunStack.Stack * int array * TypeOfStack FunStack.Stack -> TypeOfStack FunStack.Stack * int array * TypeOfStack FunStack.Stack
  val toString : TypeOfStack FunStack.Stack * int array * TypeOfStack FunStack.Stack -> string list * string list * string list
end;

structure Vmc :> VMC =
struct
    fun rules(v,m,c) = 
        let 
          val p = if FunStack.depth(c) >=1 then FunStack.top(c) else Numeral(1);
          val q = if FunStack.depth(v) >=1 then FunStack.top(v) else Numeral(1);
          val a = if FunStack.depth(c) >=2 then FunStack.nth(c,1) else Numeral(1);
          val b = if FunStack.depth(c) >=3 then FunStack.nth(c,2) else Numeral(1);
          val f = if FunStack.depth(c) >=4 then FunStack.nth(c,3) else Numeral(1);
          val e = if FunStack.depth(v) >=2 then FunStack.nth(v,1) else Numeral(1);
          val d = if FunStack.depth(v) >=3 then FunStack.nth(v,2) else Numeral(1);
        in
        if isWhile(p) then 
            if isFalse(q) then (FunStack.drop(v,3),m,FunStack.pop(c)) else
       (FunStack.drop(v,3),m,PushElem(valueCMDSEQ(e),FunStack.push(d,FunStack.push(e,c)))) 
        
        else if (isWhile(b) andalso isBexp(p) andalso isCMDSEQ(a)) then (FunStack.push(a,FunStack.push(p,v)),m,PushElem(valueBoolExp(p),FunStack.drop(c,2)))  (*FunStack.push(a,FunStack.drop(c,2))*)
    
        else if isITE(f) andalso (isBexp(p) orelse isNum(p)) then 
            case isBexp(p) of
               true =>  (v,m,PushElem(valueBoolExp(p),FunStack.pop(c)))
             | false => (FunStack.push(p,v),m,FunStack.pop(c))
            
        else if isITE(b) then 
             if isFalse(q) then (FunStack.pop(v),m, PushElem(valueCMDSEQ(a),FunStack.drop(c,3))) else (FunStack.pop(v),m, PushElem(valueCMDSEQ(p),FunStack.drop(c,3)))
        
        else if isSEQ(b) andalso (isCMD(p) andalso isCMD(a))then (v,m,PushElem(valueCMD(p),PushElem(valueCMD(a),FunStack.drop(c,3))))
        else if isSEQ(p)  then (v,m,FunStack.pop(c))
        
        else if (isCMD(p)) then   (v,m,PushElem(valueCMD(p),FunStack.pop(c))) 
        else if (isCMDSEQ(p)) then (v,m,PushElem(valueCMDSEQ(p),FunStack.pop(c)))  
        else if (isSET(b) andalso isEXP(a)) then  (FunStack.push(p,v),m,PushElem(valueEXP(a),FunStack.drop(c,2)))
        else if isSET(p) then let val memupd = addVartoMem(valueInt(q),getVarAdd(valueStr(e))); in (FunStack.drop(v,2),m,FunStack.pop(c)) end
        else if FunStack.empty(c) then (v,m,c)
        else if isWrite(p) then let val k = print(""^Int.toString(valueInt(q))^"\n"); in (FunStack.pop(v),m,FunStack.pop(c)) end
        else if isPLUS(p) then (FunStack.push(Numeral(valueInt(e) + valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isMINUS(p) then (FunStack.push(Numeral(valueInt(e) - valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isTIMES(p) then (FunStack.push(Numeral(valueInt(e) * valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isDIV(p) then (FunStack.push(Numeral(valueInt(e) div valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isMOD(p) then (FunStack.push(Numeral(valueInt(e) mod valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        
        else if isGT(p) then (FunStack.push(makeBool(valueInt(e) > valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isGEQ(p) then (FunStack.push(makeBool(valueInt(e) >= valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isLT(p) then (FunStack.push(makeBool(valueInt(e) < valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isLEQ(p) then (FunStack.push(makeBool(valueInt(e) <= valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isEQ(p) then (FunStack.push(makeBool(valueInt(e) = valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        else if isNEQ(p) then (FunStack.push(makeBool(valueInt(e) <> valueInt(q)),FunStack.drop(v,2)),m,FunStack.pop(c))
        
        else if isAND(p) then  (FunStack.push(makeBool(valueBool(e) andalso valueBool(q)),FunStack.drop(v,2)),m,FunStack.pop(c)) 
        else if isOR(p) then (FunStack.push(makeBool(valueBool(e) orelse valueBool(q)),FunStack.drop(v,2)),m,FunStack.pop(c)) 
        else if isNOT(p) then (FunStack.push(makeBool(not (valueBool(q))),FunStack.drop(v,1)),m,FunStack.pop(c))
        else if isTILDE(p) then (FunStack.push(Numeral((~1)*(valueInt(q))),FunStack.drop(v,1)),m,FunStack.pop(c))

        else if isNum(p) then (FunStack.push(p,v),m,FunStack.pop(c)) 
        else if isVar(p) then (FunStack.push(Numeral(getElem(getVarAdd(valueStr(p)))),v),m,FunStack.pop(c)) 
        else (v,m,FunStack.create())
        end


    fun toString(v,m,c) = ([FunStack.toString stElems2Str v],[arr2Str(mem,0,"")],[FunStack.toString stElems2Str c])

end;

fun helperExecute(v,m,c) = if FunStack.empty(c) then let 
        val ([p],[q],[r]) = Vmc.toString(v,m,c);
        val k = print("VMC Final State : "^"[ "^p^" ]"^", "^"[ "^q^" ]"^", "^"[ "^r^" ]"^"\n" ); 
        in () end
    else 
      let
        (* val ([p],[q],[r]) = Vmc.toString(v,m,c);
        val k = print("VMC State : "^"[ "^p^" ]"^", "^"[ "^q^" ]"^", "^"[ "^r^" ]"^"\n" ); *)
        val (v_c,m_c,c_c) = Vmc.rules(v,m,c);
      in 
      helperExecute(v_c,m_c,c_c)
      end;
fun execute(postFixStack) = let val k = addr := 0 in helperExecute(FunStack.create(),mem,postFixStack) end;