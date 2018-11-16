(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.SUB (ex1, ex2) -> trans ex1 @ trans ex2 @ [Sm5.SUB]
    | K.MUL (ex1, ex2) -> trans ex1 @ trans ex2 @ [Sm5.MUL]
    | K.DIV (ex1, ex2) -> trans ex1 @ trans ex2 @ [Sm5.DIV]
    | K.EQUAL (ex1, ex2) -> trans ex1 @ trans ex2 @ [Sm5.EQ]
    | K.LESS (ex1, ex2) -> trans ex1 @ trans ex2 @ [Sm5.LESS]
    | K.NOT ex -> trans ex @ [Sm5.NOT]
    | K.ASSIGN (id, ex) -> trans ex @ [Sm5.PUSH (Sm5.Id id); Sm5.STORE; Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.SEQ (ex1, ex2) -> trans ex1 @ [Sm5.POP] @ trans ex2 (*SEQ doesnt change env?*)
    | K.IF (ex, ex1, ex2) -> trans ex @ [Sm5.JTR (trans ex1, trans ex2)]
    | K.WHILE (e1, e2) -> (
      [Sm5.PUSH (Sm5.Fn("$", [Sm5.BIND "@"] @ trans e1 @ [Sm5.JTR(trans e2 @ [Sm5.PUSH (Sm5.Id "@");Sm5.PUSH (Sm5.Id "@");
      Sm5.PUSH (Sm5.Val (Sm5.B true)); Sm5.PUSH (Sm5.Id "$"); Sm5.CALL; Sm5.POP], [Sm5.PUSH (Sm5.Val (Sm5.Unit))]); Sm5.UNBIND])); Sm5.BIND "@"] @ [Sm5.PUSH (Sm5.Id "@"); Sm5.PUSH (Sm5.Id "@");
      Sm5.PUSH (Sm5.Val (Sm5.B true)); Sm5.MALLOC; Sm5.CALL] @ [Sm5.POP]
      (* trans e1 @ [Sm5.JTR(trans e2 @ [Sm5.POP] @ , Sm5.PUSH (Sm5.Val (Sm5.Unit)))] *)
      (* Sm5.MALLOC; Sm5.BIND "!"; Sm5.PUSH (Sm5.Id "!"); Sm5.STORE; Sm5.PUSH (Sm5.Id "!"); Sm5.LOAD;  *)
    )
    (* | K.FOR (id, ex1, ex2, ex3) -> () *)
    | K.LETF (f, x, e1, e2) -> (
      [Sm5.PUSH (Sm5.Fn(x, [Sm5.BIND f] @ trans e1 @ [Sm5.UNBIND])); Sm5.BIND f] @ trans e2 @ [Sm5.UNBIND; Sm5.POP]
    )
    | K.CALLV (f, ex) -> (
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans ex @ [Sm5.MALLOC; Sm5.CALL; Sm5.POP]
    )
    | K.CALLR (f, arg) -> (
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id arg); Sm5.LOAD; Sm5.PUSH (Sm5.Id arg); Sm5.CALL; Sm5.POP]
    )
    | K.WRITE ex -> (trans ex @ [Sm5.MALLOC; Sm5.BIND "#"; Sm5.PUSH (Sm5.Id "#"); Sm5.STORE; Sm5.PUSH (Sm5.Id "#"); Sm5.LOAD; Sm5.PUT; Sm5.PUSH (Sm5.Id "#"); Sm5.LOAD; Sm5.UNBIND; Sm5.POP])
    | _ -> failwith "Unimplemented"

end
