open Hlcommon

(*********************hlcheck*************************)
type source =
  | ComSrc of command
  | ExpSrc of exp
  | BoolSrc of booleanexp
  | ImpSrc of booleanexp * booleanexp
  | HLSrc of condition_exp * command * condition_exp
  | ConcrSynSrc of string

type hl_stmt = HLStmt of condition_exp parsed * command parsed * condition_exp parsed

let string_of_hl_stmt (HLStmt (pre_cond_p, com_p, post_cond_p)) =
	(string_of_parsed string_of_cond pre_cond_p) ^ " "
	^ (string_of_parsed string_of_com com_p) ^ " "
	^ (string_of_parsed string_of_cond post_cond_p) ^ "\n"

type proof_tree = ProofTree of proof_tree list * hl_stmt * condition_exp parsed

type syntax_pat =
	| PatElem of string * syntax_pat list
	| PatVar of string

	(* tree_term: the actual terms we are matching over *)
type ('a, 'source) tree_term =
	TermElem of string * (('a, 'source) tree_term) list * 'source
 	| TermObj of 'a * 'source

type ('a, 'source, 'sc_error) sc_pred =
    SCPred of string * ((('a, 'source) tree_term) cond -> 'sc_error option)

let string_of_sc_pred (SCPred (s,p)) = s

type ('a, 'source, 'sc_error) syntax_rule =
    SynRule of
        string * syntax_pat * syntax_pat list *
        ('a, 'source, 'sc_error) sc_pred list

type type_check_obj =
	| Cond of condition_exp
	| Program of command

type parsed_term = 
	| ParsedTerm of (type_check_obj, source) tree_term
	| ErrorTerm of (unit parsed) list

let pat_cons s sl = PatElem(s, List.map (fun s' -> PatVar s') sl)

type have_rule = (type_check_obj, source, string) syntax_rule

let assign_inst_sc cond = 
	if (match (lookup_cond cond "side_condition") with
		| Some (TermObj(Cond precond, _)) -> 
			(match (lookup_cond cond "s") with
				| Some s -> true
				| _ -> false)
		| _ -> false)
	then None
	else Some "Invaild side condition."
(* Find s in pre-condition, if found, it is a vaild side-condition *)

let assignRule: have_rule = SynRule("assign",
		PatElem("statement", [PatVar "pre_cond"; pat_cons "assign" ["s"; "e"]; PatVar "post_cond"]),
		[], [SCPred("assign_inst", assign_inst_sc)])

let pre_streng_inst_sc cond = 
	if (match (lookup_cond cond "side_condition") with
		| Some(TermObj(Cond(ImpExp (p, p')), _)) -> true
		| _ -> false)
	then None
	else Some "Invaild precondition strengthening."
(* Find in side_condition, if found ImpExp type, then found an implication *)

let prestrengRule: have_rule = SynRule("pre_streng",
		PatElem("statement", [PatVar "pre_cond_p"; pat_cons "post_weak" ["c"]; PatVar "post_cond_q"]), [
			PatElem("statement", [PatVar "pre_cond_p_prime"; PatVar "c"; PatVar "post_cond_q"])
		], [SCPred("pre_streng_inst", pre_streng_inst_sc)])

let post_weak_inst_sc cond = 
	if (match (lookup_cond cond "side_condition") with
		| Some(TermObj(Cond(ImpExp(q',q)), _)) -> true
		| _ -> false)
	then None
	else Some "Invaild postcondition weakening."
(* Find in side_condition, if found ImpExp type, then found an implication *)

let postweakRule: have_rule = SynRule("post_weak",
		PatElem("statement", [PatVar "pre_cond_p"; pat_cons "post_weak" ["c"]; PatVar "post_cond_q"]), [
			PatElem("statement", [PatVar "pre_cond_p"; PatVar "c"; PatVar "post_cond_q_prime"])
		], [SCPred("post_weak_inst", post_weak_inst_sc)])

let seqRule: have_rule = SynRule("sequence",
		PatElem("statement", [PatVar "pre_cond_p"; pat_cons "sequence" ["c1"; "c2"]; PatVar "post_cond_r"]), [
			PatElem("statement", [PatVar "pre_cond_p"; PatVar "c1"; PatVar "post_cond_q"]);
			PatElem("statement", [PatVar "pre_cond_q"; PatVar "c2"; PatVar "post_cond_r"])
		], [])

let ifRule: have_rule = SynRule("if",
		PatElem("statement", [PatVar "pre_cond_p"; pat_cons "if" ["b"; "c1"; "c2"]; PatVar "post_cond_q"]), [
			PatElem("statement", [PatVar "pre_cond_p_and_b"; PatVar "c1"; PatVar "post_cond_q"]);
			PatElem("statement", [PatVar "pre_cond_p_and_not_b"; PatVar "c2"; PatVar "post_cond_q"])
		], [])

let whileRule: have_rule = SynRule("while",
		PatElem("statement", [PatVar "pre_cond_p"; pat_cons "while" ["b"; "c"]; PatVar "post_cond_p_and_not_b"]), [
			PatElem("statement", [PatVar "pre_cond_p_and_b"; PatVar "c"; PatVar "post_cond_p"])
		], [])

(*let hl_rule_list =
  [(1,assignRule); (1,prestrengRule); (1,postweakRule); (1,seqRule);
   (1,ifRule); (1,whileRule)]*)

	(*
		functions for transforming into a form that can be checked
	*)

let monop_to_term m =
  let s = string_of_mon_op m in TermElem(s, [], ConcrSynSrc s)
let binop_to_term b = 
  let s = string_of_bin_op b in TermElem(s, [], ConcrSynSrc s)


let rec exp_to_term e = 
	let src = ExpSrc e in
	match e with
	| IntConst i -> (let s = string_of_int i in
      	 			TermElem("int", [TermElem(s, [], ConcrSynSrc s)], src))
	| Ident s -> TermElem("ident", [TermElem(s, [], ConcrSynSrc s)], src)
	| MonOpAppExp(m,e) -> TermElem("monop", [monop_to_term m; exp_to_term e], src)
	| BinOpAppExp(b, e1, e2) -> TermElem("binop", [binop_to_term b; exp_to_term e1; exp_to_term e2], src)

let rec bexp_to_term bexp = 
	let src = BoolSrc bexp in
	match bexp with
	| BoolConst b -> (let s = if b then "true" else "false" in
    			    TermElem("bool", [TermElem(s, [], ConcrSynSrc s)], src))
	| AndExp(b1,b2) -> TermElem("and", [bexp_to_term b1; bexp_to_term b2], src)
	| NotExp b -> TermElem("not", [bexp_to_term b], src)
	| LessExp(e1,e2) -> TermElem("less", [exp_to_term e1; exp_to_term e2], src)
	| GreaterExp(e1,e2) -> TermElem("greater", [exp_to_term e1; exp_to_term e2], src)
	| LessEqExp(e1,e2) -> TermElem("lesseq", [exp_to_term e1; exp_to_term e2], src)
	| GreaterEqExp(e1,e2) -> TermElem("greatereq", [exp_to_term e1; exp_to_term e2], src)
	| EqExp(e1,e2) -> TermElem("equal", [exp_to_term e1; exp_to_term e2], src)

let rec cond_to_term e = 
	match e with
	| Exp_exp pe -> exp_to_term pe
	| Bool_exp pb -> bexp_to_term pb
	| ImpExp(b1,b2) -> TermElem("implication", [bexp_to_term b1; bexp_to_term b2], ImpSrc(b1,b2))

let rec com_to_term com = 
	let src = ComSrc com in
	match com with
	| SkipCommand -> TermElem("skip", [], src)
	| AssignCommand(s,e) -> let s_term = TermElem("ident", [TermElem(s, [], ConcrSynSrc s)], ExpSrc (Ident s)) in
			TermElem("assign", [s_term; exp_to_term e], src)
	| SeqCommand(c1,c2) -> TermElem("sequence", [com_to_term c1; com_to_term c2], src)
	| IfCommand(b,c1,c2) -> TermElem("if", [bexp_to_term b; com_to_term c1; com_to_term c2], src)
	| WhileCommand(b,c) -> TermElem("while", [bexp_to_term b; com_to_term c], src)

let get_parsed_error p = match p with
	| ParseEmpty -> [ParseEmpty]
	| SyntaxError s -> [SyntaxError s]
	| _ -> []

let hl_stmt_to_parsed_term (HLStmt(pre_cond, command, post_cond)) =
  match (pre_cond, command, post_cond) with
    | (ParseOk pre_cond, ParseOk command, ParseOk post_cond) ->
    	let src = HLSrc(pre_cond, command, post_cond) in
        ParsedTerm (TermElem("hoarelogic", [cond_to_term pre_cond; com_to_term command; cond_to_term post_cond], src))
	| _ -> ErrorTerm ((get_parsed_error pre_cond)@(get_parsed_error command)@(get_parsed_error post_cond))



(*
  reconstruction of the proof tree from a list of each proof node
  
  NOTE: this part could easily be generalized, except that the
  underlying datatypes produced from parsing are not generalized
*)

(* checks if s1 is contained in s2 ("ab" "abc") *)

type string_tree = StringTree of string * string_tree list

let string_prefix s1 s2 =
  if String.length s1 > String.length s2 then false
  else String.sub s2 0 (String.length s1) = s1


let rec parent_check sx (s, po) =
  if string_prefix sx s && s <> sx then match po with
    | None -> (s, Some(sx))
    | Some p ->
      if string_prefix p sx then (s, Some(sx))
      else (s, po)
  else (s, po)

