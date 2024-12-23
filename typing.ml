open Llbc

let dummy_initial_node_convention = 0 (* By convention *)

type stype =
    One | Bot | Neg of stype | Tens of stype * stype 

type ('a) annoted_stype =
  | One_ of 'a
  | Bot_ of 'a
  | Neg_ of 'a * 'a annoted_stype 
  | Tens_ of 'a * 'a annoted_stype * 'a annoted_stype 

type styped_term_p = stype * term_p
type styped_term_n = stype * term_n
type styped_term = 
  | STerm_p of styped_term_p
  | STerm_n of styped_term_n
    

type ('a) annoted_term_p =
  | Ep_ of 'a
  | Lp_ of 'a * var_p * 'a annoted_term_p
  | Vp_ of 'a * var_p * 'a annoted_term_n
  | Pp_ of 'a * 'a annoted_term_p * 'a annoted_term_p
  | Mp_ of 'a * 'a annoted_term_p * 'a annoted_term_n
  | Cp_ of 'a * (var_p * var_n) list * var_n list  * 'a annoted_term_p 
  | Mu_ of 'a * string * 'a annoted_term_p
  | Ap_ of 'a * string

and ('a) annoted_term_n = 
  | En_ of 'a
  | Ln_ of 'a * var_n * 'a annoted_term_n
  | Vn_ of 'a * var_n * 'a annoted_term_p
  | Pn_ of 'a * 'a annoted_term_n * 'a annoted_term_n
  | Mn_ of 'a * 'a annoted_term_n * 'a annoted_term_n
  | Inn_ of 'a * 'a annoted_term_p


type ('a) annoted_term = 
  | Term_p_ of 'a annoted_term_p
  | Term_n_ of 'a annoted_term_n



let rec string_of_stype = function 
  | One -> " 1 "
  | Bot -> "bot"
  (*
  | Neg(ty) -> "not (" ^ (string_of_stype ty) ^ ")"
  *)
  | Neg(ty) -> "(" ^ (string_of_stype ty) ^ ") => bot"
  (* We do not display parenthesis to delimit factors in the product;
  this implicitely uses the fact that the product has the lowest proority,
  and might need to be changed if the types are extended.*)
  | Tens(ty1, ty2) -> (string_of_stype ty1) ^ " * " ^ (string_of_stype ty2)

let rec latex_of_stype = function 
  | One -> "\\One"
  | Bot -> "\\Bottom"
  (*
  | Neg(ty) -> "\\neg (" ^ (string_of_stype ty) ^ ")"
  *)
  | Neg(ty) ->  "(" ^ (string_of_stype ty) ^ ") \\Arrow \\Bottom"
  (* We do not display parenthesis to delimit factors in the product;
  this implicitely uses the fact that the product has the lowest proority,
  and might need to be changed if the types are extended.*)
  | Tens(ty1, ty2) -> (latex_of_stype ty1) ^ " \\Tens " ^ (latex_of_stype ty2)


(* Is called "finalise" like finalise_latex in Llbc, but actually
   only retruns a string. *)
let finalise_tikz_figure s =
  "
\\begin{center}
\\scalebox{1.0}{
\\begin{tikzpicture}[scale=1.5, >=stealth'] 
\\tikzstyle{type} = [];
\\tikzstyle{move} = [circle, draw, minimum size=.8mm, inner sep=.8mm];
\\tikzstyle{hidden} = [circle, minimum size=.8mm, inner sep=.8mm];
\\tikzstyle{term}=[fill=white, rectangle]
"
  ^ s ^
    "
\\end{tikzpicture}
} %scalebox
\\end{center}
"


let tikz_of_astype astype0 =
  (* "type_node_of_a", or tnofa for short, returns the node name for the annotation a*) 
  let tnofa i =
    if i >= 0 then 
      "tnode_" ^ (string_of_int i)
    else
      "tn" ^ (string_of_int ( -i) )
  in
  let current_free_node_index= ref 0 in
  let get_free_node () = 
    current_free_node_index:= !current_free_node_index - 1;
    !current_free_node_index
  in
  let tikz_of_node last_n n latex_label =
    "\\node [type, base right=0 of " ^ (tnofa last_n) ^ "]  "
    ^ "(" ^ (tnofa n) ^ ")  {$" ^ latex_label ^ "$};\n"
  in
  (* "sub last_a aterm" returns (a,s) where "a" is the new value to give to "last_a" *)
  let rec sub last_a = function
    (* To check, as this case rarely happens. *)
    | One_(a) -> 
      (a, tikz_of_node last_a a "\\One")
    | Bot_(a) -> 
      (a, tikz_of_node last_a a "\\Bottom")
    | Neg_(a, astype) -> 
      let (last_a1, s1) = match astype with
        | Neg_(_) | Tens_(_) -> do_parens last_a astype 
        | _ -> sub last_a astype in
      let a_arrow= get_free_node() in
      (a, 
       s1 ^ (tikz_of_node last_a1 a_arrow "\\Arrow") 
       ^ (tikz_of_node a_arrow a "\\Bottom") )
    (* Assumes that priority of Tens is the lowest. *)
    | Tens_(a, astype1, astype2) -> 
      let (last_a1, s1) = match astype1 with
        | Neg_(_) -> do_parens last_a astype1
        | _ -> sub last_a astype1 in
      let a_tens = get_free_node() in
      let (last_a2, s2) = match astype2 with
        | Neg_(_) -> do_parens a_tens astype2 
        | _ -> sub a_tens astype2 in
      (last_a2,
       s1 ^ (tikz_of_node last_a1 a_tens "\\Tens") ^ s2 )
  and do_parens last_a astype =
    let a1= get_free_node() in
    let (last_a1, s1) = sub a1 astype in
    let a2= get_free_node() in
    (a2, 
     (tikz_of_node last_a a1 "(" )
     ^ s1
     ^ (tikz_of_node last_a1 a2 ")" ) )
  in
  let a1 = get_free_node() in (* node with empty label used simply to anchor the rest *)
  let (last_a, s) = sub a1 astype0 in
  let a2 = get_free_node() in (* node for the phantom arrow between s and the dummy initial move*)
  let dummy_initial_node = dummy_initial_node_convention in
  ("\\node [] (" ^ (tnofa a1) ^ ") {};\n")
  ^ s
  ^ (tikz_of_node last_a a2 "\\phantom{\\Arrow}")
  ^ (tikz_of_node a2 dummy_initial_node "\\phantom{\\Bottom}")


(* Takes a 'a annoted_term, returns an (('a * int), ('a * int)) annoted_term, 
   where ('a * int) is the type necessary to determine a specific move in the
   current branch: 'a being a position in the type, and int being the move
   number, starting at 0 for the dummy initial move. An annotation (c_move,j_move)
   means that the connector should label the current move c_move, and that j_move
   justifies c_move.
*)
let decorate_aterm_with_pointers (aterm0 : 'a annoted_term) = 
  let dummy_move= (0, dummy_initial_node_convention) in
  (* uum is short for unused_move. *)
  let uum= (dummy_initial_node_convention, -1) in
  let uumm= (uum, uum) in
  let assoc_or_default x l =
    try List.assoc x l 
    with Not_found -> uum
  in
  (* moves_vp is short for moves_of_pvars (an assoc list) *)
  let rec sub_p (moves_vp, moves_vn, c_move, j_move) pa0 = 
    match pa0 with
      | Ep_(_) -> Ep_(uumm)
      (* Because of the way that List.assoc works, this replaces an existing assoc. *)
      | Lp_(_,x,pa) -> Lp_(uumm, x, sub_p ((x,c_move)::moves_vp, moves_vn, c_move, j_move) pa)
      | Vp_(a,x,na) -> 
        let x_j_move = assoc_or_default x moves_vp in
        let x_c_move = let (_,movei)=c_move in (a, movei+1) in
        Vp_((x_c_move, x_j_move), x, sub_n (moves_vp, moves_vn, x_c_move, x_j_move) na)
      | _ -> failwith "expected normal branch"
  and sub_n (moves_vp, moves_vn, c_move, j_move) na0 = 
    match na0 with
      | En_(_) -> En_(uumm)
      (* Because of the way that List.assoc works, this replaces an existing assoc. *)
      | Ln_(_,u,na) -> Ln_(uumm, u, sub_n (moves_vp, (u,c_move)::moves_vn, c_move, j_move) na)
      | Vn_(a,u,pa) -> 
        let u_j_move = assoc_or_default u moves_vn in
        let u_c_move = let (_,movei)=c_move in (a, movei+1) in
        Vn_((u_c_move, u_j_move), u, sub_p (moves_vp, moves_vn, u_c_move, u_j_move) pa)
      | _ -> failwith "expected normal branch"
  in
  match aterm0 with
    | Term_p_(pa) -> Term_p_ (sub_p ([], [], dummy_move, dummy_move) pa)
    | Term_n_(na) -> Term_n_ (sub_n ([], [], dummy_move, dummy_move) na)
      

(* Takes a decorated 'a branch. *)
let tikz_of_dabranch ?(draw_dummy=true) aterm0 =
  let dummy_move= (dummy_initial_node_convention, 0) in
  (* vertical space between two consecutive moves, in tikz unit. *)
  let tikz_move_step= 0.6 in 
  (* offset in tikz unit before start of current branch. *)
  let current_offset= ref 0. in  
  let nofm (pos, movei) =
    "node_" ^ (string_of_int pos) ^ "_" ^ (string_of_int movei) in
  let lnofm move =
    "l" ^ (nofm move) in
  let tnofa i =
    if i >= 0 then 
      "tnode_" ^ (string_of_int i)
    else
      "tn" ^ (string_of_int ( -i) )
  in
  let tikz_of_node c_move j_move latex_label = 
    let (c_pos,c_movei)= c_move in
    let (j_pos,j_movei)= j_move in
    let sj = if not draw_dummy && j_pos = dummy_initial_node_convention then
        ""
      else
        "\\draw[bend right=10] (" ^ (nofm j_move) ^ ") to (" ^ (nofm c_move) ^ ");\n"
    in
    let style = if not draw_dummy && c_pos = dummy_initial_node_convention then
        "hidden" else "move"
    in
    "\\node [" ^ style ^ ", below= "
    ^ (string_of_float( !current_offset +. (float_of_int (c_movei + 1)) *. tikz_move_step ))
    ^ " of " ^ (tnofa c_pos) ^ "]  "
    ^ "(" ^ (nofm c_move) ^ ")  {};\n"

    ^ sj

    ^ "\\node [term, above=0 of " ^ (nofm c_move) 
    ^ "] (" ^ (lnofm c_move) ^ ") {$" 
    ^ latex_label ^ "$};\n"
  in
  let rec sub_p acc_s c_move j_move pa0 = 
    let flush_acc_node s =
      tikz_of_node c_move j_move (acc_s ^ s)
    in
    match pa0 with
      | Ep_(_) -> flush_acc_node "" (* Change "" to "\\Ep" to make Ep explicit. *)
      | Lp_(_,Var_p(sx),pa) -> sub_p (acc_s ^ "\\Lp " ^ sx ^ ". ") c_move j_move pa
      | Vp_((x_c_move,x_j_move), Var_p(sx), na) -> 
        let underscore_or_not = match na with | En_(_) -> "" | _ -> "\\_" in
        ( flush_acc_node "" )
        ^ sub_n (sx ^ underscore_or_not)  x_c_move x_j_move na
      | _ -> failwith "expected normal branch"
  and sub_n acc_s c_move j_move na0 = 
    let flush_acc_node s =
      tikz_of_node c_move j_move (acc_s ^ s)
    in
    match na0 with
      | En_(_) -> flush_acc_node ""
      | Ln_(_,Var_n(su),na) -> sub_n (acc_s ^ "\\Ln " ^ su ^ ". ") c_move j_move na
      | Vn_((u_c_move,u_j_move), Var_n(su), pa) -> 
        let underscore_or_not = match pa with | Ep_(_) -> "" | _ -> "\\_" in
        ( flush_acc_node "" )
        ^ sub_p (su ^ underscore_or_not)  u_c_move u_j_move pa
      | _ -> failwith "expected normal branch"
  in
  match aterm0 with
    | Term_p_(pa) -> sub_p "" dummy_move dummy_move pa
    | Term_n_(na) -> sub_n "" dummy_move dummy_move na


let tikz_of_abranch ?(draw_dummy=true) t =
  tikz_of_dabranch ~draw_dummy ( decorate_aterm_with_pointers t)
    

let output_tikz_of_term ?(draw_dummy=true) ?(compile_latex=compile_latex_default) t = 
(*
  let astype = Neg_( 1, Tens_(0, (Bot_(2)),(Bot_(3)))) in
  let aterm = Term_n_(Ln_(0,Var_n("u"),Vn_(1, Var_n("u"), (Lp_(2, Var_p("x"), (Lp_(3, Var_p("y"), (Vp_(2, Var_p("x"), Vn_(1, Var_n("u"), (Lp_(2, Var_p("x"), (Lp_(3, Var_p("y"), (Vp_(2, Var_p("x"), En_(0))))))))))))))))) in
*)
  let astype = Neg_( 1, Tens_(0, Neg_(2, Bot_(4)),(Bot_(3)))) in
  let aterm = Term_n_(Ln_(0,Var_n("u_0"),Vn_(1, Var_n("u_0"), (Lp_(2, Var_p("x"), (Lp_(3, Var_p("c"), (Vp_(2, Var_p("x"), Ln_(4, Var_n("v"), Vn_(4, Var_n("v"), (Vp_(2, Var_p("x"), Ln_(4, Var_n("v'"), Vn_(4, Var_n("v'"), Vp_(3, Var_p("c"), En_(0))))))))))))))))) in


  let s = (tikz_of_astype astype) ^ (tikz_of_abranch ~draw_dummy aterm) in
  finalise_latex ~compile_latex (finalise_tikz_figure s)
    

let _ = ()

(*
let infere_type t =
  (* If type_variables contains (3, Bot), then 'a3 is Bot *)
  let type_variables = ref ( [] : (int * stype) list ) in
  
  let rec infere_sub = function 
    | Ep -> 
*)

