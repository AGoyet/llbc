let global_output_file_name="output.tex";;
let global_latex_string= ref "";; (* Used as output buffer when creating latex file. *)
let compile_latex_default= true;; (* Should the program try to launch latex on output.tex? *)
let steps_default=32;; 
let global_step= ref 0;; 
let xu_gindex= ref 0;; (* Global, used to create indexed fresh names. *)
let a_gindex= ref 0;; (* Global, used to create indexed fresh fixed-point names. *)

(* Used in parser.ml and main.ml *)
exception Empty;;

let (<<) f g = function x -> f (g x);;
let (>>) f g = function x -> g (f x);;
let id x= x;;



(*
p: positive (player starting), n: negative (opponent starting)

E: epsilon
L: lambda
V: var
P: plus
M: merge
C: connect
*)


type var_p = Var_p of string;;
type var_n = Var_n of string;;

type term_p = 
  | Ep
  | Lp of var_p * term_p
  | Vp of var_p * term_n
  | Pp of term_p * term_p
  | Mp of term_p * term_n
  | Cp of (var_p * var_n) list * var_n list  * term_p 
  | Nu of var_p * term_p

and  term_n = 
  | En
  | Ln of var_n * term_n
  | Vn of var_n * term_p
  | Pn of term_n * term_n
  | Mn of term_n * term_n
  | Bt of term_p
;;

type term = 
  | Term_p of term_p
  | Term_n of term_n
;;

let pfun_of_tfun tfun pa =
  match tfun (Term_p(pa)) with 
    | Term_p(pa2) -> pa2 
    | _ -> failwith "function did not preserve polarity"

let nfun_of_tfun tfun na =
  match tfun (Term_n(na)) with 
    | Term_n(na2) -> na2 
    | _ -> failwith "function did not preserve polarity"


let lp x pa= Lp(x,pa)
and vp x na= Vp(x,na)
and pp(pa,pb)= Pp(pa,pb)
and mp(pa,na)= Mp(pa,na)
and nu x pa= Nu(x,pa)
and cp links stack pa= Cp(links, stack, pa)
and ln u na= Ln(u,na)
and vn u na= Vn(u,na)
and pn(na,nb)= Pn(na,nb)
and mn(na,nb)= Mn(na,nb)
and bt(pa) = Bt(pa)
;;

(* "twc" stands for "term with context" *)
type 'a parametered_twc=
  | Hole_p of (term_p -> 'a) * term_p
  | Hole_n of (term_n -> 'a) * term_n
;;
type twc=
  | TWC_p of term_p parametered_twc
  | TWC_n of term_n parametered_twc
;;
let dummy_twc= TWC_p(Hole_p(id,Ep));;

(* Used when printing context. *)
let hole_placeholder_p= Vp(Var_p("[]"),En)
and hole_placeholder_n= Vn(Var_n("[]"),Ep)
;;


let aterm_of_atwc = function 
  | Hole_p(context,term_p) -> context(term_p)
  | Hole_n(context,term_n) -> context(term_n)
;;

let term_of_twc = function
  | TWC_p(atwc) -> Term_p(aterm_of_atwc atwc)
  | TWC_n(atwc) -> Term_n(aterm_of_atwc atwc)
;;

let member_term_of_twc = function
  | TWC_p(Hole_p(ctxt,p)) -> Term_p(p)
  | TWC_p(Hole_n(ctxt,n)) -> Term_n(n)
  | TWC_n(Hole_p(ctxt,p)) -> Term_p(p)
  | TWC_n(Hole_n(ctxt,n)) -> Term_n(n)
;;

let member_context_of_twc = function
  | TWC_p(Hole_p(ctxt,p)) -> 
    (fun t -> match t with 
      | Term_p(p) -> Term_p(ctxt(p))
      | Term_n(n) -> assert false
    )
  | TWC_p(Hole_n(ctxt,n)) -> 
    (fun t -> match t with 
      | Term_p(p) -> assert false
      | Term_n(n) -> Term_p(ctxt(n))
    )
  | TWC_n(Hole_p(ctxt,p)) -> 
    (fun t -> match t with 
      | Term_p(p) -> Term_n(ctxt(p))
      | Term_n(n) -> assert false
    )
  | TWC_n(Hole_n(ctxt,n)) -> 
    (fun t -> match t with 
      | Term_p(p) -> assert false
      | Term_n(n) -> Term_n(ctxt(n))
    )
;;



let term_to_term_fun (p_fun, n_fun)= function
  | Term_p(pa) -> Term_p(p_fun(pa))
  | Term_n(na) -> Term_n(n_fun(na))
;;

let term_to_afun (p_fun, n_fun)= function
  | Term_p(pa) -> p_fun(pa)
  | Term_n(na) -> n_fun(na)
;;


(* printing *)
let rec string_of_term_p = function
  | Ep -> "ε"
  | Lp(Var_p(s), pa) -> "λ" ^ s ^ "." ^ (string_of_term_p pa)
  | pa when pa = hole_placeholder_p -> "[]p" (* case used to print contexts *)
  | Vp(Var_p(s), na) -> s ^ "_" ^ (string_of_term_n na)
  | Pp(pa, pb) -> "(" ^ (string_of_term_p pa) ^ "+" ^ (string_of_term_p pb) ^ ")"
  | Mp(pa, na) -> "(" ^ (string_of_term_p pa) ^ " < " ^ (string_of_term_n na) ^ ")"
  | Cp(links, stack, pa) ->
    let rec string_of_links = function
      | [] -> ""
      | [ (Var_p(sx), Var_n(su)) ] -> "(" ^ sx ^ "," ^ su ^ ")" 
      | (Var_p(sx), Var_n(su))::rest -> 
        "(" ^ sx ^ "," ^ su ^ ")" 
        ^ (string_of_links rest)
    and string_of_stack = function
      | [] -> ""
      | [ Var_n(su) ] -> su
      | Var_n(su)::rest  -> su ^ ";" ^ (string_of_stack rest)
    in if (stack = []) then
        "V" ^ (string_of_links links) ^ "." ^ (string_of_term_p pa) 
      else
        "V" ^ (string_of_links links) ^ " [" ^ (string_of_stack stack) ^ "]." 
        ^ (string_of_term_p pa) 
  | Nu(Var_p(s), pa) -> "ν" ^ s ^ "." ^ (string_of_term_p pa) 

and string_of_term_n = function
  | En -> "ὲ" (* "ε̄" *)
  | Ln(Var_n(s), na) -> "ƛ" ^ s ^ "." ^ (string_of_term_n na) (* "λ̄" *)
  | na when na = hole_placeholder_n -> "[]n" (* case used to print contexts *)
  | Vn(Var_n(s), pa) -> s ^ "_" ^ (string_of_term_p pa)
  | Pn(na, nb) -> "(" ^ (string_of_term_n na) ^ "+" ^ (string_of_term_n nb) ^ ")"
  | Mn(na, nb) -> "(" ^ (string_of_term_n na) ^ " < " ^ (string_of_term_n nb) ^ ")"
  | Bt(pa) -> "Bt(" ^ (string_of_term_p pa) ^ ")"
;;
let rec string_of_term= 
  term_to_afun (string_of_term_p, string_of_term_n)
;;

let rec list_of_comb_p= function
  | Pp(pa,pb) -> pa::(list_of_comb_p pb)
  | pa -> [pa]
;;
let rec list_of_comb_n= function
  | Pn(na,nb) -> na::(list_of_comb_n nb)
  | na -> [na]
;;


let rec latex_of_links = function
  | [] -> ""
  | [ (Var_p(sx), Var_n(su)) ] -> "(" ^ sx ^ "," ^ su ^ ")" 
  | (Var_p(sx), Var_n(su))::rest -> 
    "(" ^ sx ^ "," ^ su ^ ")" 
    ^ (latex_of_links rest)
;;
let rec latex_of_stack = function
  | [] -> ""
  | [ Var_n(su) ] -> su
  | Var_n(su)::rest  -> su ^ " \\cdot " ^ (latex_of_stack rest)
;;

let latex_array (operator:string) func line_list= 
  match line_list with 
    | [] -> ""
    | line::rest -> 
      "\\hspace{0pt}"
      ^ "\\begin{array}{l} \n" 
      ^ "( \\phantom{" ^ operator ^ "} ~ "
      ^ (func line) 
      ^ (List.fold_left
           (fun s ta -> 
             s ^ " \\\\ \n \\phantom{ ( } {" ^ operator ^ "} ~"
             ^ (func ta) )
           "" rest)
      ^ " ) \n \\end{array} \n"
;;

(* Here lvl is the maximum number of indentation levels which should be 
   displayed. Indentation level is increased for P and Mp nodes. *)
let rec latex_of_term_p ?(pretty=false) ?(lvl=0) = function
  | Ep -> "\\Ep"
  | Lp(Var_p(s), pa) -> "\\Lp " ^ s ^ ". " ^ (latex_of_term_p ~pretty ~lvl pa)
  | pa when pa = hole_placeholder_p -> "[]_p" (* case used to print contexts *)
  | Vp(Var_p(s), na) -> s ^ "\\_ " ^ (latex_of_term_n ~pretty ~lvl na)
  | Pp(pa, pb) ->
    if (pretty && (lvl >= 1))  then 
      latex_array "\\Pp"  (latex_of_term_p ~pretty ~lvl:(lvl-1)) (list_of_comb_p (Pp(pa,pb)))
    else
      "(" ^ (latex_of_term_p ~pretty ~lvl pa) ^ " \\Pp " 
      ^ (latex_of_term_p ~pretty ~lvl pb) ^ ")"
  | Mp(pa, na) -> 
    if (pretty && (lvl >= 1))  then 
      latex_array "\\Mp"  
        (latex_of_term ~pretty ~lvl:(lvl-1)) 
        ([Term_p(pa); Term_n(na)])
    else
      "(" ^ (latex_of_term_p ~pretty ~lvl pa) ^ " \\Mp " ^ (latex_of_term_n ~pretty ~lvl na) ^ ")"
  | Cp(links, stack, pa) ->
    "\\Cp{" ^ (latex_of_links links) ^ "}{" ^ (latex_of_stack stack) ^ "}. " 
    ^ (latex_of_term_p ~pretty ~lvl pa) 
  | Nu(Var_p(s), pa) -> "\\Nu " ^ s ^ ". " ^ (latex_of_term_p ~pretty ~lvl pa)

and latex_of_term_n ?(pretty=false) ?(lvl=0) = function
  | En -> "\\En"
  | Ln(Var_n(s), na) -> "\\Ln " ^ s ^ ". " ^ (latex_of_term_n ~pretty ~lvl na)
  | na when na = hole_placeholder_n -> "[]_n" (* case used to print contexts *)
  | Vn(Var_n(s), pa) -> s ^ "\\_ " ^ (latex_of_term_p ~pretty ~lvl pa)
  | Pn(na, nb) -> 
    if (pretty && (lvl >= 1))  then 
      latex_array "\\Pn" (latex_of_term_n ~pretty ~lvl:(lvl-1)) (list_of_comb_n (Pn(na, nb)))
    else
      "(" ^ (latex_of_term_n ~pretty ~lvl na) ^ " \\Pn " 
      ^ (latex_of_term_n ~pretty ~lvl nb) ^ ")"
  | Mn(na, nb) -> 
    if (pretty && (lvl >= 1))  then 
      latex_array "\\Mn"  
        (latex_of_term ~pretty ~lvl:(lvl-1)) 
        ([Term_n(na); Term_n(nb)])
    else
      "(" ^ (latex_of_term_n ~pretty ~lvl na) ^ " \\Mn " ^ (latex_of_term_n ~pretty ~lvl nb) ^ ")"
  | Bt(pa) -> "\\Bt (" ^ (latex_of_term_p ~pretty ~lvl pa) ^ ")"

and latex_of_term ?(pretty=false) ?(lvl=0) = function
  | Term_p(pa) -> latex_of_term_p ~pretty ~lvl pa
  | Term_n(na) -> latex_of_term_n ~pretty ~lvl na
;;


let compile_latex_now ?(output=global_output_file_name) ?(compile_latex=compile_latex_default) ()=
  if compile_latex then
    if Sys.os_type = "Unix" then
      let _ = Unix.system ("latex " ^ output ^ " > /dev/null") in ()
;;

let finalise_latex ?(output=global_output_file_name) ?(compile_latex=compile_latex_default) s =
  let chan= open_out output in
  output_string chan
    (
"\\pdfoutput=1
\\topmargin=-70pt
\\documentclass{minimal}
\\usepackage[margin=0cm,a4paper]{geometry}
\\usepackage{amssymb}
\\usepackage{style_llbc}

\\usepackage{tikz}
\\usetikzlibrary{arrows}
\\usetikzlibrary{positioning}

\\begin{document}"
^
s
^
"\\end{document}
"
    );
  close_out chan;
  compile_latex_now ~compile_latex ();
;;

let latex_mathmode s =
"  \\[
"
  ^ s
  ^ 
"\\]
"

let finalise_latex_mathmode ?(compile_latex=compile_latex_default) s =
  finalise_latex ~compile_latex (latex_mathmode s)

(* testing function *)
let quick_latex ?(lvl=0) ta=
  finalise_latex_mathmode (latex_of_term ~pretty:true ~lvl ta);
;;


let string_of_context = function 
  | TWC_p(Hole_p(context, _)) ->
    string_of_term_p( context( hole_placeholder_p))
  | TWC_p(Hole_n(context, _)) -> 
    string_of_term_p( context( hole_placeholder_n))      
  | TWC_n(Hole_p(context, _)) -> 
    string_of_term_n( context( hole_placeholder_p))
  | TWC_n(Hole_n(context, _)) -> 
    string_of_term_n( context( hole_placeholder_n))
;;



let print_context twc = 
  print_string(string_of_context(twc));
  print_newline()
;;

let print_term ta = 
  print_string(string_of_term ta);
  print_newline()
;;


exception NA ;; (* means "does not apply"; used when tryping to apply rules *)


(* The result is a stack and a term_n of the form nosum_guarded = Vn or En *)
let local_normal_form_n na0= 
  let rec sub stack na0 = match na0 with
    | Vn(u,pa) -> stack, Vn(u,pa)
    | En -> stack, En
    | Ln(u, na) -> sub (stack@[u]) na  (* its @ and not :: because it's a stack *)
    | _ -> raise NA (* "is not in local normal form" *)
  in
  sub [] na0
;;

let rec context_of_stack_n = function
  | [] -> id
  | u::stack -> (ln u)<<(context_of_stack_n stack)
;;

type rules= (term_p -> term_p) * (term_n -> term_n) ;;  

let no_rules_p = function 
  | _ -> raise NA
and no_rules_n = function 
  | _ -> raise NA
;;
let no_rules : rules = (no_rules_p, no_rules_n)
;;


(* We now define a generic "walk" function on terms. It is used in conjuction 
   with a function of type "case_fun", to perform some transformation on a term. 
   Intuitively, the case_fun itself handles some cases of the pattern matching.
   The cases which are not handled raise the "Partial" exception.
   The "walk" function recursively calls the case_fun while walking through the term.
   In the walk function, the case_fun f is given (walk f) as its argument of type "rules". 
   This argument is generally called "keep_walking".
   It is used by f to ask for the walk to continue on subterms.
*)

exception Partial;;

type case_fun_p = rules -> term_p -> term_p ;;
type case_fun_n = rules -> term_n -> term_n ;;
type case_fun= case_fun_p * case_fun_n;;


let rec walk_p (case_fun:case_fun) pa0 = 
  let (case_fun_p, case_fun_n)= case_fun in
  let keep_walking = (walk_p case_fun, walk_n case_fun) in
  try case_fun_p keep_walking pa0
  with Partial ->  match pa0 with
    | Ep -> Ep
    | Lp(x,pa) -> Lp(x, (walk_p case_fun pa))
    | Vp(x,na) -> Vp(x, (walk_n case_fun na))
    | Pp(pa,pb) -> Pp((walk_p case_fun pa), (walk_p case_fun pb))
    | Mp(pa,na) -> Mp((walk_p case_fun pa), (walk_n case_fun na))
    | Cp(links, stack, pa) -> Cp(links, stack, (walk_p case_fun pa))
    | Nu(x,pa) -> Nu(x, (walk_p case_fun pa))
and  walk_n (case_fun:case_fun) na0 = 
  let (case_fun_p, case_fun_n)= case_fun in
  let keep_walking = (walk_p case_fun, walk_n case_fun) in
  try case_fun_n keep_walking na0
  with Partial ->  match na0 with
    | En -> En
    | Vn(u,pa) -> Vn(u, (walk_p case_fun pa))
    | Ln(u,na) -> Ln(u, (walk_n case_fun na))
    | Pn(na,nb) -> Pn((walk_n case_fun na), (walk_n case_fun nb))
    | Mn(na,nb) -> Mn((walk_n case_fun na), (walk_n case_fun nb))
    | Bt(pa) -> Bt((walk_p case_fun pa))
;;
let walk (case_fun:case_fun) = 
  term_to_term_fun (walk_p case_fun, walk_n case_fun)
;;


let btua_p u a (keep_walking : rules) = function
  | _ -> raise Partial
and btua_n u a (keep_walking : rules) = function
  | En -> Vn(u,Vp(a,En))
  | Vn(v,pa) -> Mn(Vn(u,Vp(a,En)),Vn(v, (fst keep_walking) pa))
  | _ -> raise Partial
;;
let btua_case_fun u a = (btua_p u a, btua_n u a)
;;


let replacement_alpha_p x0 pa0 keep_walking = function
  | Nu(x,pa) -> 
    if x=x0 then 
        Nu(x,pa)
    else Nu(x, (fst keep_walking) pa)
  | Vp(x,na) -> 
    if x=x0 then
        Mp(Nu(x0,pa0), ((snd keep_walking) na))
    else Vp(x, (snd keep_walking) na)
  | _ ->  raise Partial
and replacement_alpha_n x0 pa0 keep_walking = function
  | (_ : term_n) -> (raise Partial : term_n)
;;
let replacement_alpha_case_fun x0 pa0  =
  (replacement_alpha_p x0 pa0, replacement_alpha_n x0 pa0)
;;


let initialize_name_index_sub index s=
  (* if s is "c23", where c is any char, set global index to at least 24 *)
  try index:= max 
        !index 
        (int_of_string(String.sub s 1 (String.length(s) -1)) + 1)
  with Failure "int_of_string" -> ()
;;

let initialize_xu_index_p keep_walking = function
  | Vp(Var_p(s),na) -> 
    if  (String.sub s 0 1) = "x" then
      initialize_name_index_sub xu_gindex s;
    Vp(Var_p(s), (snd keep_walking) na)
  | _ -> raise Partial
and initialize_xu_index_n keep_walking = function
  | Vn(Var_n(s),pa) -> 
    if  (String.sub s 0 1) = "u" then
      initialize_name_index_sub xu_gindex s;
    Vn(Var_n(s), (fst keep_walking) pa)
  | _ -> raise Partial
;;


let initialize_xu_index_case_fun : case_fun = 
  (initialize_xu_index_p, initialize_xu_index_n)
;;

let initialize_xu_index t =
  let _ = walk initialize_xu_index_case_fun t in
  ()
;;

let get_fresh_name index =
  index:= !index +1;
  !index -1
;;

let alpha_renaming_xu_casefun_p s1 s2 keep_walking = function
  | Lp(x,pa) -> 
    if x = Var_p(s1) then Lp(x,pa) 
    else Lp(x, (fst keep_walking) pa)
  | Nu(x,pa) -> 
    if x = Var_p(s1) then Nu(x,pa) 
    else Nu(x, (fst keep_walking) pa)
  | Vp(x, na) -> 
    if x = Var_p(s1) then Vp(Var_p(s2), (snd keep_walking) na)
    else Vp(x, (snd keep_walking) na)
  | _ -> raise Partial
and alpha_renaming_xu_casefun_n s1 s2 keep_walking = function
  | Ln(u,na) -> 
    if u = Var_n(s1) then Ln(u,na) 
    else Ln(u, (snd keep_walking) na)
  | Vn(u, pa) -> 
    if u = Var_n(s1) then Vn(Var_n(s2), (fst keep_walking) pa)
    else Vn(u, (fst keep_walking) pa)
  | _ -> raise Partial

let alpha_renaming_xu_casefun s1 s2 = 
  (alpha_renaming_xu_casefun_p s1 s2, alpha_renaming_xu_casefun_n s1 s2)

let alpha_renaming_xu s1 s2 ta =
  if s1=s2 
  then ta 
  else walk (alpha_renaming_xu_casefun s1 s2) ta

let alpha_renaming_xu_p s1 s2 =
  pfun_of_tfun (alpha_renaming_xu s1 s2)


let linked_p x links= List.exists (fun (y,v) -> y=x) links
and linked_n u links= List.exists (fun (y,v) -> v=u) links

exception Stop of bool

let free_in_casefun_p s keep_walking = function
  | Lp(x,pa) -> 
    if x = Var_p(s) then raise (Stop false)
    else (fst keep_walking) pa
  | Vp(x, na) -> 
    if x = Var_p(s) then raise (Stop true)
    else  Vp(x,(snd keep_walking) na)
  | _ -> raise Partial
and free_in_casefun_n s keep_walking = function
  | Ln(u,na) -> 
    if u = Var_n(s) then raise (Stop false)
    else (snd keep_walking) na
  | Vn(u, pa) -> 
    if u = Var_n(s) then raise (Stop true)
    else  Vn(u,(fst keep_walking) pa)
  | _ -> raise Partial

let free_in s ta = 
  try let _ = walk (free_in_casefun_p s, free_in_casefun_n s) ta in
      false
  with Stop(b) -> b



(* Actual reduction rules (the others being equivalences) *)
let reduction_p= function
    | Mp(Lp(x,pa),Vn(u,pb)) ->
      let Var_p(sx)= x in
      if not (free_in sx (Term_p(pb))) then
        Lp(x,Mp(pa,Vn(u,pb)))
      else 
        let sx2 = "x" ^ string_of_int (get_fresh_name xu_gindex) in
        let x2 = Var_p(sx2) in
        let pb2 = alpha_renaming_xu_p sx sx2 pb in
        Lp(x2,Mp(pa,Vn(u,pb2)))
    | Mp(Vp(x,na),Vn(u,pa)) -> Vp(x,Mn(na,Vn(u,pa)))
    | Mp(pa,na) -> 
      let (stack, na2) = local_normal_form_n na in (* may raise NA *)
      if (stack == []) then
        raise NA
      else
        Cp([], stack, Mp(pa,na2))
    | Cp(links, u::stack, Lp(x,pa)) -> 
      (* The fun thing about this whole case is that NOT managing the alpha renaming yields the same result...*)
      let Var_p(sx),Var_n(su) = x,u in
      (* This is an attempt to generate only one new fresh index,
         to rename e.g. xfoo,ufoo to x3,u3 instead of x3,u4 *)
      let xindex = ref None in 
      let sx2 = if linked_p x links then
          let xi= string_of_int (get_fresh_name xu_gindex) in
          xindex:= Some(xi);
          "x" ^ xi
        else sx in
      let su2 = if linked_n u links then 
          let ui = match !xindex with Some(xi) -> xi | None -> string_of_int(get_fresh_name xu_gindex) in
          "u" ^ ui
        else su in
      let pa2= alpha_renaming_xu_p sx sx2 (alpha_renaming_xu_p su su2 pa) in
      Cp((Var_p(sx2),Var_n(su2))::links, stack, pa2)

    | Cp(links, [], Lp(x,pa)) -> 
      if linked_p x links then 
        let Var_p(sx)=x in
        let sx2= "x" ^ string_of_int (get_fresh_name xu_gindex) in
        let x2= Var_p(sx2) in
        Lp(x2, Cp(links, [], alpha_renaming_xu_p sx sx2 pa))
      else
        Lp(x, Cp(links, [], pa))
          
    | Cp(links, stack, Vp(x,na)) -> 
      let (stack_na2, na2) = local_normal_form_n na in (* may raise NA *)
      if (linked_p x links) then
        begin match na2 with
          | Vn(u,pa3) -> 
            if (List.mem (x,u) links) then 
              Cp(links, stack_na2@stack, pa3)
            else 
              Ep
          | En -> Ep 
          | _  -> failwith("expected form nosum_guarded")
        end

      else (* x notin links *)
        Vp(x, 
           ((context_of_stack_n stack_na2)<<(context_of_stack_n stack)) 
             (match na2 with 
               | Vn(u,pa3) -> 
                 if (linked_n u links) then 
                   En 
                 else 
                   Vn(u, Cp(links, [], pa3))
               | En -> En
               | _  -> failwith("expected form nosum_guarded")
             )
        )
    | Nu(x, pa) -> 
      walk_p (replacement_alpha_case_fun x pa) pa

    | _ -> raise NA

and reduction_n= function 
  | Mn(Ln(u,na),nb) -> Ln(u,Mn(na,nb))
  | Mn(Vn(u,pa),Ln(v,nb)) -> Ln(v,Mn(Vn(u,pa),nb))
  | Mn(En,Ln(v,nb)) -> Ln(v,Mn(En,nb))  (* Only necessary when not well typed. *)
  | Mn(Vn(u,pa),Vn(v,pb)) -> Pn(Vn(u,Mp(pa,Vn(v,pb))), Vn(v,Mp(pb,Vn(u,pa))))
  | Bt(pa) -> 
    let a_i= get_fresh_name xu_gindex in
    let xu_i= get_fresh_name xu_gindex in
    let u= Var_n( "u" ^ (string_of_int xu_i))
    and a= Var_p("A" ^ (string_of_int a_i)) in
    Ln(u, Vn(u, Nu(a, walk_p (btua_case_fun u a) pa)))
  | _ -> raise NA
;;
let reduction : rules =(reduction_p, reduction_n)
;;



(* 
neut: neutrality
*)
let neutrality_p = function
  | Pp(Ep,pa) -> pa
  | Pp(pa,Ep) -> pa
  | Mp(Ep,na) -> Ep
  | Mp(pa,En) -> pa
  | Cp(_,_,Ep)-> Ep
  | _ -> raise NA
and neutrality_n = function
  (* these can be added if all terms are well-typed *)
  (*
  | Pn(En,na) -> na
  | Pn(na,En) -> na
  | Mn(En,na) -> na  
  | Mn(na,En) -> na  
  *)

  (* these weaker cases are correct for untypped terms *)
  | Pn(En, Vn(u,pa)) -> Vn(u,pa)
  | Pn(Vn(u,pa), En) -> Vn(u,pa)
  | Pn(En,En) -> En
  | Mn(En,Vn(u,pa))  -> Vn(u,pa)
  | Mn(Vn(u,pa), En)  -> Vn(u,pa)
  | Mn(En,En) -> En
  | _ -> raise NA
;;
let neutrality : rules = (neutrality_p, neutrality_n)
;;


(* Toward "comb" form, ie nosum | P(nosum, comb) *)
let distributivity_p = function
  | Lp(x,Pp(pa,pb)) -> Pp(Lp(x,pa),Lp(x,pb))
  | Vp(x,Pn(na,nb)) -> Pp(Vp(x,na),Vp(x,nb))
  | Mp(Pp(pa,pb),na) -> Pp(Mp(pa,na),Mp(pb,na))
  | Mp(pa,Pn(na,nb)) -> Pp(Mp(pa,na),Mp(pa,nb))
  | Cp(l,s,Pp(pa,pb)) -> Pp(Cp(l,s,pa),Cp(l,s,pb))
  | _ -> raise NA
and distributivity_n = function
  | Ln(u,Pn(na,nb)) -> Pn(Ln(u,na),Ln(u,nb))
  | Vn(u,Pp(pa,pb)) -> Pn(Vn(u,pa),Vn(u,pb))
  | Mn(Pn(na,nb),nc) -> Pn(Mn(na,nc),Mn(nb,nc))
  | Mn(na,Pn(nb,nc)) -> Pn(Mn(na,nb),Mn(na,nc))
  | _ -> raise NA
;;
let distributivity : rules = (distributivity_p, distributivity_n)
;;

(* Toward cannonical form of the comb. *)
let associativity_p = function
  | Pp(Pp(pa,pb),pc) -> Pp(pa,Pp(pb,pc))
  | _ -> raise NA
and associativity_n= function
  | Pn(Pn(na,nb),nc) -> Pn(na,Pn(nb,nc))
  | _ -> raise NA
;;
let associativity : rules = (associativity_p, associativity_n)
;;

(* *)
let convinience_p = function
  | Cp([],[],pa) -> pa
  | _ -> raise NA
and convinience_n= function
  | _ -> raise NA
;;
let convinience : rules = (convinience_p, convinience_n)
;;


let f = function 
  | [] -> 0
  | ( x : int ) :: r -> 1


(* A nafun is a partial function which raises NA on the undefined cases. *)
let rec nafun_of_nafunlist l x = match l with
  | [] -> raise NA
  | f::rest -> try f x with NA -> nafun_of_nafunlist rest x
;;
let rec listpair_of_pairlist = function
  | [] -> ([],[])
  | (a,b)::rest -> 
    let (a2,b2)= listpair_of_pairlist rest in
    (a::a2, b::b2)
;;
let rules_of_ruleslist (l : rules list) : rules= 
  let (lp,ln)= listpair_of_pairlist l in
  (nafun_of_nafunlist lp, nafun_of_nafunlist ln)
;;

  

let equivalences= 
  rules_of_ruleslist [neutrality; distributivity; associativity; convinience]
;;

let neut_assoc=
  rules_of_ruleslist [neutrality; associativity]

let neut_assoc_conv=
  rules_of_ruleslist [neutrality; associativity; convinience]
;;
(* E L V P M C *)
let append_context context atwc=
  match atwc with
    | Hole_p(cpa,pa) -> Hole_p((context<<cpa), pa)
    | Hole_n(cna,na) -> Hole_n((context<<cna), na)
;;



(*
let rec find_rule rule twc=
  begin match twc with
    | Hole_p(context, pa) -> 
      append_context context (find_rule_p rule pa)
    | Hole_n(context, na) -> 
      append_context context (find_rule_n rule na)
  end

*)

(* lvl is the max level at which to look for an applicable rule. 
   A level is the number of V encountered from the root. 
   -1 is a special value, which stands for infinity.  *)

(* find_rule_p returns a term_p term_with_context_t *)
let rec find_rule_p ?(lvl= -1) (rules : rules) pa0=
  begin try let _ = (fst rules) pa0 in 
            (* The definition of levels would allow to do 
               V(P(_))->P(V(_)) at level 0; but this is unwanted (the P itself 
               is at level 1 here, and changes level). We therefore make an
               exception for this case, asking that lvl be non 0.
            *)
            begin match pa0 with
              | Vp(_,Pn(_)) -> if lvl == 0 then raise NA
              | _ -> ()
            end;
            Hole_p(id, pa0)
      with NA -> match pa0 with
        | Ep -> raise NA
        | Lp(x,pa) -> 
          append_context (lp x) (find_rule_p ~lvl rules pa)
        | Vp(x,na) -> 
          if lvl == 0 then 
            raise NA
          else
            append_context (vp x) (find_rule_n ~lvl:(max (-1) (lvl - 1)) rules na)
        | Pp(pa,pb) -> 
          begin try append_context 
                      (fun pa2 -> Pp(pa2,pb)) (find_rule_p ~lvl rules pa)
            with NA -> 
              append_context 
                (fun pb2 -> Pp(pa,pb2)) (find_rule_p ~lvl rules pb)
          end
        | Mp(pa,na) -> 
          begin try append_context 
                      (fun pa2 -> Mp(pa2,na)) (find_rule_p ~lvl rules pa)
            with NA -> 
              append_context 
                (fun na2 -> Mp(pa,na2)) (find_rule_n ~lvl rules na)
          end
        | Cp(links, stack, pa) ->
          append_context (cp links stack) (find_rule_p ~lvl rules pa)
        | Nu(x, pa) ->
          append_context (nu x) (find_rule_p ~lvl rules pa)
    end
and find_rule_n ?(lvl= -1) rules na0 =
    begin try let _ = (snd rules) na0 in 
            (* The definition of levels would allow to do 
               V(P(_))->P(V(_)) at level 0; but this is unwanted (the P itself 
               is at level 1 here, and changes level). We therefore make an
               exception for this case, asking that lvl be non 0.
            *)
              begin match na0 with
                | Vn(_,Pp(_)) -> if lvl == 0 then raise NA
                | _ -> ()
              end;
              Hole_n(id, na0)
      with NA -> match na0 with
        | En -> raise NA
        | Ln(u,na) -> 
          append_context (ln u) (find_rule_n ~lvl rules na)
        | Vn(u,pa) -> 
          if lvl == 0 then 
            raise NA
          else
            append_context (vn u) (find_rule_p  ~lvl:(max (-1) (lvl - 1)) rules pa)
        | Pn(na,nb) -> 
          begin try append_context 
                      (fun na2 -> Pn(na2,nb)) (find_rule_n ~lvl rules na)
            with NA -> 
              append_context 
                (fun nb2 -> Pn(na,nb2)) (find_rule_n ~lvl rules nb)
          end
        | Mn(na,nb) -> 
          begin try append_context 
                      (fun na2 -> Mn(na2,nb)) (find_rule_n ~lvl rules na)
            with NA -> 
              append_context 
                (fun nb2 -> Mn(na,nb2)) (find_rule_n ~lvl rules nb)
          end
        | Bt(pa) -> append_context bt (find_rule_p ~lvl rules pa)
    end
;;
let find_rule ?(lvl= -1) rules ta = 
  match ta with
  | Term_p(pa) -> TWC_p(find_rule_p ~lvl rules pa)
  | Term_n(na) -> TWC_n(find_rule_n ~lvl rules na)
;;


let apply_rule_atwc (rules : rules) atwc = 
  match atwc with
    | Hole_p(context, pa) -> Hole_p(context, (fst rules)(pa))
    | Hole_n(context, na) -> Hole_n(context, (snd rules)(na))
;;
let apply_rule_twc (rules : rules) twc =
  match twc with
    | TWC_p(atwc) -> TWC_p(apply_rule_atwc rules atwc)
    | TWC_n(atwc) -> TWC_n(apply_rule_atwc rules atwc)
;;

(*
let find_and_apply_rule_p rules pa=
  aterm_of_atwc (apply_rule_atwc rules (find_rule_p rules pa))
and find_and_apply_rule_n rules na=
  aterm_of_atwc (apply_rule_atwc rules (find_rule_n rules na))
;;
*)
let find_and_apply_rule ?(lvl= -1) rules ta=
  term_of_twc (apply_rule_twc rules (find_rule ~lvl rules ta))
;;
  
(*
let rec max_apply_rules_p rules pa0 = 
  try let atwc= find_rule_p rules pa0 in
      max_apply_rules_p rules (aterm_of_atwc (apply_rule_atwc rules atwc))
  with NA -> pa0

and     max_apply_rules_n rules na0 = 
  try let atwc= find_rule_n rules na0 in
      max_apply_rules_n rules (aterm_of_atwc (apply_rule_atwc rules atwc))
  with NA -> na0
;;
*)
let rec max_apply_rules ?(lvl= -1) rules ta0 = 
  try let twc= find_rule ~lvl rules ta0 in
      max_apply_rules ~lvl rules (term_of_twc (apply_rule_twc rules twc))
  with NA -> ta0
;;



let print_steps_start ?(latex=false) ()=
  if latex then
    global_latex_string:= "\\begin{array}{c@{\\hspace{1em}}l}\n"
;;

let print_steps_stop ?(latex=false) ?(compile_latex=compile_latex_default) ()=
  if latex then begin
    global_latex_string:= !global_latex_string ^ "\\end{array}\n";
    finalise_latex_mathmode ~compile_latex !global_latex_string;
  end
  (*
  else
     print_string "no more rules to apply\n"
  *)
;;


let print_step ?(latex=false) (s, latex_s) ?(print_lvl=0) ta=
  if latex then
    global_latex_string:= !global_latex_string ^
      latex_s ^ " & " ^ latex_of_term ~pretty:true ~lvl:print_lvl ta ^ " \\phantom{\\Cp{()}{}} \\\\ \n"
  else begin
    print_string (s ^ string_of_term ta);
    print_newline();
  end
;;
    
    
    
let strategy1_sub ?(latex=false) ?(lvl= -1) ?(print_lvl=0) ta= 
  let taref= ref ta and keep_going= ref true in
  print_step ~latex ("   ", "") !taref;
  while !keep_going do
    begin try let _ = find_rule ~lvl equivalences !taref in (* can raise NA *) 
              taref:= max_apply_rules ~lvl equivalences !taref;  (* cannot *)
              print_step ~latex ~print_lvl (" = ", "\\equiv") !taref;
      with NA -> () ; (* taref unchanged, do not print anything *)
    end;
    try 
      taref:= find_and_apply_rule ~lvl reduction !taref;
      print_step ~latex ~print_lvl ("-> ", "\\rightarrow") !taref;
    with NA ->
      keep_going:= false;
  done;
;;

let strategy1 ?(latex=false) ?(lvl= -1) ?(print_lvl=0) ta= 
  print_steps_start ~latex ();
  strategy1_sub ~latex ~print_lvl ~lvl ta;
  print_steps_stop ~latex ();
;;



(* This is essentially strategy1 ~lvl, but returns the term, and may raise NA.
   lvl here is the meaning of "local".
   Reduction rules are applied in order, starting with those appliable when 
   looking at the lowest level. *)
let local_normal_reduce ?(latex=false) ?(print_lvl=0) ?(lvl=0) ?(ctxt=id) ?(steps=steps_default) ta =
  let taref= ref ta and keep_going= ref true and used_reduction= ref false
  and level=ref 0 in
  while !level <= lvl && not !used_reduction do
    while !keep_going do
      begin try let _ = find_rule ~lvl:!level equivalences !taref in (* can raise NA *) 
                taref:= max_apply_rules ~lvl:!level equivalences !taref;  (* cannot *)
                print_step ~latex ~print_lvl (" = ", "\\equiv") (ctxt !taref);
        with NA -> () ; (* taref unchanged, do not print anything *)
      end;
    (* may raise NA *)
      begin 
        try 
          taref:= find_and_apply_rule ~lvl:!level reduction !taref;
          print_step ~latex ~print_lvl ("-> ", "\\rightarrow") (ctxt !taref);
          global_step:= !global_step +1;
          used_reduction:= true;
        with NA ->
          keep_going:= false;
      end;
      if !global_step >= steps then
        raise NA (* *)
    done;
    level:= !level + 1;
    keep_going:= true;
  done;
  !taref
;;
  


(* Used with find_rule to locate "active" nodes, ie. M,C,Nu or Bt *)
let dummy_active_p = function
  | Mp(pa,na) -> Mp(pa,na)
  | Cp(links, stack, pa) -> Cp(links, stack, pa)
  | Nu(x,pa) -> Nu(x,pa)
  | _ -> raise NA
and dummy_active_n = function
  | Mn(na,nb) -> Mn(na,nb)
  | Bt(pa) -> Bt(pa)
  | _ -> raise NA
;;
let dummy_active:rules = (dummy_active_p,dummy_active_n)
;;


let find_min_level ?(start_lvl=0) rules ta=
  (* The first find_rules is simply to potentially raise NA.*)
  let _ = find_rule rules ta in 
  let level=ref start_lvl and keep_going= ref true 
  and twc= ref dummy_twc in
  while !keep_going do
    try twc:= find_rule ~lvl:(!level) rules ta;
        keep_going:= false
    with NA -> level:= !level + 1
  done;
  !twc
;;


let strategy2 ?(latex=false) ?(print_lvl=0) ?(compile_latex=compile_latex_default) ?(steps=steps_default) ta= 
  let taref= ref ta and keep_going= ref true in
  print_steps_start ~latex ();
  print_step ~latex ~print_lvl ("   ", "") !taref;
  global_step:= 0;
  while !keep_going do
    begin 
      try 
        taref:= find_and_apply_rule neut_assoc_conv !taref;
        print_step ~latex ~print_lvl (" = ", "\\equiv") !taref;
      with NA -> ();
    end;
    try let twc= find_min_level dummy_active !taref in
        let ctxt= member_context_of_twc twc and t= member_term_of_twc twc in
        (* prints as needed; does not raise NA. *)
        taref:= ctxt (local_normal_reduce ~latex ~print_lvl ~ctxt ~lvl:1 ~steps t); 
    (* find_min_level *)
    with NA -> keep_going:=false;
  done;
  print_steps_stop ~latex ~compile_latex ();
  
;;

