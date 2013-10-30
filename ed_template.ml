(*
   <span class="VAR">label</span>  is preserved or expanded
   <div ...> becomes <p>
   <p ...> becomes <p>
   <br ...>  becomes <br>
   all other markup is removed
*)
let rec strip l =
  let open Hclean in
  match l with
  | Element ("span", ["class", Some var], ([] | [Data _])) as x :: l ->
      x :: strip l

  | Element ("div", attrs, children) :: l ->
      let e = Element ("p", [], strip children) in
      e :: strip l

  | Element ("br", attrs, children) :: l ->
      let e = Element ("br", [], strip children) in
      e :: strip l

  | Element (_, _, children) :: l ->
      let seq = strip children in
      seq @ strip l

  | Data s :: l ->
      Data s :: strip l

  | [] -> []

let rec subst env l =
  BatList.map (subst_node env) l

and subst_node env = function
  | Hclean.Element ("span", ["class", Some var], children) ->
      (try Hclean.Data (List.assoc var env)
       with Not_found -> failwith ("Unbound template variable " ^ var))
  | x -> x

let parse html =
  if String.length html > 1_000_000 then
    (* avoid stack overflows from malicious input that may crash the server *)
    invalid_arg "Ed_template: input too long"
  else
    Hclean.of_string html

(*
   Remove or rewrite unsupported markup from HTML snippet
*)
let sanitize html =
  let seq = strip (parse html) in
  Hclean.to_string seq

(*
   Sanitize and replace variables with text
*)
let instantiate env html =
  let template = strip (parse html) in
  let result = subst env template in
  Hclean.to_string result
