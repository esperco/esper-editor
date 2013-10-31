(*
   <span class="var" title="VARNAME">LABEL</span>
     is replaced by the value (text) of the variable VARNAME.
     LABEL must be just data.

   Sanitization rules:

   1. ... are removed in the following:

     <span class="... var ..." title="VARNAME" ...>LABEL</span>
     <div ...> becomes <p>
     <p ...> becomes <p>
     <br ...>  becomes <br>

   2. The children elements of var nodes are removed.
   3. All other markup is removed, but data is preserved.
   4. The contenteditable="false" attribute is added to var nodes
      for user-agent's convenience.
*)
let rec strip l =
  let open Hclean in
  match l with
  | Element ("span", attrs, children) :: l
      when Hclean.has_attr_val "title" attrs
        && Hclean.has_class "var" attrs ->
      let varname =
        match Hclean.attr "title" attrs with
        | Some (Some s) -> s
        | _ -> assert false
      in
      let attrs = [
        "class", Some "var"; (* other classes were removed *)
        "title", Some varname;
        "contenteditable", Some "false";
      ] in
      let children = BatList.filter Hclean.is_data children in
      let e = Element ("span", attrs, children) in
      e :: strip l

  | Element (("p"|"div"), attrs, children) :: l ->
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

and subst_node env node =
  let open Hclean in
  match node with
  | Element ("span", attrs, children)
      when Hclean.has_attr_val "title" attrs
        && Hclean.has_class "var" attrs ->
      let varname =
        match Hclean.attr "title" attrs with
        | Some (Some s) -> s
        | _ -> assert false
      in
      (try Data (List.assoc varname env)
       with Not_found -> failwith ("Unbound template variable " ^ varname))
  | Element (name, attrs, children) ->
      Element (name, attrs, subst env children)
  | Data _ as x -> x

let parse html =
  if String.length html > 1_000_000 then
    (* avoid stack overflows from malicious input that may crash the server *)
    invalid_arg "Ed_template: input too long"
  else
    Hclean.of_string html

let sanitize html =
  let seq = strip (parse html) in
  Hclean.to_string seq

let instantiate env html =
  let template = strip (parse html) in
  let result = subst env template in
  Hclean.to_string result

(* Tests *)

let test_sanitize_input = "
<!doctype html>
<html>
__<head>
__</head>
__<body>
____<p>
______Have you ever seen rain?
______<div class=\"foo bar\">
________Nope, but <span class=\"z var \" title=\"theirname\">THEIR NAME</span>.
______</div>
____<p>
______Naturally.<br>
______Right.
__</body>
</html>
"

let test_sanitize_output = "


__
__
__
____<p>
______Have you ever seen rain?
______<p>
________Nope, but <span class=\"var\" title=\"theirname\" contenteditable=\"false\">THEIR NAME</span>.
______</p>
____<p>
______Naturally.<br>
______Right.
__</br></p></p>

"

let test_sanitize () =
  sanitize test_sanitize_input = test_sanitize_output

let test_instantiate_template = "
<p>
Hello, <span class='var' title='name'>NAME</span>.
2 * <span class='whatever var' title='x'>X</span> =
  <span class='var' title='&gt;'>Y</span>
"

let test_instantiate_env = [
  "x", "1";
  ">", "2";
  "name", "Bob";
]

let test_instantiate () =
  let output = "
<p>
Hello, Bob.
2 * 1 =
  2
</p>"
  in
  instantiate test_instantiate_env test_instantiate_template = output

let tests = [
  "html sanitization", test_sanitize;
  "html template instantiation", test_instantiate;
]
