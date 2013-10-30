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


let sanitize html =
  if String.length html > 1_000_000 then
    (* avoid stack overflows from malicious input that may crash the server *)
    invalid_arg "Editor.sanitize: input too long"
  else
    let seq = Hclean.of_string html in
    let seq = strip seq in
    Hclean.to_string seq
