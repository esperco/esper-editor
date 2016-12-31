(* Very simpl(istic) HTML-to-text converter for email body *)

type element_class =
  | Hidden
  | Break_before
  | Break_after
  | Preformatted
  | Quote
  | Paragraph
  | List
  | Numbered_list
  | Space_before
  | Space_after
  | List_item
  | Contiguous

let classify_element = function
  | "javascript" | "style" | "head" -> Hidden
  | "br" -> Break_before
  | "pre" -> Preformatted
  | "blockquote" -> Quote
  | "p" | "div" | "table" -> Paragraph
  | "ul" -> List
  | "ol" -> Numbered_list
  | "tr" -> Break_after
  | "td" -> Space_after
  | "li" -> List_item
  | _ -> Contiguous

let compact_whitespace =
  let rex = Pcre.regexp "[ \t\n\r]+" in
  fun s ->
    let s = Pcre.substitute ~rex ~subst:(fun _ -> " ") s in
    if s = " " then ""
    else s

let space_rex = Pcre.regexp "^[ \t\r\n]+"
let remove_leading_space s =
  try
    match Pcre.extract ~rex:space_rex s with
    | [| prefix |] ->
        let prelen = String.length prefix in
        let len = String.length s in
        assert (prelen <= len);
        String.sub s prelen (len - prelen)
    | _ -> assert false
  with Not_found -> s

let text_of_html l =

  let starts_line = ref true in
  let prev_newlines = ref 2 in

  let nl buf indent newlines =
    let n = newlines - !prev_newlines in
    if n > 0 then (
      for i = 2 to n do
        Buffer.add_string buf ("\n" ^ indent);
      done;
      Buffer.add_string buf "\n";
      prev_newlines := n;
      starts_line := true;
    )
  in

  let remove_leading_space_if_needed s0 =
    if !starts_line then
      let s = remove_leading_space s0 in
      if s <> "" then
        starts_line := false;
      s
    else
      s0
  in

  let text buf indent s =
    if s <> "" then (
      if !prev_newlines > 0 then
        Buffer.add_string buf indent;
      Buffer.add_string buf (remove_leading_space_if_needed s);
      prev_newlines := 0;
    )
  in

  let rec print buf pre indent l =
    List.iter (print_node buf pre indent) l

  and print_node buf pre indent = function
    | Hclean.Data s ->
        let s =
          if pre then s
          else compact_whitespace s
        in
        text buf indent s
    | Hclean.Element (name, attrs, children) ->
        match classify_element name with
        | Hidden -> ()
        | Break_before ->
            nl buf indent 1;
            print buf pre indent children
        | Break_after ->
            print buf pre indent children;
            nl buf indent 1
        | Preformatted ->
            nl buf indent 2;
            print buf true indent children;
            nl buf indent 2
        | Quote ->
            nl buf indent 1;
            print buf pre ("> " ^ indent) children;
            nl buf indent 1
        | Paragraph ->
            nl buf indent 2;
            print buf pre indent children;
            nl buf indent 2
        | List
        | Numbered_list ->
            nl buf indent 2;
            print buf pre ("  " ^ indent) children;
            nl buf indent 2
        | Space_before ->
            text buf indent " ";
            print buf pre indent children
        | Space_after ->
            print buf pre indent children;
            text buf indent " "
        | List_item ->
            nl buf indent 1;
            text buf indent "• ";
            print buf pre ("  " ^ indent) children
        | Contiguous ->
            print buf pre indent children
  in
  let buf = Buffer.create 1000 in
  print buf false "" l;
  Buffer.contents buf

let of_html s =
  text_of_html (Hclean.of_string s)


let sample_input = "
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv='content-type' content='text/html; charset=UTF-8'>
    <title>Esper</title>

    <meta name='viewport' content='width=device-width, initial-scale=1'>
    <link href='/assets/css/bootstrap.min.css' rel='stylesheet' media='screen'>
  </head>

  <body>

<pre>
Code!     Code!
     Code!
Code!     Code!
</pre>

    <div id='global-status' class='animated'></div>

    <ul id='desktop-navbar' class='hide nav nav-pills nav-stacked home-page'>
      <li class='nav-header unselectable'></li>
      <li>
        <a href='#' class='account-block hide' data-toggle='pill'>
          <img id='settings-icon' class='svg' src='/assets/img/settings.svg'>
          <div class='nav-text'>Settings</div>
        </a>
      </li>
      <li>
        <a href='#!logout' class='account-block hide'>
          <img id='logout-icon' class='svg' src='/assets/img/logout.svg'>
          <div class='nav-text'>Log out</div>
        </a>
      </li>
      <li>
        <a href='#' class='nav-block hide' data-toggle='pill'>
          <img id='search-icon' class='svg' src='/assets/img/search.svg'>
          <div class='nav-text'>Search</div>
        </a>
      </li>
    </ul>
    <blockquote>
      <p>He said this.<br>And something else on another line.</p>
      <blockquote>
        <p>I originally
           said that.</p>
      </blockquote>
    </blockquote>
  </body>
</html>
"

let sample_output =
"
Code!     Code!
     Code!
Code!     Code!


  * 
  * 

    Settings

  * 

    Log out

  * 

    Search

"
