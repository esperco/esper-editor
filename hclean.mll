(*
   HTML snippet parser and printer, designed for server-side HTML
   sanitization.
   UTF-8 character encoding is assumed.
*)
{
  open Printf

  type token =
      Open_element of (string * (string * string option) list)
    | Close_element of string
    | Empty_element of (string * (string * string option) list)
    | Cdata of string

  let fuse_cdata l =
    let rec fuse buf = function
      | Cdata s :: l ->
          Buffer.add_string buf s;
          fuse buf l
      | l ->
          Buffer.contents buf, l
    in
    let rec scan acc = function
      | Cdata "" :: l ->
          scan acc l
      | Cdata _ :: _ as l ->
          let s, l = fuse (Buffer.create 100) l in
          scan (Cdata s :: acc) l
      | x :: l ->
          scan (x :: acc) l
      | [] ->
          List.rev acc
    in
    scan [] l

  let encode_data ~quot s =
    let buf = Buffer.create (2 * String.length s) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '\"' when quot -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c
    done;
    Buffer.contents buf

  (* taken from Utf8val *)
  let utf8_encode buf x =
    (* Straight <= doesn't work with signed 31-bit ints
       (which are outside of Unicode but supported by UTF-8) *)
    let maxbits n x = x lsr n = 0 in
    let add = Buffer.add_char in

    if maxbits 7 x then
      (* 7 *)
      add buf (Char.chr x)
    else if maxbits 11 x then (
      (* 5 + 6 *)
      add buf (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
      add buf (Char.chr (0b10000000 lor (x         land 0b00111111)))
    )
    else if maxbits 16 x then (
      (* 4 + 6 + 6 *)
      add buf (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
      add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor (x          land 0b00111111)))
    )
    else if maxbits 21 x then (
      (* 3 + 6 + 6 + 6 *)
      add buf (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
      add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
    )
    else if maxbits 26 x then (
      (* 2 + 6 + 6 + 6 + 6 *)
      add buf (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
      add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
    )
    else (
      assert (maxbits 31 x);
      (* 1 + 6 + 6 + 6 + 6 + 6 *)
      add buf (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
      add buf (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
    )

  let string_of_unicode n =
    let buf = Buffer.create 6 in
    utf8_encode buf n;
    Buffer.contents buf

  (* 252 standard HTML entities + &apos; (XML only) *)
  let entities = [
    "lt", 60;
    "gt", 62;
    "amp", 38;
    "quot", 34;
    "apos", 39;
    "nbsp", 160;
    "iexcl", 161;
    "cent", 162;
    "pound", 163;
    "curren", 164;
    "yen", 165;
    "brvbar", 166;
    "sect", 167;
    "uml", 168;
    "copy", 169;
    "ordf", 170;
    "laquo", 171;
    "not", 172;
    "shy", 173;
    "reg", 174;
    "macr", 175;
    "deg", 176;
    "plusmn", 177;
    "sup2", 178;
    "sup3", 179;
    "acute", 180;
    "micro", 181;
    "para", 182;
    "middot", 183;
    "cedil", 184;
    "sup1", 185;
    "ordm", 186;
    "raquo", 187;
    "frac14", 188;
    "frac12", 189;
    "frac34", 190;
    "iquest", 191;
    "Agrave", 192;
    "Aacute", 193;
    "Acirc", 194;
    "Atilde", 195;
    "Auml", 196;
    "Aring", 197;
    "AElig", 198;
    "Ccedil", 199;
    "Egrave", 200;
    "Eacute", 201;
    "Ecirc", 202;
    "Euml", 203;
    "Igrave", 204;
    "Iacute", 205;
    "Icirc", 206;
    "Iuml", 207;
    "ETH", 208;
    "Ntilde", 209;
    "Ograve", 210;
    "Oacute", 211;
    "Ocirc", 212;
    "Otilde", 213;
    "Ouml", 214;
    "times", 215;
    "Oslash", 216;
    "Ugrave", 217;
    "Uacute", 218;
    "Ucirc", 219;
    "Uuml", 220;
    "Yacute", 221;
    "THORN", 222;
    "szlig", 223;
    "agrave", 224;
    "aacute", 225;
    "acirc", 226;
    "atilde", 227;
    "auml", 228;
    "aring", 229;
    "aelig", 230;
    "ccedil", 231;
    "egrave", 232;
    "eacute", 233;
    "ecirc", 234;
    "euml", 235;
    "igrave", 236;
    "iacute", 237;
    "icirc", 238;
    "iuml", 239;
    "eth", 240;
    "ntilde", 241;
    "ograve", 242;
    "oacute", 243;
    "ocirc", 244;
    "otilde", 245;
    "ouml", 246;
    "divide", 247;
    "oslash", 248;
    "ugrave", 249;
    "uacute", 250;
    "ucirc", 251;
    "uuml", 252;
    "yacute", 253;
    "thorn", 254;
    "yuml", 255;
    "fnof", 402;
    "Alpha", 913;
    "Beta", 914;
    "Gamma", 915;
    "Delta", 916;
    "Epsilon", 917;
    "Zeta", 918;
    "Eta", 919;
    "Theta", 920;
    "Iota", 921;
    "Kappa", 922;
    "Lambda", 923;
    "Mu", 924;
    "Nu", 925;
    "Xi", 926;
    "Omicron", 927;
    "Pi", 928;
    "Rho", 929;
    "Sigma", 931;
    "Tau", 932;
    "Upsilon", 933;
    "Phi", 934;
    "Chi", 935;
    "Psi", 936;
    "Omega", 937;
    "alpha", 945;
    "beta", 946;
    "gamma", 947;
    "delta", 948;
    "epsilon", 949;
    "zeta", 950;
    "eta", 951;
    "theta", 952;
    "iota", 953;
    "kappa", 954;
    "lambda", 955;
    "mu", 956;
    "nu", 957;
    "xi", 958;
    "omicron", 959;
    "pi", 960;
    "rho", 961;
    "sigmaf", 962;
    "sigma", 963;
    "tau", 964;
    "upsilon", 965;
    "phi", 966;
    "chi", 967;
    "psi", 968;
    "omega", 969;
    "thetasym", 977;
    "upsih", 978;
    "piv", 982;
    "bull", 8226;
    "hellip", 8230;
    "prime", 8242;
    "Prime", 8243;
    "oline", 8254;
    "frasl", 8260;
    "weierp", 8472;
    "image", 8465;
    "real", 8476;
    "trade", 8482;
    "alefsym", 8501;
    "larr", 8592;
    "uarr", 8593;
    "rarr", 8594;
    "darr", 8595;
    "harr", 8596;
    "crarr", 8629;
    "lArr", 8656;
    "uArr", 8657;
    "rArr", 8658;
    "dArr", 8659;
    "hArr", 8660;
    "forall", 8704;
    "part", 8706;
    "exist", 8707;
    "empty", 8709;
    "nabla", 8711;
    "isin", 8712;
    "notin", 8713;
    "ni", 8715;
    "prod", 8719;
    "sum", 8721;
    "minus", 8722;
    "lowast", 8727;
    "radic", 8730;
    "prop", 8733;
    "infin", 8734;
    "ang", 8736;
    "and", 8743;
    "or", 8744;
    "cap", 8745;
    "cup", 8746;
    "int", 8747;
    "there4", 8756;
    "sim", 8764;
    "cong", 8773;
    "asymp", 8776;
    "ne", 8800;
    "equiv", 8801;
    "le", 8804;
    "ge", 8805;
    "sub", 8834;
    "sup", 8835;
    "nsub", 8836;
    "sube", 8838;
    "supe", 8839;
    "oplus", 8853;
    "otimes", 8855;
    "perp", 8869;
    "sdot", 8901;
    "lceil", 8968;
    "rceil", 8969;
    "lfloor", 8970;
    "rfloor", 8971;
    "lang", 9001;
    "rang", 9002;
    "loz", 9674;
    "spades", 9824;
    "clubs", 9827;
    "hearts", 9829;
    "diams", 9830;
    "OElig", 338;
    "oelig", 339;
    "Scaron", 352;
    "scaron", 353;
    "Yuml", 376;
    "circ", 710;
    "tilde", 732;
    "ensp", 8194;
    "emsp", 8195;
    "thinsp", 8201;
    "zwnj", 8204;
    "zwj", 8205;
    "lrm", 8206;
    "rlm", 8207;
    "ndash", 8211;
    "mdash", 8212;
    "lsquo", 8216;
    "rsquo", 8217;
    "sbquo", 8218;
    "ldquo", 8220;
    "rdquo", 8221;
    "bdquo", 8222;
    "dagger", 8224;
    "Dagger", 8225;
    "permil", 8240;
    "lsaquo", 8249;
    "rsaquo", 8250;
    "euro", 8364;
  ]

  let entity_tbl =
    let tbl = Hashtbl.create 500 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k (string_of_unicode v)) entities;
    tbl

  let string_of_entity_name s =
    try Hashtbl.find entity_tbl s
    with Not_found -> ""
}

let letter = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let namechar = letter | digit | '.' | ':' | '-' | '_'
let name = ( letter | '_' | ':' ) namechar*
let ws = [ ' ' '\t' '\r' '\n' ]
let unquoted_attribute = [^ '"' '\'' '>' ' ' '\t' '\n' '\r' ]+

rule parse_nodes acc = parse
  | "<!--" ([^'-'] | '-'[^'-'])* "-->"
      { (* ignore comment *)
        parse_nodes acc lexbuf }
  | "<!" [^'>']* ">"
      { (* ignore doctype *)
        parse_nodes acc lexbuf }
  | "<?" [^'>']* "?>"
      { (* ignore xml processing instruction *)
        parse_nodes acc lexbuf }
  | "<" (name as elt_name)
      { let closed, l = parse_attributes [] lexbuf in
        let elt_name = String.lowercase_ascii elt_name in
        let x =
          if closed then
            Empty_element (elt_name, l)
          else
            Open_element (String.lowercase_ascii elt_name, l)
        in
        parse_nodes (x :: acc) lexbuf }
  | "</" (name as elt_name) ws* ">"
      { let x = Close_element elt_name in
        parse_nodes (x :: acc) lexbuf }
  | "&"
      { let x = parse_entity lexbuf in
        parse_nodes (Cdata x :: acc) lexbuf }
  | "<"
      { (* tolerate unescaped "<" *)
        parse_nodes (Cdata "<" :: acc) lexbuf }
  | [^ '<' '&']+ as s
      { parse_nodes (Cdata s :: acc) lexbuf }
  | eof
      { fuse_cdata (List.rev acc) }

and parse_entity = parse
  | "#x" (hexdigit+ as s) ";"
      { string_of_unicode (int_of_string ("0x" ^ s)) }
  | "#" (digit+ as s) ";"
      { string_of_unicode (int_of_string s) }
  | (name as name) ";"
      { string_of_entity_name name }
  | ""
      { (* tolerate unescaped "&" *)
        "&" }

and parse_attributes acc = parse
  | ">"
      { false, List.rev acc }
  | "/>"
      { true, List.rev acc }
  | ws+
      { parse_attributes acc lexbuf }

  | (name as k)
      { parse_attributes ((String.lowercase_ascii k, None) :: acc) lexbuf }

  | (name as k) ws* "=" (unquoted_attribute as v)
      { parse_attributes ((String.lowercase_ascii k, Some v) :: acc) lexbuf }

  | (name as k) ws* "=" ws* '"'
      { let v = parse_string_literal1 (Buffer.create 100) lexbuf in
        parse_attributes ((String.lowercase_ascii k, Some v) :: acc) lexbuf }

  | (name as k) ws* "=" ws* "'"
      { let v = parse_string_literal2 (Buffer.create 100) lexbuf in
        parse_attributes ((String.lowercase_ascii k, Some v) :: acc) lexbuf }

  | _
      { (* ignore junk *)
        parse_attributes acc lexbuf }

  | eof
      { false, List.rev acc }

and parse_string_literal1 buf = parse
  | ( [^ '"' '&']* as s)
      { Buffer.add_string buf s;
        parse_string_literal1 buf lexbuf }
  | "&"
      { Buffer.add_string buf (parse_entity lexbuf);
        parse_string_literal1 buf lexbuf }
  | ('"' | eof)
      { Buffer.contents buf }

and parse_string_literal2 buf = parse
  | ( [^ '\'' '&']* as s)
      { Buffer.add_string buf s;
        parse_string_literal2 buf lexbuf }
  | "&"
      { Buffer.add_string buf (parse_entity lexbuf);
        parse_string_literal2 buf lexbuf }
  | ('\'' | eof)
      { Buffer.contents buf }

{
  type node =
    | Data of string
    | Element of string * (string * string option) list * node list

  type t = node list

  let rec close_all pending acc =
    match pending with
    | [] -> List.rev acc
    | ((name, attrs), parent_acc) :: pending ->
        close_all pending (Element (name, attrs, List.rev acc) :: parent_acc)

  let rec make_seq pending acc l =
    match l with
    | Open_element x :: l ->
        make_seq ((x, acc) :: pending) [] l
    | Close_element name :: l ->
        close_matching name pending acc l
    | Empty_element (name, attrs) :: l ->
        make_seq pending (Element (name, attrs, []) :: acc) l
    | Cdata s :: l ->
        make_seq pending (Data s :: acc) l
    | [] ->
        close_all pending acc

  and close_matching cl_name pending acc l =
    match pending with
    | [] ->
        (* drop closing tag with no matching opening tag *)
        make_seq [] acc l
    | ((op_name, attrs), parent_acc) :: pending ->
        if op_name = cl_name then
          let node = Element (op_name, attrs, List.rev acc) in
          make_seq pending (node :: parent_acc) l
        else
          (* continue auto-closing until matching element is found *)
          let node = Element (op_name, attrs, List.rev acc) in
          close_matching cl_name pending (node :: parent_acc) l

  let of_string s =
    let lexbuf = Lexing.from_string s in
    make_seq [] [] (parse_nodes [] lexbuf)

  let to_string ?(noclose = []) l =
    let noclose_tbl = Hashtbl.create (2 * List.length noclose) in
    List.iter
      (fun k -> Hashtbl.replace noclose_tbl (String.lowercase_ascii k) ())
      noclose;
    let rec print_seq buf l =
      List.iter (function
        | Data s ->
            Buffer.add_string buf (encode_data ~quot:false s)
        | Element (name, attrs, children) ->
            bprintf buf "<%s%a>%a"
              name print_attributes attrs
              print_seq children;
            if not (Hashtbl.mem noclose_tbl name) then
              bprintf buf "</%s>" name
      ) l
    and print_attributes buf l =
      List.iter (fun x -> bprintf buf " %a" print_attribute x) l

    and print_attribute buf (name, opt_val) =
      match opt_val with
      | None -> bprintf buf "%s" name
      | Some s -> bprintf buf "%s=\"%s\"" name (encode_data ~quot:true s)
    in
    let buf = Buffer.create 1000 in
    print_seq buf l;
    Buffer.contents buf

  let rewrite ?noclose s =
    to_string ?noclose (of_string s)

  (* Utilities *)
  let is_data = function Data _ -> true | _ -> false
  let is_element = function Element _ -> true | _ -> false
  let attr name attrs = try Some (List.assoc name attrs) with Not_found -> None
  let has_attr name attrs = List.exists (fun (k, v) -> k = name) attrs
  let has_attr_val name attrs =
    List.exists (fun (k, v) -> k = name && v <> None) attrs

  let split_re = Str.regexp "[ \t\r\n]+"
  let split s = Str.split split_re s
  let classes attrs =
    match attr "class" attrs with
    | Some (Some s) -> split s
    | _ -> []
  let has_class cl attrs =
    List.mem cl (classes attrs)

  let make_test (input, output) =
    (input, (fun () -> rewrite ~noclose:["br"; "img"] input = output))

  let tests =
    List.map make_test [
      "<p>",
      "<p></p>";

      "<a>X<b>",
      "<a>X<b></b></a>";

      "<a><b></a>",
      "<a><b></b></a>";

      "<a><b><c></a>",
      "<a><b><c></c></b></a>";

      "<div class='abc'>",
      "<div class=\"abc\"></div>";

      "<div class=\"abc\">",
      "<div class=\"abc\"></div>";

      "A<div class='abc'>def<a>J<b>K</a>X</div>Z",
      "A<div class=\"abc\">def<a>J<b>K</b></a>X</div>Z";

      "&eacute;",
      "\xc3\xa9";

      "<a href=\"/?x=0&amp;y=0\">x</a>",
      "<a href=\"/?x=0&amp;y=0\">x</a>";

      "hello, <!-- nothing -->world",
      "hello, world";

      "<>",
      "&lt;&gt;";

      "<p>a<br>b<img/></p>",
      "<p>a<br>b<img></p>";
    ]
}
