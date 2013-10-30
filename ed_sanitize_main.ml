let main ~offset =
  let orig = BatPervasives.input_all stdin in
  let out = Ed_template.sanitize orig in
  print_string out
