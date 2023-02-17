let read_line filepath =
  let channel = open_in filepath in
  try
    let line = input_line channel in
    close_in channel;
    line
  with
  | e ->
    (match e with
     | End_of_file -> ""
     | _ ->
       close_in_noerr channel;
       raise e)
;;

(* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str
    then (
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ]))
    else chars
  in
  explode_inner 0 []
;;
