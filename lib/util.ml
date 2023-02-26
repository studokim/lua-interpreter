let read_file filepath =
  let channel = open_in filepath in
  let program = really_input_string channel (in_channel_length channel) in
  close_in channel;
  program
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
