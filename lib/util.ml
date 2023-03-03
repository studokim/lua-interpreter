let read_file filepath =
  let channel = open_in filepath in
  let program = really_input_string channel (in_channel_length channel) in
  close_in channel;
  program
;;
