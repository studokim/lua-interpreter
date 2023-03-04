(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let read_file filepath =
  let channel = open_in filepath in
  let program = really_input_string channel (in_channel_length channel) in
  close_in channel;
  program
;;

let concat = String.concat ""
