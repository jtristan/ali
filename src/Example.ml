(***********************************************************************)
(*                                                                     *)
(*                               ALI                                   *)
(*                                                                     *)
(*                       Jean-Baptiste Tristan                         *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  described in file ../../LICENSE.                                   *)
(*                                                                     *)
(***********************************************************************)


open LLVM

let f (x : modul) = 
  (* count x; print_report(); *) AsmWriter.print x; flush stdout
  (*print x; flush stdout; x *)
;;

register f;;


