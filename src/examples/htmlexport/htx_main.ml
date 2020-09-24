open Htx_options ;;

let mkdir dirname =
  if not @@ Sys.file_exists dirname then Unix.mkdir dirname 0o700 ;;

let sub_dir dirname ~base = Filename.concat base dirname ;;

let func_dir = "functions"
let mnem_dir = "mnemonics"
;;

let function_directory = sub_dir func_dir
let mnemonic_directory = sub_dir mnem_dir
;;

let dirfile dirbase ~filename =
  let base = Directory.get () in
  Filename.concat (sub_dir dirbase ~base) filename
;;

let _mnemonic_file = dirfile mnem_dir
let _function_file = dirfile func_dir
let html_file file = file ^ ".html"
;;



let create_base_directory ~base =
  let dirname = Filename.dirname base in
  if Sys.file_exists dirname then begin
      Logger.debug "Creating base directory for HTML export %s ..." base;
      mkdir base;
    end
  else Logger.fatal "Please create directory %s before starting BINSEC." dirname
;;

let create_directories () =
  let base = Directory.get () in
  match Level.get () with
  | `Callgraph -> create_base_directory ~base
  | `Function ->
     create_base_directory ~base;
    (* Now we can create directories located inside our base directory for
       functions *)
     mkdir @@ function_directory ~base
  | `Mnemonic ->
     (* Same as function with an added directory dedicated to menmonic graphs *)
     create_base_directory ~base;
     mkdir @@ function_directory ~base;
     mkdir @@ mnemonic_directory ~base
;;


let export_callgraph _cfg =
  let file =
    Filename.concat (Directory.get ()) (html_file "callgraph") in
  let oc = open_out_bin file in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "Callgraph@.";
  close_out oc
;;


let export cfg =
  export_callgraph cfg
;;

let _run cfg =
  (* First setup the directories for our exported files *)
  create_directories ();
  export cfg
;;


(* Cli.Boot.enlist ~f:export_cfg ~name:"Export IDA-imported CFG as HTML"; *)
