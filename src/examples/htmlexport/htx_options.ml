include Cli.Make(
  struct
    let shortname = "htx"
    let name = "Plugin example (HTML export)"
  end
)

module Directory =
  Builder.String(
      struct
        let name = "html-directory"
        let default = "binsec_cfg_html"
        let doc = "Set HTML directory for HTML export"
      end)


module Level =
  Builder.Variant_choice_assoc(
      struct
        let name = "export-level"

        let doc = " Set level of details for HTML export"

        type t = [ `Callgraph | `Function | `Mnemonic ]
        let assoc_map = [
            "callgraph", `Callgraph;
            "function",  `Function;
            "mnemonic",  `Mnemonic;
          ]
        ;;

        let default = `Callgraph
      end
    )
