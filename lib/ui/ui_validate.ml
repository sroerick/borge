(** Semantic validation for borge UI specs.

    Checks that references are valid:
    - (use component-name) refers to a defined component
    - (fill slot-name) refers to an existing slot in the layout
    - Variant names are unique per element
    - Theme variable references refer to defined entries *)

type issue = {
  message : string;
  severity : [ `Error | `Warning ];
}

let validate _app = []
  (* TODO: implement semantic validation *)
