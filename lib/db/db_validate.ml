(** Semantic validation for borge DB specs.

    Checks that references are valid:
    - (references table.column) refers to an existing table
    - (ownership column) refers to an existing column on the same table
    - Group capability tables exist in the schema
    - Operations reference existing tables *)

type issue = {
  message : string;
  severity : [ `Error | `Warning ];
}

let validate _app = []
  (* TODO: implement semantic validation *)
