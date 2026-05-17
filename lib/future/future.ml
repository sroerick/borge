(* Scan specs for planned/in-progress work (borge future command) - STUB *)

type future_item = {
  file : string;
  line : int;
  section_path : string;
  status : Spec.status;
  description : string;
  depends_on : string list;
}

type roadmap = {
  planned : future_item list;
  in_progress : future_item list;
  ready_for_review : future_item list;
}

let get_roadmap _dir = {
  planned = [];
  in_progress = [];
  ready_for_review = [];
}
