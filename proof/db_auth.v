(* proof/db_auth.v — witness for the ownership-consistency obligation.

   This is the human-authored proof that discharges the canonical
   (asserts (property ownership-consistency ...)) obligation declared in
   examples/crud-app/db.borg. It is checked against the emitted Coq
   representation (proof/db_auth_schema.v, produced by lib/proof/proof_emit.ml
   from the typed DB AST).

   The property: for every (group G, operation op, table T) permitted by
   the spec, EITHER G is admin/can-all (discharged via is_admin), OR the
   capability carries no where-predicate (unrestricted read — no
   ownership constraint to violate), OR the column referenced by the
   where-predicate matches the table's declared (ownership ...) column.

   This catches the bug class where a spec author declares
   (ownership owner-id) on a table but writes a (can ... (where
   assignee-id = current-user)) rule — a contradiction no test catches,
   because no input exercises it; the contradiction is in the spec itself.

   See docs/engine.borg subsection `obligations` for the contract. *)

Require Import BorgeSchema.

(* The theorem. The witness quantifies over all (group, operation, table)
   triples and case-analyzes against the inductives the emitter produces.
   For the crud-app spec, the emitter produces:
     - tables: Users, Projects, Tasks, Comments
     - owner_of: Projects => Some "owner-id", Tasks => Some "assignee-id",
                   Comments => Some "author-id", Users => None
     - groups: Admin, Member, Viewer
     - is_admin: is_admin_Admin : is_admin Admin
     - permitted: permitted_Member_Create_Tasks, permitted_Member_Read_Tasks,
                   permitted_Member_Update_Tasks, permitted_Member_Read_Projects,
                   permitted_Member_Create_Comments, permitted_Member_Read_Comments,
                   permitted_Viewer_Read_Tasks, permitted_Viewer_Read_Projects,
                   permitted_Viewer_Read_Comments
     - predicate_column_of: Member,Create,Tasks => Some "assignee-id";
                              Member,Read,Tasks => Some "assignee-id";
                              Member,Update,Tasks => Some "assignee-id";
                              Member,Read,Projects => None;  (* no where-clause *)
                              Member,Create,Comments => None;
                              Member,Read,Comments => None;
                              Viewer,Read,Tasks => None;
                              Viewer,Read,Projects => None;
                              Viewer,Read,Comments => None;
                              _,_,_ => None
   The proof cases on the permitted hypothesis and on whether the group
   is admin. *)

Theorem ownership_consistency :
  forall (g : group) (op : operation) (t : table),
    permitted g op t ->
    is_admin g \/
    (predicate_column_of g op t = None \/
     predicate_column_of g op t = owner_of t).
Proof.
  intros g op t Hperm.
  (* Case on whether g is admin first — the escape hatch. *)
  destruct g as [| |].
  - (* Admin *) left. apply is_admin_Admin.
  - (* Member *) right.
    (* Case-analyze the permitted hypothesis for Member.
       Each permitted_Member_*_* constructor corresponds to one
       (op, t) pair the member is allowed. *)
    inversion Hperm; subst.
    + (* permitted_Member_Create_Tasks *)
      right. reflexivity.
    + (* permitted_Member_Read_Tasks *)
      right. reflexivity.
    + (* permitted_Member_Update_Tasks *)
      right. reflexivity.
    + (* permitted_Member_Read_Projects *)
      (* No where-clause → predicate_column_of = None → left disjunct. *)
      left. reflexivity.
    + (* permitted_Member_Create_Comments *)
      left. reflexivity.
    + (* permitted_Member_Read_Comments *)
      left. reflexivity.
  - (* Viewer *) right.
    inversion Hperm; subst.
    + (* permitted_Viewer_Read_Tasks — no where-clause → None *)
      left. reflexivity.
    + (* permitted_Viewer_Read_Projects — no where-clause → None *)
      left. reflexivity.
    + (* permitted_Viewer_Read_Comments — no where-clause → None *)
      left. reflexivity.
Qed.

(* Note on the admits cap: this proof is complete (0 admits). If the
   spec were inconsistent — say ownership(tasks) = "owner-id" but the
   member's where-predicate referenced "assignee-id" — the
   permitted_Member_Create_Tasks case would fail to discharge because
   owner_of Tasks = Some "owner-id" <> Some "assignee-id" = predicate.
   Coq would reject the proof, surfacing the contradiction. That
   rejection is the whole point: a property that fails to prove is a
   spec bug, reported as a Fail verdict in the .borg.meta proof block. *)
