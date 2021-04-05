# CoqTop-Elaborate

Elaborate Coq code with coqtop output.

## Demo

Given the following input:

```coq
Module NatPlayground.
  Inductive nat : Type :=
    | O
    | S (n : nat).

  Definition pred (n : nat) : nat :=
    match n with
    | O => O
    | S n' => n'
    end.

  Check (S (S (S (S O)))).
End NatPlayground.

Fixpoint evenb (n:nat) : bool :=
  match n with
  | O        => true
  | S O      => false
  | S (S n') => evenb n'
  end.

Definition oddb (n:nat) : bool :=
  negb (evenb n).

Example test_oddb_1: oddb 1 = true.
Proof.
  simpl.
  reflexivity.
Qed.

Example test_oddb_2: oddb 4 = false.
Proof.
  simpl.
  reflexivity.
Qed.
```

We have the following elaborated output:

```coq
Module NatPlayground.
(** Interactive Module NatPlayground started *)
  Inductive nat : Type :=
    | O
    | S (n : nat).
(** [Coq Proof View]
 * nat is defined
 * nat_rect is defined
 * nat_ind is defined
 * nat_rec is defined
 * nat_sind is defined
 *)

  Definition pred (n : nat) : nat :=
    match n with
    | O => O
    | S n' => n'
    end.
(** pred is defined *)

  Check (S (S (S (S O)))).
(** [Coq Proof View]
 * S (S (S (S O)))
 *      : nat
 *)
End NatPlayground.
(** Module NatPlayground is defined *)

Fixpoint evenb (n:nat) : bool :=
  match n with
  | O        => true
  | S O      => false
  | S (S n') => evenb n'
  end.
(** [Coq Proof View]
 * evenb is defined
 * evenb is recursively defined (guarded on 1st argument)
 *)

Definition oddb (n:nat) : bool :=
  negb (evenb n).
(** oddb is defined *)

Example test_oddb_1: oddb 1 = true.
(** [Coq Proof View]
 * 1 subgoal
 *
 *   =============
 *   oddb 1 = true
 *)
Proof.
  simpl.
(** [Coq Proof View]
 * 1 subgoal
 *
 *   =============
 *   oddb 1 = true
 *)
  reflexivity.
(** No more subgoals. *)
Qed.

Example test_oddb_2: oddb 4 = false.
(** [Coq Proof View]
 * 1 subgoal
 *
 *   ==============
 *   oddb 4 = false
 *)
Proof.
  simpl.
(** [Coq Proof View]
 * 1 subgoal
 *
 *   ==============
 *   oddb 4 = false
 *)
  reflexivity.
(** No more subgoals. *)
Qed.
```
