# CoqTop-Elaborate

[![language](https://img.shields.io/badge/language-Haskell-blue)](https://www.haskell.org)
[![license](https://img.shields.io/badge/Source-AGPL--v3.0-blueviolet)](https://www.gnu.org/licenses/agpl-3.0.html)
![build](https://github.com/Krantz-XRF/coqtop-elaborate/workflows/build/badge.svg)
[![docs](https://img.shields.io/badge/Doc-GitHub%20Pages-brightgreen)](https://krantz-xrf.github.io/coqtop-elaborate/)

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

## License

This program is free software: you can redistribute it and/or modify it under the terms of *the GNU Affero General Public License* as published by *the Free Software Foundation*, either version 3 of *the License*, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
