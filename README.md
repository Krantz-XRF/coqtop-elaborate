# CoqTop-Elaborate

Elaborate Coq code with coqtop output.

## Demo

Given the following input:

```coq
Theorem refl : forall {X} {x : X}, x = x.
Proof.
  intros X x.
  reflexivity.
Qed.
```

We have the following elaborated output:

```coq
Theorem refl : forall {X} {x : X}, x = x.
(** [Coq Proof View]
 * 1 subgoal
 *
 *   ================================
 *   forall (X : Type) (x : X), x = x
 *)
Proof.
  intros X x.
(** [Coq Proof View]
 * 1 subgoal
 *
 *   X : Type
 *   x : X
 *   ========
 *   x = x
 *)
  reflexivity.
(** No more subgoals. *)
Qed.
```
