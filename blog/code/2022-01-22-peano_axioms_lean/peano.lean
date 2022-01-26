namespace sandbox

@[derive decidable_eq]
inductive Nat
    | zero : Nat
    | succ (n : Nat) : Nat

open Nat

------------------
-- Peano Axioms --
------------------

-- 1) 0 is a natural number
instance : has_zero Nat := ⟨zero⟩

axiom zero_is_nat : zero = 0

-- 2) reflexive: equality is reflexive
axiom reflexive (n : Nat) : n = n

-- 3) symmetry of equality
axiom symm (n m: Nat) : n = m -> m = n

-- 4) transitivity of equality
axiom trans (a b c: Nat) : a = b ∧ b = c -> a = c

-- 5) natural are closed under equality
-- ???

-- 6) succ is Nat
-- definition of Nat.succ

-- 7) injection
axiom injection (m n: Nat) :  m = n <-> succ(m) = succ(n)

-- 8) succ(n) cannot be 0
axiom non_zero_succ (n : Nat) : succ(n) = 0 -> false

---------------------
-- Addition Axioms --
---------------------

def add : Nat → Nat → Nat
| m 0 := m
| m (succ n) := succ (add m n)

-- Defines the + operator
instance : has_add Nat := ⟨add⟩

-- Explicit statements based on the definition of add
axiom add_zero (n : Nat) : n + 0 = n
axiom add_succ (m n: Nat) : m + succ n = succ (m + n)

------------
-- Lemmas --
------------

  lemma zero_add (n : Nat) : 0 + n = n :=
    begin
    induction n with d hd,

    -- base
    rw zero_is_nat,
    rw add_zero,

    -- inductive step
    rw add_succ,
    rw hd,
    end

-- Show that addition is associative
lemma add_assoc (a b c : Nat) : (a + b) + c = a + (b + c) :=
begin
  induction c with d hd,

  -- base
  rw zero_is_nat,
  repeat {rw add_zero},

  -- inductive step
  repeat {rw add_succ},
  rw hd,

end

lemma x (n: Nat): (n + 0) + n = n + (n + 0) :=
  begin
  rw add_zero,
  end


end sandbox
