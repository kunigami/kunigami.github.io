import tactic

namespace sb

inductive Nat
| zero : Nat
| succ (n : Nat) : Nat

open Nat

instance : has_zero Nat := ⟨zero⟩
axiom zero_is_nat : zero = 0

instance : has_one Nat := ⟨succ zero⟩
axiom one_is_nat : succ zero = 1

-- addition
def add : Nat → Nat → Nat
| m 0 := m
| m (succ n) := succ (add m n)

instance : has_add Nat := ⟨add⟩
axiom add_zero (m: Nat) : m + 0 = m
axiom add_succ (m n: Nat) : m + succ n = succ (m + n)

-- subtraction
def sub : Nat → Nat → Nat
  | (succ a) (succ b) := sub a b
  | a b := a

instance : has_sub Nat := ⟨sub⟩
axiom sub_zero (a: Nat) : a - 0 = a
axiom sub_eq (a: Nat) : a - a = 0
axiom sub_assoc (a b c: Nat): a - b + c = a - (b - c)

-- multiplication
def mul : Nat → Nat → Nat
| m 0 := 0
| m (succ n) := m + (mul m n)

instance : has_mul Nat := ⟨mul⟩

axiom mul_zero (n: Nat) : n * 0 = 0
axiom mul_succ (n m: Nat): m * (succ n) = m + (m * n)

axiom one_mul (n: Nat) : 1 * n = n
axiom mul_dist_add (a b c: Nat) : a * ( b + c) = a * b + a * c
axiom mul_dist_sub (a b c: Nat) : a * ( b - c) = a * b - a * c
axiom mul_assoc (a b c: Nat) : a * ( b * c) = a * b * c
axiom mul_comm (a b: Nat) : a * b = b * a

-- divisibility
def dvd (a b : Nat) : Prop :=
  ∃ (c : Nat), b = a * c

instance : has_dvd Nat := ⟨dvd⟩

lemma one_dvd_n (n : Nat) : 1 ∣ n :=
begin
use n,
rw one_mul,
end

def is_gcd (a b g: Nat) : Prop :=
  (g ∣ a) ∧ (g ∣ b) ∧ (∀ x: Nat, (x ∣ a) ∧ (x ∣ b) -> (x ∣ g))




lemma is_gcd_for_euclid (a b q g: Nat):
  is_gcd b (a - q * b) g -> is_gcd a b g :=
begin
-- (1) replace by definition
repeat {rw is_gcd},

-- (2) add to hypothesis
intro H,
cases H with H1 hrest,
cases hrest with H2 H3,

-- (3) break goal into 3
repeat {apply and.intro},

-- Goal 1: d | a
  cases H2 with k1 eq1,
  cases H1 with k2 eq2,
  use k1 + k2 * q,
  rw mul_dist_add,
  rw← eq1,
  rw mul_assoc,
  rw← eq2,
  rw mul_comm,
  rw sub_assoc,
  rw sub_eq,
  rw sub_zero,

-- Goal 2: d | b
  exact H1,

-- Goal 3: ∀ (x : Nat), (x ∣ a) ∧ (x ∣ b) → (x ∣ d)
  intro x1,
  -- (4) assume (x1 ∣ a) ∧ (x1 ∣ b)
  intro G3H,
  cases G3H with G3H1 G3H2,

  -- (5) use x1 in
  -- ∀ (x : Nat), x ∣ b ∧ x ∣ a - q * b → x ∣ g,
  specialize H3 x1,

  -- (6) turn (x1 ∣ g) into (x1 ∣ b) ∧ (x1 ∣ a - q * b)
  apply H3,
  repeat {apply and.intro},

    -- Goal 3.1: (x1 ∣ b)
    exact G3H2,

    -- Goal 3.2: (x1 ∣ a - q * b)
    cases G3H1 with k3 eq3,
    cases G3H2 with k4 eq4,
    use k3 - q * k4,
    rw mul_dist_sub,
    rw← eq3,
    rw mul_comm q k4,
    rw mul_assoc,
    rw← eq4,
    rw mul_comm,
end

end sb

open nat

def gcd : nat → nat → nat
| 0        a := a
| (succ b) a := have a % succ b < succ b, from mod_lt a (succ_pos b),
                gcd (a % succ b) (succ b)
