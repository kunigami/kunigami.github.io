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



def is_gcd (a b g: Nat) : Prop :=
  (g ∣ a) ∧ (g ∣ b) ∧ (∀ x: Nat, (x ∣ a) ∧ (x ∣ b) -> (x ∣ g))




lemma is_gcd_for_euclid (a b d q: Nat): is_gcd b (a - q * b) d -> is_gcd a b d :=
begin
-- lhs
rw is_gcd,
-- rhs
rw is_gcd,
intro h1,
cases h1 with h1b hrest,
cases hrest with h1a h1g,

repeat {apply and.intro},

-- Goal 1: d | a
cases h1a with k1 eq1,
cases h1b with k2 eq2,
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
exact h1b,

-- Goal 3: ∀ (x : Nat), (x ∣ a) ∧ (x ∣ b) → (x ∣ d)
intro x2,
intro h2, -- assume (x ∣ a) ∧ (x ∣ b)
cases h2 with h2a h2b,

-- wolo means x2 | (a - q * b)
--have wolo := x2 * (k2 - q * k1) = a - q * b,


specialize h1g x2,
-- use h1g with [h2a] + [wolo] to arrive at x2 | d
apply h1g,
repeat {apply and.intro},
exact h2b,



cases h2a with k1 eq3,
cases h2b with k2 eq4,
use k1 - q * k2,
rw mul_dist_sub,
rw← eq3,
rw mul_comm q k2,
rw mul_assoc,
rw← eq4,
rw mul_comm,


end

def euclid : Nat → Nat → Nat
  |

end sb
