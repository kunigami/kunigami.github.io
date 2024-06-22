import random
import hashlib

class ApproxSet:
    def __init__(self, b=128, l=16):
        self.b = b
        self.mask = 2**b - 1
        self.size = 2**l
        self.arr = [None] * self.size
        # How many entries are set 
        self.used = 0

    def _get_hash_pair(self, value):
        # hash in hexadecimal
        hash_hex = hashlib.sha256(str(value).encode()).hexdigest()
        hash_int = int(hash_hex, 16)
        # Use first l bits for index
        index = (hash_int >> self.b) % self.size
        # Use last b bits for value
        value = hash_int & ((1 << self.b) - 1)
        return (index, value)

    def add(self, value):
        (index, value) = self._get_hash_pair(value)
        if self.arr[index] is None:
            self.used = self.used + 1
            # Replacement strategy
            self.arr[index] = value

    def utilization(self):
        return float(self.used)/float(self.size)

    def __contains__(self, value):
        (index, value) = self._get_hash_pair(value)
        return self.arr[index] == value

def random_value():
    return random.randint(1, 100000)  

def evaluate(s, inserted, n):
    false_negative = 0
    false_positive = 0
    outset = 0
    inset = 0
    for i in range(n):
        value = random_value()
        if value in inserted:
            inset += 1
            if value not in s:
                false_negative += 1
        else:
            outset += 1
            if value in s:
                false_positive += 1
    
    inset = max(inset, 1)
    print(f"{s.utilization():.2f}, {(false_negative / inset):.2f}, {(false_positive / outset):.2f}")

n = 1000000
inserted = set()
s = ApproxSet()
step = 0
for i in range(n):
    value = random_value()

    s.add(str(value))
    inserted.add(value)

    if s.utilization() > step:
        step += 0.1
        evaluate(s, inserted, n)

    if step > 0.7:
        break
