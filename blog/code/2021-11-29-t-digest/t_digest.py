from math import pi, asin

from typing import List

class Bin:
    avg: float
    size: int

    def __init__(self, _avg: float, _size: int = 1):
        self.avg = _avg
        self.size = _size

    # Override the '+' operator to implement merging two bins
    def __add__(self, other: 'Bin') -> 'Bin':
        weight = self.avg * self.size + other.avg * other.size
        size = self.size + other.size
        return Bin(_avg=weight/size, _size=size)


class TDigest:
    bins: List[Bin]
    delta: int

    def __init__(self, bins: List[Bin] = None, delta: int = 10):
        self.delta = delta
        self.bins = self._compress(bins) if bins is not None else []

    def __get_elements_count(self):
        return sum([b.size for b in self.bins])

    @staticmethod
    def from_list(xs: List[Bin]) -> 'TDigest':
        bins = [Bin(x) for x in xs]
        return TDigest(bins)

    def _get_potential(self, alpha: float):
        return self.delta * asin(2*alpha - 1) / (2*pi)

    # Add one element by converting it to a single-element t-digest then
    # concatenating with this one.
    def append(self, value: float):
        tdigest_to_add = TDigest(Bin(value))
        self += tdigest_to_add

    # Override the '+=' operator to implement merging a t-digests onto
    # the current one
    def __iadd__(self, other: 'TDigest') -> 'TDigest':
        merged_bins = self._merge_bins(self.bins, other.bins)
        new_bins = self._compress(merge_bins)
        self.bins = new_bins

    def _merge_bins(xs: List[Bin], ys: List[Bin]) -> List[Bin]:
        merged = []
        i, j = 0
        while i < len(xs) or j < len(ys):
            if j >= len(ys) or xs[i].avg <= ys[j].avg:
                merged.append(xs[i])
                i += 1
            elif i >= len(xs) or xs[i].avg > ys[j].avg:
                merged.append(ys[j])
                j +=  1
        return merged

    def _compress(self, xs: List[Bin]) -> List[Bin]:
        if len(xs) == 0:
            return xs

        n = sum([x.size for x in xs])
        ys = [xs[0]]
        # lowest potential of the current
        # merged bin ys[-1]
        min_potential = self._get_potential(0)
        total = xs[0].size

        for i in range(1, len(xs)):
            x = xs[i]
            next_alpha = 1.0 * (total + x.size) / n

            if self._get_potential(next_alpha) - min_potential <= 1:
                ys[-1] = ys[-1] + x
            else:
                ys.append(x)
                min_potential = self._get_potential(1.0 * total / n)

            total += x.size

        return ys

    def get_quantile(self, qid: float) -> float:
        bins = self.bins

        # if the elements were sorted, idx would represent the
        # index in the array corresponding to the quantile index qid
        idx = qid * self._get_elements_count()

        max_idx = bins[0].size / 2

        # idx is on the first half of the first bin
        if idx < max_idx:
            return bins[0].avg

        for i in range(len(bins) - 1):
            b = bins[i]
            b_next = bins[i + 1]

            interval_length = (b.size + b_next.size) / 2
            # target index is in between b and b_next. interpolate
            if idx <= max_idx + interval_length:
                alpha = (idx - max_idx) / interval_length
                return b.avg * (1 - alpha) + b_next.avg * alpha

            max_idx += interval_length

        # idx is on the second half of the last bin
        return bins[-1].mean


# -----------------------------------------------------------------------------
# Tests
# -----------------------------------------------------------------------------

if __name__ == '__main__':
    t = TDigest.from_list(list(range(10)))
    for i in range(10):
        print(t.get_quantile(i/10))

    # TODO:
    # gaussian
    # uniform
    # geometric
    # real world (house prices)

    # online version of those
