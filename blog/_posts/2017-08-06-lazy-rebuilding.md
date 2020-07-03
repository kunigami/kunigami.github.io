---
layout: post
title: "Lazy Rebuilding"
tags: [data structures, ocaml]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2017-08-06-lazy-rebuilding/2344_07_cover.jpg" alt="Book" />
</figure>

In this chapter Okasaki starts with a common technique data structures use to achieve efficient complexity times. A classic example are balanced binary search trees, which, on one hand, need to be balanced to avoid degenerated cases (think of a binary tree which can degenerate to a path), but on the other hand, balancing is usually too expensive to perform at every operation.

The tradeoff is to allow a certain degree of "imbalance", such that the big-O complexity of operations does not degenerate, and to let us perform re-balances only every so often, in a way that leads to an efficient amortized complexity. Structures such as AVL trees and Red-black trees make use of this technique.

The general technique is named **batched rebuilding**. The main issue with it though is that we saw that [amortized analysis does not play well persistent data structures]({{site.url}}/blog/2017/03/19/amortization-and-persistence-via-lazy-evaluation.html).

To address that problem, a technique was developed by Mark Overmars, called **global rebuilding**.
### Global Rebuilding
The basic idea is to keep two copies of a structure in parallel, one of which we perform incremental updates (write) and the other is used for read operations. After a few operations, we copy the write version into the read-only version. The idea is that the incremental updates are cheap, but might put the structure in a state that is not suitable for reading. This allows for distributing the costs across operations such that each operation has worst case efficient bounds.

The major downside of this technique is the complexity and corner cases, because we also need to account for changes to the structure that renders the write copy of the structure invalid.

We'll now describe yet another implementation of queues that use this technique, developed by Robert Hood and Robert Melville. Similar to other queue implementations, this structure maintains a front and rear lists, the latter in reverse order. It also has the invariant that the rear list cannot be larger than the front.

When that happens we must perform a rotation, which consists in reversing the rear queue and appending it to the end of the front queue. We cannot perform this operation piecemeal, because we only have efficient access to the first element of a list. The operation we *can* do partially is concatenating the reverse of a list to the beginning of another list, that is, given `xs` and `ys`, we can do `~xs + ys`, where `~` represents the reverse of a list. Note we can also reverse a list, that is `xs` to `~xs` piecemeal by having `ys = []`.

Now, to achieve our goal which is `xs + ~ys`, we reverse both `xs` and `ys`, then concatenate the reverse of `~xs` to `~ys`:

1) Start with `xs` and `ys`
2) Reverse both: `~xs` and `~ys`
3) Then `(~(~xs)) + ~ys` which is `xs + ~ys`

We can perform these operations one step at a time by using a "state machine", we first start with a state `Reversing(xs, ys)` which will reverse `xs` and `ys` at the same time into `~xs` and `~ys`, at which point we switch to the state `Appending(~xs, ~yx)` which will concatenate the `xs` into `~ys`, so then we get to `Done(xs + ~ys)`.

The problem of keeping a separate state is that it might not be accurate by the time it's done. In particular, if we remove an element from the front, the `Appending` step can be shortcut. We can keep a count of how many of the elements in `~xs` of `Appending` are still present. If, by the time we're appending the number of present elements goes to 0, the last elements of `~xs` (that is, the first elements of `xs`) have been removed, so they do not need to be transferred to `~ys`.

A possible implementation of the states is:

{% highlight ocaml %}
type 'a rotationState =
  Idle
  | Reversing of {
    validCount: int;
    front: 'a list;
    rear: 'a list;
    frontReversed: 'a list;
    rearReversed: 'a list;
  }
  | Appending of {
    validCount: int;
    front: 'a list;
    rear: 'a list;
  }
  | Done of 'a list
;;

{% endhighlight %}

The `Idle` is just a placeholder to make sure the state machine can keep going to the next state even when we're not currently performing a rotation. The state transition definition is given by:

{% highlight ocaml %}
let nextState rotationStep =
  match rotationStep with
  | Reversing ({
      validCount;
      front = firstElem :: restFront;
      frontReversed;
      rear = lastElem :: restRear;
      rearReversed;
    }) -> Reversing ({
      validCount = validCount + 1;
      front = restFront;
      frontReversed = firstElem :: frontReversed;
      rear = restRear;
      rearReversed = lastElem :: rearReversed;
    })
  | Reversing ({
      validCount;
      front = [];
      frontReversed;
      (* Invariant: restRear must be empty *)
      rear = lastElem :: restRear;
      rearReversed;
    }) -> Appending ({
      validCount;
      front = frontReversed;
      rear = lastElem :: rearReversed;
    })
  | Appending ({validCount = 0; front; rear}) -> Done rear
  (* Transfer one element of front to rear *)
  | Appending ({validCount; front = elem :: restFront; rear}) ->
      Appending ({
        validCount = validCount - 1;
        front = restFront;
        rear = elem :: rear;
      })
  | rotationStep -> rotationStep (* No-op *)
;;
{% endhighlight %}

One important detail here is that `Appending ({validCount = 0; front; rear})` must appear before the other matching rule for `Appending`, because the other one includes this.

When we remove an element from the front of the queue we need to update the number of valid elements in the front of the rotation state:

{% highlight ocaml %}
let invalidate rotationStep = match rotationStep with
  | Reversing ({validCount; front; frontReversed; rear; rearReversed})
    -> Reversing ({
      validCount = validCount - 1;
      front;
      frontReversed;
      rear;
      rearReversed;
    })
  | Appending ({validCount = 0; rear = lastElem :: restRear})
    -> Done restRear
  | Appending ({validCount; front; rear}) ->
      Appending ({validCount = validCount - 1; front; rear})
  | rotationStep -> rotationStep
;;

{% endhighlight %}

Similar to the [Real-time queues]({{site.url}}/blog/2017/07/09/eliminating-amortization.html), we can guarantee constant time worst case for all the operations if we execute the state machine twice for each operation. The `check()` function verifies if we need a rotation and also executes the `nextStep()` function twice.

{% highlight ocaml %}
let processStateFromQueue {frontSize; front; rearSize; rear; rotation} =
  let newState = (nextState (nextState rotation)) in match newState with
    | Done newFront ->
      {frontSize; front = newFront; rearSize; rear; rotation = Idle}
    | _ -> {frontSize; front; rearSize; rear; rotation = newState}
;;

let check queue = match queue with
  {frontSize; front; rearSize; rear; rotation} ->
    if rearSize <= frontSize then processStateFromQueue queue
    else
      (* Initiate the rotation process. *)
      let newState = Reversing ({
        validCount = 0;
        front;
        frontReversed = [];
        rear;
        rearReversed = [];
      })
      in processStateFromQueue {
        frontSize = frontSize + rearSize;
        front;
        rearSize = 0;
        rear = [];
        rotation = newState;
      }
;;

{% endhighlight %}

The other operations are then straightforward. The only thing is that `pop()` must call the `invalidate()` function because it's decreasing the size of `front`:

{% highlight ocaml %}
let push elem {frontSize; front; rearSize; rear; rotation} =
  check {
    frontSize;
    front;
    rearSize = rearSize + 1;
    rear = elem :: rear;
    rotation;
  }
;;

let peek {frontSize; front; rearSize; rear; rotation} = match front with
  | [] -> raise Empty_queue
  | firstElem :: restFront -> firstElem
;;

let pop {frontSize; front; rearSize; rear; rotation} = match front with
  | [] -> raise Empty_queue
  | firstElem :: restFront ->
    let newState = invalidate rotation in
    check {
      frontSize = frontSize - 1;
      front = restFront;
      rearSize;
      rear;
      rotation = newState;
    }
;;

{% endhighlight %}

The full, up-to-date implementation in on [github](https://github.com/kunigami/ocaml-data-structures/blob/master/queue/hoodMevilleQueue.ml).
### Lazy Rebuilding
As we can see, the Hood-Melville implementation is quite involved. Okasaki argues that making use of lazy evaluation and memoization simplifies the implementation. We can indeed see that the implementation of [Real-time queues]({{site.url}}/blog/2017/07/09/eliminating-amortization.html) is much cleaner.

One simplification is that we don't have to sync the write copy with the read copy. We just need to make sure to execute items from the schedule by the time they are needed. Memoization will take care of the rest.

Another advantage in this case is that reversing and concatenating lazily evaluated lists does not require an intermediate reversal of the front, which means that we can remove the front element without the need to invalidate any items in the schedule.

The author provided more examples of lazy rebuilding for deques (double-ended queues) by first presenting an amortized version using the Banker's method and then a lazy rebuilding version. The [Banker's deque](https://github.com/kunigami/ocaml-data-structures/blob/master/deque/bankersDeque.ml) and [Real-time deque](https://github.com/kunigami/ocaml-data-structures/blob/master/deque/realTimeDeque.ml) are also on my github.
### Conclusion
In this chapter the technique behind Real-time queues was formalized. I really enjoy the organization of the book in which the author presents a data structure and from there he extracts a generic pattern of technique that can be applicable to other cases.

When I was studying data structures I don't recall learning about general techniques underlying existing data structures, such as batched rebuilding for AVL trees and Red-black trees. That would have been interesting.