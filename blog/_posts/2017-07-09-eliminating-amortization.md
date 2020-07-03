---
layout: post
title: "Eliminating Amortization"
tags: [data structures, ocaml, Purely Funcional Data Structures]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2017-07-09-eliminating-amortization/2344_07_cover.jpg" alt="Book" />
</figure>


In this chapter Okasaki presents techniques to convert persistent data structures with amortized bounds to worst-case bounds. A few reasons to want worst-case guarantees include:


* Real-time systems, where an expensive operation can cause the system to miss a hard deadline, even if it's efficient when amortized over time.
* Parallel systems, in which we want avoid one processor to take more time than the other if it happens to execute the expensive operation.


The intuition is to make the theoretical amortized analysis part of the actual implementation. In the amortized analysis, we saw either through the *Banker's method* or the *Physicist method* the idea of paying debt ahead of time so by the time an expensive operation takes place, we could show it's cost had already been distributed throughout previous operations.

To eliminate the amortization, we can literally pay off these costs when running the algorithm. We do it through a structure called **schedule**, which contains the unevaluated operations on the data structure. Whenever we perform an operation, we evaluate one or more items of the schedule. Due to memoization, by the time we actually need the evaluated structure, it will be evaluated.

Often the amortized analysis can dictate how the execution of the schedule is performed. For example, if the analysis tells us to pay off 2 units of debt on every action, that can be translated to executing 2 items from the schedule on every action.

The main challenge in this conversion is to modify the data structure in such a way it can be partially executed.

We'll discuss an example using queues and then binomial heaps.

### Real-time Queues

For the queue structure, the costly operation is the rotation that takes place when we need to combine the rear with the front. It's a monolithic operation, so we need to change that.

Let's start by defining the structure. It's similar to the stream queue, except that we don't need the rear to be a stream and we have a `schedule` field, which is also a stream. The idea is that whenever a rotation happens, it will be "scheduled" to be executed little by little.

{% highlight ocaml %}
type 'a realTimeQueue = {
  front: 'a stream;
  rear: 'a list;
  schedule: 'a stream
}
{% endhighlight %}

The `rotation()` function is the most complicated part of the structure. The invariant is that we only call this function when `|rear| = |front| + 1`.

We construct a stream with the elements of rear reversed, `newSchedule` and on the return step of the recursion we append the elements of `front` to that stream.

Note that this is performed lazily, so a call to rotate only executes one step.

{% highlight ocaml %}
(*
  Rotate the queue to generate a new front of the queue by reversing the
  rear and appending it. Lazily evaluated.
*)
let rec rotate ({ front; rear; schedule }: 'a realTimeQueue): 'a stream =
  match rear with
     (*
       This should never happen with a valid queue because rotate is only called
       when |rear| > |front|
      *)
    | [] -> raise Empty_queue
    | last_elem :: rest_rear ->
      let newSchedule = Stream.insert last_elem schedule in
      match front with
        (*
          At this point, newSchedule contains the rear of the queue reversed
        *)
        | lazy Nil -> newSchedule
        | lazy (StreamCell (first_elem, rest_front)) ->
          lazy (
            let newFront = rotate {
              front = rest_front;
              rear = rest_rear;
              schedule = newSchedule
            } in
            StreamCell (first_elem, newFront)
          )
;;

{% endhighlight %}

Now we have the execution of the schedule. It serves two purposes. One is to execute an item from the schedule and the other is to trigger a rotation when the is schedule empty.

The schedule being empty is a proxy to when `|rear| = |front| + 1`. Why? Because when the schedule is populated, it has the same size of front (they're both assigned the same stream), and rear is empty. Whenever we insert an element, increasing the size of rear by one, or remove an element, reducing the size of front by one, we decrease the difference between `|front| - |rear|` by 1, and so the size of the schedule is decreased by 1.

Implementation-wise, maybe it would be more clear if we checked for the empty schedule outside and only called `exec()` is it was not empty.

{% highlight ocaml %}
(*
 * exec evaluates the first element of the schedule stream. Because of memoization, this means that
 * whenever we evaluate 'front',  we guarantee that all operations are already memoized.
 *)
let exec ({ front; rear; schedule }: 'a realTimeQueue) =
  match schedule with
    | lazy (StreamCell (_, rest_schedule)) ->
      { front; rear; schedule = rest_schedule }
    (* Due to invariants, this means that |rear| > |front|  *)
    | lazy Nil ->
      (* Now newFront = front @ (rev rear) *)
      let newFront = rotate {front; rear; schedule = Stream.empty} in
      {front = newFront; rear = []; schedule = newFront}
;;

{% endhighlight %}

With these in place, the usual operations for queue are straightforward. The ones that mutate the queue, `push()` and `pop()`, need to make sure to call `exec()`.

{% highlight ocaml %}
let push
  (elem: 'a)
  ({ front; rear; schedule }: 'a realTimeQueue):
('a realTimeQueue) =
  exec { front; rear = elem :: rear ; schedule }
;;

let pop ({ front; rear; schedule }: 'a realTimeQueue): 'a realTimeQueue =
  match front with
    | lazy Nil -> raise Empty_queue
    | lazy (StreamCell (_, rest_front)) ->
      exec { front = rest_front ; rear ; schedule }
;;

let peek ({ front; rear; schedule }: 'a realTimeQueue): 'a =
    match front with
      | lazy Nil -> raise Empty_queue
      | lazy (StreamCell (first_elem, _)) -> first_elem
;;

{% endhighlight %}

### Scheduled Binomial Heaps

We discussed Binomial Heaps in a [previous post]({{site.url}}/blog/2016/10/23/persistent-data-structures.html). The idea is that a binomial heap is a list of binomial trees, an a binomial tree is defined recursively based on it's rank.

The heap is represented by a binary number (as a list of digits). If the k-th digit of the number is 1, it indicates the existence of a binomial tree of rank k (containing 2^(k-1)). A heap with `n` elements, has a unique representation, which is the binary representation of `n`.

For example, if `n = 10`, then the binary representation of the heap is `1010`, meaning it contains a binomial tree of rank 2 (2 elements), and one with rank 4 (8 elements), holding 10 elements in total.

Inserting an element into the heap is analogous to incrementing a binary number by 1. We first create a binomial tree with rank 0 containing that element, and try to insert it into the beginning of the digits list. If the position we want to insert is occupied, we need to merge the trees, to generate a tree with a higher rank, which is analogous to the carry over operation when adding two binary numbers.

The schedule is a list of all the numbers generated when any operation is performed.

The structure for the heap is then the following:

{% highlight ocaml %}
(*
  Binomial tree corresponding to a digit in the heap.
*)
type tree = Node of Element.t * tree list;;
type tv = Element.t
(*
  A heap with n elements can be associated to the binary representation of
  n. The 0's correspond to no tree, while the 1s in position i correspond to
  trees with size 2^i
*)
type digit = Zero | One of tree;;
type schedule = digit stream list;;
type heap = {
  digits: digit stream;
  schedule: schedule;
};;

{% endhighlight %}

As we discussed above, the insertion is analogous to incrementing the binary number. But because the digits are a stream, we need to deal with lazy evaluation:

{% highlight ocaml %}
(*
  Links two trees of same rank r. The resulting tree has rank r + 1
*)
let link (tree1: tree) (tree2: tree) = match (tree1, tree2) with
  (Node (node1, children1), Node (node2, children2)) ->
    if (Element.compare node1 node2) <= 0
      then Node (node1, tree2 :: children1)
      else Node (node2, tree1 :: children2)
;;

(*
  Insert a tree into a stream of digits. This assumes that
  the rank of the tree being inserted has the rank corresponding to the
  position of the current digits.

  This is analogous to the carrying over operation of adding a 1 to a binary
  number. For example, if we are to add 1 to 1011, then we'll have

  101(11) -> match One, link -> 10(11)0
  10(11)0 -> match One, link -> 1(1)00
  1(1)00 -> match Zero -> 1100
*)
let rec insertTree (tree: tree) (digits: digit stream): digit stream =
  match digits with
    | lazy Nil -> Stream.empty |> Stream.insert (One tree)
    | lazy (StreamCell (firstDigit, rest)) -> match firstDigit with
      | Zero -> Stream.insert (One tree) rest
      | One firstTree ->
        lazy (StreamCell (Zero, (insertTree (link tree firstTree) rest)))
;;
{% endhighlight %}

Executing the schedule consists in evaluating one digit from the first number on the schedule. The key is to avoid evaluating the part of the number that already has been evaluated. It's possible to show this happens when we find the first digit one. The intuition is that the trailing zeros from the current number were 1's before the increment, so there was a mutation (linking) while the remaining digits were not modified by that operation.

For example, if we have the number `101011`, an increment causes it to become `101100`. The two trailing zeros in `101100` correspond to a linking of the binomial tree.

{% highlight ocaml %}
(*
  Execute (evaluate) the first item of the schedule. If the first digit of
  the item is Zero, we re-schedule the rest of the digits. If it's a One, it
  means that the remaining digits have been executed, so we just discard it.
*)
let execSchedule
  (schedule: digit stream list)
: digit stream list = match schedule with
  | [] -> []
  | firstItem :: rest -> match firstItem with
    | lazy (StreamCell (Zero, job)) -> job :: rest
    | _ -> rest
;;
{% endhighlight %}

Finally, inserting a new element consists in creating a binomial tree of ranking 0, insert it, and then execute the schedule.

{% highlight ocaml %}
(*
  Inserts an element in the heap.
*)
let insert (elem: tv) (heap: heap): heap =
  match heap with {digits; schedule} ->
    let newDigits = insertTree (Node (elem, [])) digits in
    let newSchedule = execSchedule (newDigits :: schedule) in
    {digits = newDigits; schedule = newSchedule}
;;

{% endhighlight %}

The full code is available on [github](https://github.com/kunigami/ocaml-data-structures/blob/master/binomial_heap/scheduled_binomial_heap.ml).

### Lessons learned

Because we now have several different implementations of queues, I wanted to implement tests that were applicable to any queue implementing an "interface".

One way to do that is to put the interface in a separate module, [IQueue.ml](https://github.com/kunigami/ocaml-data-structures/blob/master/queue/IQueue.ml) and have each implementation require it as their module type:

{% highlight ocaml %}
open IQueue;;

module Queue_stream: IQueue =
  struct
    ...
  end
;;
{% endhighlight %}

To make the test work with any module implementing the `IQueue` interface, we can use a functor (module transformation) and

{% highlight ocaml %}
(* TestBase.ml *)
module MakeTest(Queue: IQueue) =
  struct
    let testSingleInsertion text_ctx =
      let result = Queue.push 10 Queue.newEmpty in
      let resultAsList = Queue.toList result in
      assert_equal
        ~msg:"Should insert one element properly"
        resultAsList
        [10]
    ;;
    
    ...
    let run = run_test_tt_main suite
  end
;;

(* RealTimeQueueTest.ml *)
open RealTimeQueue;;
open TestBase;;

module RealTimeQueueTest = TestBase.MakeTest(RealTimeQueue);;

let () =
  RealTimeQueueTest.run
;;

{% endhighlight %}

Other things I've learned were [matching lazy patterns](https://github.com/yallop/ocaml-patterns/wiki/Lazy-patterns). Matching a lazily evaluated variable with the keyword `lazy` forces the evaluation, so we can use a cleaner syntax, for example:

{% highlight ocaml %}
(* Before *)
  | value ->
    let forcedValue = Lazy.force value in match forcedValue with ...

(* After *)
  | lazy forcedValue -> match forcedValue with ...
{% endhighlight %}

Another syntactic construct that helps with legibility is the record. The examples in Okazaki's book use tuples for most of composed structs, but I prefer the more verbose and explicit records.

{% highlight ocaml %}
(* Tuple *)
type 'a realTimeQueue = 'a stream * 'a list * 'a stream

(* Record *)
type 'a realTimeQueue = {
  front: 'a stream;
  rear: 'a list;
  schedule: 'a stream
}
{% endhighlight %}

Finally, another lesson learned is that adding an insertion operation to the `Stream` API is likely wrong. The idea of the stream is that is avoids evaluation of its tail, so the whole block has to lazily evaluated.

For example, in the queue implementation, in the first block, we will evaluate all the rotation and then make the result lazy, which is not what we want.

{% highlight standard ml %}
(* Incorrect Stream.insert() version *)
let newFront = rotate {
  front = rest_front;
  rear = rest_rear;
  schedule = newSchedule
} in
Stream.insert first_elem newFront (* ==> lazy StreamCell (first_elem, newFront) *)

(* Correct version *)
lazy (
  let newFront = rotate {
    front = rest_front;
    rear = rest_rear;
    schedule = newSchedule
  } in
  StreamCell (first_elem, newFront)
)

{% endhighlight %}

### Conclusion

Eliminating evaluation is a very interesting technique, but it has limited application is practice. It complicates the implementation and, as I learned, it can be tricky to spot bugs (for example, the one in which we're evaluating the `rotate()` function) that would only show up if we profile the data structure.