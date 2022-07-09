---
layout: post
title:  "Fighting with Recursion Schemes"
date:   2021-06-27 23:23:10
image:  "/images/20210627-2/icon.png"
header: "/images/20210627-2/header.jpg"
level: Advanced
categories: Recursion-Schemes Cats-Effect ScalaJS Example
---

## Introduction to the problem 

Recursion schemes is a technique that allows us to abstract **the core behaviour
of a recursive function** over a **recursive data structure** from the recursion itself.

You might be interested in reading this if:

* You want to know what problem Recursion Schemes is solving
* You want to know how to extract recursion from recursive operations
* You want to know about the Scala library **Droste**
* You want to know how to build a game like Rock, Paper, Scissors defining its logic with Recursion Schemes

### First example: List

This is usually explained with a list, because it's one of the simplest data structures
where we use recursion in our day to day.

Let's define a `List[T]` for any type `T` like this:

{% highlight scala %}
sealed trait List[+T]
final case class ::[T](head: T, tail: List[T]) extends List[T]
case object Nil extends List[Nothing]

/*
val example: List[Int] = 1 :: 4 :: 2 :: Nil
┌───┐    ┌───┐    ┌───┐    ┌─────┐
│ 1 ├───►│ 4 ├───►│ 2 ├───►│ Nil │
└───┘    └───┘    └───┘    └─────┘
*/
{% endhighlight %}

So we could define these operations for a `List[Int]`:
{% highlight scala %}
val sum: List[Int] => Int
val filter: (Int => Boolean) => List[Int] => List[Int]
val size: List[Int] => Int
val mkString: List[Int] => String
{% endhighlight %}

In all these cases we traverse the list from start to end, performing some
specific operation.

{% highlight scala %}
def recursion[A](f: Int => A, combine: (A, A) => A, empty: A): List[Int] => A = {
  case head :: tail => f(head) combine recursion(tail)
  case Nil => empty
}
{% endhighlight %}

That is basically:

{% highlight scala %}
trait List[+A] {
  def foldRight[B](z: B)(op: (A, B) => B): B
}
{% endhighlight %}

For example, we could implement `sum` as:

{% highlight scala %}
val sum: List[Int] => Int = _.foldRight(0) { 
  case (item, result) => item + result
}
{% endhighlight %}

### Second example: Binary Tree

Let's explore another example where every node of the data structure can have
more than one child (using a similar implementation to `List`):
{% highlight scala %}
sealed trait BTree[+T]
final case class BNode[T](value: T, left: BTree[T], right: BTree[T]) extends BTree[T]
case object Nil extends BTree[Nothing]

/*
val example: BTree[Int] = 
  BNode(1,
    BNode(4, 
      BNode(2, Nil, Nil),
      BNode(3, 
        BNode(1, Nil, Nil),
        Nil
      )
    ),
    BNode(5, Nil, Nil)
  )

                ┌───┐
          ┌────►│ 2 │
          │     └───┘
        ┌─┴─┐
  ┌────►│ 4 │
  │     └─┬─┘
┌─┴─┐     │     ┌───┐   ┌───┐
│ 1 │     └────►│ 3 ├──►│ 1 │
└─┬─┘           └───┘   └───┘
  │     ┌───┐
  └────►│ 5 │
        └───┘
*/
{% endhighlight %}

We can define the following operations one more time:
{% highlight scala %}
val sum: BTree[Int] => Int
val filter: (Int => Boolean) => BTree[Int] => BTree[Int]
val size: BTree[Int] => Int
val mkString: BTree[Int] => String
{% endhighlight %}

We traverse the tree in a similar way for every operation:
{% highlight scala %}
def recursion[A](combine: (Int, A, A) => A, empty: A): BTree[Int] => A = {
  case BNode(value, left, right) => 
    combine(value, recursion(left), recursion(right))
  case Nil => empty
}
{% endhighlight %}

For example, for `sum`:
{% highlight scala %}
val sum: Btree[Int] => Int = recursion(
  (value, resultLeft, resultRight) => value + resultLeft + resultRight,
  empty = 0
)
{% endhighlight %}

### Conclusion

Then we have:

1. An operation to calculate a result and combine it in every level of the recursion
2. An operation to traverse the specific recursive data structure

The former point is defined by the specific operation we want to do, while the latter
is defined by the data structure. This means that we can separate them.

## Recursion Schemes

### Fold operation

Recursion Schemes allows us to generalise, for example, the fold operation that takes
a `F[A] => A`, for any `F` that's a `Functor` (which knows how to traverse the data structure).

So in this case we don't want to map over the value that the data structure contains, but over 
the valuse that conform the data structure.

In order to make the explanation easier, we can fix the type `T` for `List[T]` to `Int`.

{% highlight scala %}
sealed trait ListInt
final case class ::(head: Int, tail: ListInt) extends ListInt
case object Nil extends ListInt
{% endhighlight %}

And then parametrise the type that represents the recursive type as `T`:
{% highlight scala %}
sealed trait ListIntF[+T]
final case class ::[+T](head: Int, tail: T) extends ListIntF[T]
case object Nil extends ListIntF[Nothing]
implicit class ConsInt[T](t: T) {
  def ::(newHead: Int): ::[T] = new ::(newHead, t)
}
{% endhighlight %}

Now we can try to implement `sum` again, as `F[A] => A` where `F = ListInt[F]`:
{% highlight scala %}
val sum: ListIntF[Int] => Int = {
  case value :: tailResult => value + tailResult
  case Nil                 => 0
}
{% endhighlight %}

But we still need to define a `Functor` that knows how to traverse `ListIntF[F]`:
{% highlight scala %}
implicit val listIntFunctor: Functor[ListIntF] = new Functor[ListIntF] { 
  override def map[A, B](fa: ListIntF[A])(f: A => B): ListIntF[B] = fa match {
    case head :: tail => head :: f(tail)
    case Nil          => Nil
  }
}
{% endhighlight %}

So if we have thi list, for example:
{% highlight scala %}
val list: ListIntF[ListIntF[ListIntF[ListIntF[ListIntF[Nothing]]]]] = 
  1 :: 2 :: 3 :: 4 :: Nil
{% endhighlight %}

We can apply manually this `ListIntF[Int] => Int` in every level of the nested structure:
{% highlight scala %}
val res = 
 sum(list.map(n2 => sum(n2.map(n3 => sum(n3.map(n4 => sum(n4.map(sum))))))))
println(res) // 10: Int
{% endhighlight %}

But obviously we don't want to have to do this for every possible list size.

We could represent the type for a ListIntF of any size with:
{% highlight scala %}
type Fix[F[_]] = F[Fix[F]]
// Fix for Fixed point (function with periodic point = 1)
val list: Fix[ListIntF] = 1 :: 2 :: 3 :: 4 :: Nil
{% endhighlight %}

But it's not possible to define a recursive type in Scala.

### Fixed point

Scala doesn't allow us to define a recursive type to represent F[F[F[...]]], but we can define
a recursive data type:
{% highlight scala %}
case class Fix[F[_]](unfix: F[Fix[F]])
{% endhighlight %}

Doing this we can define a ListIntF like:
{% highlight scala %}
val list: Fix[ListIntF] = 
  Fix(1 :: Fix(2 :: Fix(3 :: Fix(4 :: Fix(Nil: ListIntF[Fix[ListIntF]])))))
  // (Compiler complains about Nil with no annotation because it doesn't
  //  have a parameter type, but it could)
{% endhighlight %}

### Catamorphism (Fold operation)

Now we have everything we need to implement the fold operation, or, as it's called
in Recursion Schemes, catamorphism:
{% highlight scala %}
def cata[F[_], A](algebra: F[A] => A)(implicit F: Functor[F]): Fix[F] => A =
    fix => algebra(fix.unfix.map(cata(algebra)))
{% endhighlight %}

The name given to the operation `F[A] => A` is **Algebra**.

With this abstraction we can implement our sum operation for the entire `ListIntF`:

{% highlight scala %}
val sumList: Fix[ListIntF] => Int = cata(sum)
val list: Fix[ListIntF] = ... // same as earlier
val res = sumList(list)
println(res) // 10: Int
{% endhighlight %}

### Anamorphism (Unfold operation)

We can also perform the opposite operation with the same tools we've built for the previous
exercise. This generalised unfold is called anamorphism, and expects an operation defined as
`A => F[A]`, called **Coalgebra**.

{% highlight scala %}
def ana[F[_], A](coalgebra: A => F[A])(implicit F: Functor[F]): A => Fix[F] =
    a => Fix(coalgebra(a).map(ana(coalgebra)))
{% endhighlight %}

We can create a list of numbers from N to 1 for example:
{% highlight scala %}
val nListCoalgebra: Int => ListIntF[Int] = {
  case i if i > 0 => i :: (i - 1)
  case _           => Nil
}
{% endhighlight %}

And then:
{% highlight scala %}
val buildNList: Int => Fix[ListIntF] = ana(nListCoalgebra)
println(buildNList(3)) // Fix(::(3,Fix(::(2,Fix(::(1,Fix(Nil)))))))
{% endhighlight %}

### Hylomorphism (Fold + Unfold operations)

We can perform fold + unfold with the operations we've implemented already:
{% highlight scala %}
val sumNList: Int => Int = n => sumList(buildNList(n))
println(sumNList(4)) // 10
{% endhighlight %}

But we can implement it so it performs the operations from the Algebra and the Coalgebra at the same time:

{% highlight scala %}
def hylo[F[_] : Functor, A, B](algebra: F[B] => B)(coalgebra: A => F[A]): A => B =
    a => algebra(coalgebra(a) map hylo(algebra)(coalgebra))
{% endhighlight %}

And then:
{% highlight scala %}
val sumNList2: Int => Int = hylo(sum)(nListCoalgebra)
println(sumNList2(4)) // 10
{% endhighlight %}

## Droste

Droste is a Scala library for recursion. It contains these operations and many more.

Example:
{% highlight scala %}
val sumAlgebra: Algebra[ListIntF, Int] = Algebra {
    case head :: tailResult => head + tailResult
    case Nil                => 0
  }
val sizeAlgebra: Algebra[ListIntF, Int] = Algebra {
  case _ :: tailResult => 1 + tailResult
  case Nil                => 0
}
val mkStringAlgebra: Algebra[ListIntF, String] = Algebra {
  case value :: other => value.toString + " :: " + other
  case Nil => "Nil"
}
val nListCoalgebra: Coalgebra[ListIntF, Int] = Coalgebra {
  case n if n > 0 => n :: (n - 1)
  case _          => Nil
}
{% endhighlight %}

We can even combine the results for different operations (`sum`, `size` and `mkString`) traversing the list
only once:
{% highlight scala %}
val doManyThings = scheme.ghylo(
  (sumAlgebra zip sizeAlgebra zip mkStringAlgebra).gather(Gather.cata),
  nListCoalgebra.scatter(Scatter.ana)
)
println(doManyThings(4)) // ((10,4),4 :: 3 :: 2 :: 1 :: Nil)
{% endhighlight %}

## Example 1: Running a Rock, Paper, Scissors game

Let's define the actions the player can perform:
{% highlight scala %}
sealed trait Action
case object Rock extends Action
case object Paper extends Action
case object Scissors extends Action
{% endhighlight %}

And a strategy, or how the player decides the next action based on opponent's previous actions:
{% highlight scala %}
type Strategy = PartialFunction[List[Action], Action]
val strategy: PartialFunction[List[Action], Action] = {
  case Nil                 => Rock  // First move
  case Rock :: Rock :: Nil => Paper // Two Rocks in a row
}
{% endhighlight %}

And the definition of the game:
{% highlight scala %}
case class Player(wins: Int = 0, strategy: Strategy, history: List[Action] = Nil)
case class Game(player1: Player, player2: Player, round: Int = 0)
{% endhighlight %}

We're going to create a new recursive structure that represents the development of the game:
{% highlight scala %}
sealed trait GameF[+T]
case class StepF[T](action1: Action, action2: Action, next: T) extends GameF[T]
case class EndF[T](player1: Player, player2: Player) extends GameF[T]
{% endhighlight %}

And now the game rules as a Coalgebra:
* We calculate what the next move is for each player
* We update the status of the player based on these moves (win count)
* We move to the next round

{% highlight scala %}
val coalgebra: Coalgebra[GameF, Game] = Coalgebra {
  case Game(player1, player2, 3) => EndF(player1, player2)
  case g @ Game(player1, player2, round) =>
    val (nextAction1, nextAction2) =
      (player1.strategy(player2.history), player2.strategy(player1.history))
    StepF(
      nextAction1,
      nextAction2,
      g.copy(
        player1 = updatePlayer(player1, nextAction1, nextAction2),
        player2 = updatePlayer(player2, nextAction2, nextAction1),
        round = round + 1
      )
    )
}
{% endhighlight %}

And now we define an algebra to extract the game result in different ways:
* Returning a string with the winner:
{% highlight scala %}
val algebraResult: Algebra[GameF, String] = Algebra {
  case StepF(_, _, result) => result
  case EndF(player1, player2) =>
    if (player1.wins > player2.wins) "Player1 wins"
    else if (player2.wins > player1.wins) "Player2 wins"
    else "Nobody wins"
  }
{% endhighlight %}

* Returning an IO that will print the result for every round:
{% highlight scala %}
val algebraStepByStep: Algebra[GameF, IO[Unit]] = Algebra {
  case StepF(a1, a2, io) => IO(println(s"Player1: $a1 | Player2: $a2")) *> io
  case EndF(_, _) => IO.unit
}
{% endhighlight %}

And finally, we run the game to return both things:
{% highlight scala %}
val runGame: Game => (IO[Unit], String) = scheme.ghylo(
  (algebraStepByStep zip algebraResult).gather(Gather.cata),
  coalgebra.scatter(Scatter.ana)
)
{% endhighlight %}

## Example 2: Parsing a game strategy

A typical use case for recursion schemes is transforming the representation of
a data structure to/from different formats.

For the previous game, we could model every case for the strategy:
{% highlight scala %}
case Action1 :: Action2 :: Nil => Action
{% endhighlight %}

as
{% highlight scala %}
case class StrategyCase(condition: NonEmptyList[String], action: String)
{% endhighlight %}

And expand it into a nested structure:
{% highlight scala %}
case class StrategyNodeF[+T](
  action: Option[Action],
  onRock: Option[T],
  onScissors: Option[T],
  onPaper: Option[T]
)
{% endhighlight %}

Coalgebra for Strategy, to define how we unfold a List of cases:
{% highlight scala %}
val strategyCoalgebra: Coalgebra[StrategyNodeF, List[StrategyCase]] = Coalgebra { list =>
  val grouped = list.groupBy(_.condition.head)
  val casesFor: Action => Option[List[StrategyCase]] = s =>
    grouped.get(s.toString).map(cases => 
      cases.flatMap(c => 
        NonEmptyList.fromList(c.condition.tail).map(l => c.copy(condition = l)).toList))
  StrategyNodeF(
    grouped.get("Nil").flatMap(c => Action.fromString(c.head.action)),
    casesFor(Rock),
    casesFor(Scissors),
    casesFor(Paper)
  )
}
{% endhighlight %}

If we want to transform the Tree to Strategy:
{% highlight scala %}
val toStrategyAlgebra: Algebra[StrategyNodeF, List[Action] => Option[Action]] = Algebra {
  case StrategyNodeF(action, onRock, onScissors, onPaper) =>
    {
      case Nil => action
      case Rock :: tail => onRock.flatMap(_(tail))
      case Scissors :: tail => onScissors.flatMap(_(tail))
      case Paper :: tail => onPaper.flatMap(_(tail))
    }
}
{% endhighlight %}

If we wannt to serialise the Strategy as JSON to store it:
{% highlight scala %}
val toJsonAlgebra: Algebra[StrategyNodeF, Json] = Algebra {
  case StrategyNodeF(action, onRock, onScissors, onPaper) =>
    Json.fromFields(
      action.map("action" -> _.toString.asJson) ++
        onRock.map("onRock" -> _) ++
        onScissors.map("onScissors" -> _) ++
        onPaper.map("onPaper" -> _)
    )
}
{% endhighlight %}

## Final Example

Using all these concepts I've built a game very similar to Rock, Paper, Scissors, following
these rules:

* It's a battle between two fighters (20 turns max)
* A fighter can:
    * **Rest**: recovers 2 energy points, but the player is vulnerable
    * **Attack**: requires 2 energy points, and the player is invulnerable
    * **Defend**: requires 1 energy point, and the player is invulnerable
    * If you don't have the required points, it will rest by default
* The winner will be the one that deals more damage to the opponent (3 max)

See example [here][game-example].


### And more!

Check droste for more implementations, or read about more morphisms:

* Histomorphism
* Zygomorphism
* Paramorphism
* Apomorphism
* Futumorphism
* Etc.

## Links

* [Droste][droste]
* [Fixed point][fixed-point]
* [Recursion schemes elementary introduction][intro]

[droste]: https://github.com/higherkindness/droste
[fixed-point]: https://en.wikipedia.org/wiki/Fixed_point_(mathematics)
[intro]: https://free.cofree.io/2017/11/13/recursion/
[game-example]: https://algd.github.io/recursion-schemes-talk/shared.html