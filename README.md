VPlan
=====

[![Build Status](https://travis-ci.org/bennofs/vplan.png)](https://travis-ci.org/bennofs/vplan)

A generic library for building schedules (timetables).


Introduction
------------
Warning: *This is work-in-progress. The API may still change. I don't care about backwards compatibility, so use at your
own risk!*

Note: *An understanding of [lens](http://lens.github.io/) is required in order to use this package. You should also
be familar with the standard typeclasses (Functor, Applicative, Monad, Foldable, Traversable).*

This package provides a datastructure for building flexible timetables in haskell. It aims to be extensible, but also
provides a lot of features out of the box.

Schedules (or Timetables) are built with so-called 'Modifiers'. A modifier is just a data type that represents an
operation on a schedule or a generator of values. Examples of modifiers are:
  -   Empty    : doesn't contain a value for any index in the schedule. 
  -   Constant : generates a constant value a, for every index in the schedule.
  -   Limit    : Limits the input range of another modifier, so that it doesn't contain any values at indices outside 
                 of the start-end range.
  -   ...

Building up and indexing schedules
----------------------------------
So how do you use these modifiers? First, we have to import the VPlan library:

    >>> import Data.VPlan
    >>> import Control.Lens

You also need to import lens, as it provides most of the functions and type classes for VPlan.

The high-level interface is exported from Data.VPlan.Combinators. It contains various functions for building schedules, for example
`empty` :


    >>> :t empty
    empty :: Supported Empty s => s i v

This says that we can create an empty schedule (s) for indices of type (i) which contains values of type (v), but only if the Empty generator
is supported by that schedule. A schedule can be made with the Schedule data type, which takes a list of modifiers and the index/value type.
So, we can define a schedule that supports the Empty, Combine, Const, Limit and Repeat modifiers, with Int indices and containing String values:

    >>> :set -XTypeOperators
    >>> type CustomSchedule = Schedule (Empty :><: Combine :><: Const :><: Limit :><: Repeat) Int String

Note that we had to enable the TypeOperators extension in order to use the (:><:) operator provided by VPlan. There is also an USchedule type alias
provided by VPlan that constructs a schedule supporting most modifiers. But back to our example, we can create an empty schedule 
of our type now:

    >>> let s = empty :: CustomSchedule

Indexing is done via the Ixed instance from lens:

    >>> s ^? ix 1      -- Try to get the value at index 1 in the schedule
    Nothing

There is also an instance of Contains, which allows you to check if the schedule has some key (i.e. it's value is not Nothing)

    >>> s ^. contains 1
    False

Lets build a more interresting schedule: A schedule that contains a value only at the given key.

    >>> let t = eq 3 (single "Hello world") :: CustomSchedule   -- Only contains "Hello world" at the index 3, nothing else.
    >>> t ^? ix 3                                               -- Lookup the value at index 3
    Just "Hello world"
    >>> t ^? ix 4                                               -- At any other index, we only get Nothing.
    Nothing

There are a few new things in that example. The first is `single`, which is just a function returning a schedule that has
the given value at all indices. But our value should only be located at the index 3, so we need to narrow our schedule
to only contain "Hello world" at the index *eq*ual to 3, using `eq`.

But how can a schedule have mutiple values at different indices? There is one combinator we haven't talked about yet, with the name (-||-).
With it, we can *mix* multiple schedules into one. So if we have one schedule with a value at index 3, and another one with a value
at the index 4, the result of combining these schedules using (-||-) will have a value at both of those indices. A little example
to illustrate that:

    >>> let u = eq 4 (single "Another value") :: CustomSchedule
    >>> let tu = t -||- u
    >>> s ^? ix 4
    Just "Another value"
    >>> s ^? ix 3
    Just "Hello world"

But that still isn't that interresting. After all, we can do that with just a Data.Map too. But the next example shows something
we cannot do easily with maps: Repeating.

    >>> let tu' = every 4 tu   -- Repeat the schedule every 4 items
    >>> tu' ^? ix 3 -- Just as before
    Just "Hello world"
    >>> tu' ^? ix 7 -- This is new
    Just "Hello world"

You can find more combinators in the Data.VPlan.Combinators module, they are documented using haddock.


Known shortcomings / Bugs
-------------------------
 - The type inference pretty bad
 - Linked to the above: The error messages can be VERY dense
