
VPlan
=====

[![Build Status](https://travis-ci.org/bennofs/vplan-utils.png?branch=master)](https://travis-ci.org/bennofs/vplan-utils)

A generic library for building schedules (timetables).


Introduction
------------
Warning: *This is work-in-progress. The API may still change. I don't care about backwards compatibility, so use at your
own risk!*

This package provides a datastructure for building flexible timetables in haskell. It aims to be extensible, but also
provides a lot of features out of the box.

Schedules (or Timetables) are built with so-called 'Modifiers'. A modifier is just a datatype that stands for an
operation or a generater. Examples of modifiers are:
  -   Empty: doesn't contain a value for any index in the schedule.
  -   Constant a: generates a constant value a, for every index in the schedule.
  -   Limit c bound s: Limits the input range of another modifier s, so it behaves like s only when the index compared to
                       the bound is equal to the condition. For all other indices, it behaves like 'Empty'.
  -   and some more

Modifiers are types of kind * -> *. They get the schedule type as the first argument, so that they themselves can
contain schedules. This leads to a Tree-like structure of types. The Schedule datatype just takes a modifier and passes
itself to that modifier, so we get an infinitely recursive type.


Building up and indexing schedules
----------------------------------
How do you use these modifiers? To get started, import Data.VPlan:

    >>> import Data.VPlan

There are some combinators for building Schedules with the included Modifiers in Data.VPlan.Combinators, which is export
ed by Data.VPlan. The simplest combinator is empty, which just creates an empty schedule:

    >>> :t empty
    empty :: Supported Empty s => s

This tells us that our Schedule type has to support the Empty modifier, and if it does, we can get a schedule with it.
In this example, we're just going to use the Empty modifier as the only modifier in our Schedule type. To index into an
Schedule, you can use the Traversal `ix` from the lens package:

    >>> (empty :: Schedule Int Int Empty) ^? ix 10
    Nothing

Here we're telling to lookup the index 10 in a schedule, and return the first item in a Just, or Nothing if there is no
item. In this case, we get a Nothing because the empty schedule doesn't contain a value at any place.
The type of this empty schedule is Schedule Int Int Empty, which stands for a schedule with indices of type Int,
values of type Int and only one modifier, Empty. There is also a lens to check if there is a value at a given place,
called contains:

    >>> (empty :: Schedule Int Int Empty) ^. contains 10
    False

As we expected, we got False, because there is no value at index 10.
Let's look at another combinator, with the name 'single'. It's type is:

    >>> :t single
    single :: Supported Constant s => IxValue s -> s

This allows use the create schedules that contain the same value at every index, for example 3:

    >>> (single 3 :: Schedule Int Int Constant) ^? ix 5
    Just 3

So far we only had Schedules with one Modifier. But you can't do much with one Modifier, so there is another Modifier
we can use at top-level that combines multiple modifiers. It works like this:

    >>> :set -XTypeOperators
    >>> (single 3 :: Schedule Int Int (Constant :><: Empty :><: Close)) ^? ix 5
    Just 3

First we're enabling the TypeOperators language extension. Then we're creating a Schedule with a single value 3, which
goes from Int to Int and contain either an Empty modifier or a Constant modifier. The Close just marks the end of the
list, but don't forget it.

There is a type alias which chains all modifiers available in this package, called AllModifiers, and a Schedule type
that supports all those modifiers with the name USchedule. We're using this from now on,
so we don't have to write out all of the Modifiers.

With that knowledge, we can look at further combinators. First, there is eq, which takes an index `i` and limits a given
schedule, so that it acts like Empty for all indices that are not equal to `i`. Example:

    >>> (eq 3 (single 4) :: USchedule Int Int) ^? ix 3
    Just 4
    >>> (eq 3 (single 4) :: USchedule Int Int) ^? ix 5
    Nothing

To chain multiple schedules together, with the first one taking higher precendence than the last one, you can use `-||-`:

    >>> let s = eq 3 (single 4) -||- eq 5 (single 1) -||- eq 3 (single 2) -||- eq 6 (single 10) :: USchedule Int Int
    >>> s ^. contains 2
    False
    >>> s ^? ix 5
    Just 1
    >>> s ^.. ix 3
    [4,2]

In the last example, we used `^..` to get all the values at a given index, with earlier values before later values.
You can also move or swap values in a schedule:

    >>> let s1 = move 6 1 s
    >>> s1 ^? ix 1
    Just 10
    >>> s1 ^? ix 6
    Nothing
    >>> let s2 = move 1 3 s1
    >>> s2 ^.. ix 3
    [4,2,10]
    >>> let s3 = swap 3 5 s2
    >>> s3 ^? ix 3
    Just 1
    >>> s3 ^? ix 5
    Just 4

You can find more combinators in the Data.VPlan.Combinators module, they are documented using haddock.

Inspecting schedules
--------------------
Other than indexing and creating schedules, you might also want to _inspect_ schedules, querying there structure
and data. For this purpose, Schedule is an instance of Plated (a class from lens), which allows you to obtain the
immediate children of some modifier. Example:

    >>> :t (single 3 -||- single 4 :: USchedule Int Int) ^.. plate
    [USchedule Int Int]

Because there is no pretty printing for Schedules yet, you can't really try this. But you can get all the values
of some type in a Schedule, by using template from Data.Data.Lens (lens package):

    >>> (single 3 -||- single 4 :: USchedule Int Int) ^.. template :: [Int]
    [3,4]

To learn more about this, look at the documentation of the Control.Lens.Plated and Data.Data.Lens packages. All
Modifiers defined in this package have Typeable and Data instances, so that shoulnd't be a problem.

Defining custom modifiers
-------------------------
Here are the steps you need to do to define your own modifiers:
 -  enable TemplateHaskell, StandaloneDeriving and any extensions GHC wants you to enable
 -  import Data.VPlan.At qualified (in the following steps, we assume the alias A for the import)
 -  make a datatype for your modifier that takes the Schedule type as last argument
 -  call the TH-macro `makeModifier` like this:

        makeModifier ''YourDataType

 -  derive the instances you want using standalone deriving. Recommended: `Eq` and `Data` at least. Example:

        deriving instance (Eq (Index s), Eq a) => Eq (YourDataType a s)

 -  make instances for A.Contains and A.Ixed. Look at the implementations of the included modifiers for example
    definitions.
 -  Use your data type as modifier for the schedule, for example by chaining it onto AllModifiers:

        type YourModifiers = YourModifiers :><: AllModifiers

These steps may sound complicated, but they aren't as complicted as you might think.

TODO
----
 - [ ] Implement Repeat modifier and combinators
 - [ ] Maybe some pretty prining for schedules?
 - [ ] Serialization of schedules?

Known shortcomings / Bugs
-------------------------
 -  The type inference pretty bad
