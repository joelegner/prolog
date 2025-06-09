% planning.pl
/*
This contains thoughts on planning, mostly based on a book.

Planning considers a finite set of _objects_.
The _objects_ are usually few.
The _objects_ form a closed _world_.
The _world_ is described by _facts_.
_Facts_ are relationships between _objects_.
A _state_ is a set of _facts_.
Each _state_ represents the _world_ at a moment.
Actions change the _state_.
The _actions_ form a finite set.
The _actions_ are usually few.
Planning uses simple _actions_.
Some _actions_ delete _facts_.
Some _actions_ add _facts_.
Added _facts_ are called _goals_.
Goals describe desired states.
A final _state_ contains the _goals_.
Planning ends in a final _state_.
Planning starts from an initial _state_.
Planning finds a sequence of _actions_.
The _actions_ move the _world_ from the initial _state_ to the final _state_.

Key terms include:

**Action**: A simple operation that transforms one **State** of the **World** into another by adding or deleting **Facts** about **Objects**.  

**Fact**: A **Relationship** that holds between **Objects** and helps define the current **State** of the **World**.  

**Final State**: The set of **Facts** that describes the **World** after the **Sequence** of planned **Actions** has been completed and **Goals** have been achieved.  

**Goal**: A **Fact** that must hold in the **Final State** of the **World** and is achieved by performing specific **Actions**.  

**Initial State**: The set of **Facts** that describes the **World** before any **Actions** have been taken.  

**Object**: An element of a closed **World** to which **Actions** apply and between which **Relationships**, or **Facts**, are defined.  

**Planning**: The process of finding a **Sequence** of **Actions** that transforms an **Initial State** of the **World** into a **Final State** where certain **Goal** **Facts** hold.  

**Relationship**: A connection between **Objects** that, when expressed as a **Fact**, describes part of the **State** of the **World**.  

**Sequence**: An ordered list of **Actions** that, when applied in turn, transforms the **Initial State** of the **World** into the **Final State**.  

**Set**: A finite collection of items, such as **Objects**, **Actions**, or **Facts**, used to define the components of the **World** and the process of **Planning**.  

**State**: A complete set of **Facts** that describes the **World** at a specific moment.  

**World**: A closed set of **Objects** whose **State** is fully described by the **Facts** that hold among those **Objects**.

We need to describe the relationships between objects in the world.

What objects exist in this world?

- Three cubes: a, b, and c.
- One floor.

How do they relate in our world?

- Cubes may be stacked on cubes.
- Cubes may be stacked on the floor.
- Any stack of cubes must be ultimately on the floor.
- A cube is "clear" if there is no cube on top of it.
- A cube cannot sit on itself.

What actions are allowed in our world?

- Move a single clear block from the supporting block to the floor.
- Move a single clear block from the floor to another clear block.
- Move a single clear block from the supporting block to a different block.

How is a state of the world described?

A state is a conjunction of facts:

1. For each block (a, b, c), tell what it is on.
2. Identify each block that is "clear". 

It would be helpful here to look at page 225 of the Prolog_for_Programmers_neat.pdf file.

What is the initial state of the world?

on(a, floor).
on(b, floor).
on(c, a).
clear(b).
clear(c).

What is the desired end state of the world?

on(a, b).
on(b, floor).
on(c, a).
clear(c).
*/
