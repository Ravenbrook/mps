@make [article]
@device [dover]

@begin [titlepage]
@begin [titlebox]
@majorheading [Planet of the Feebs]
@heading [A Somewhat Educational Simulation Game]

Scott E. Fahlman
Computer Science Department
Carnegie-Mellon University

Version: @value [filedate]
@end [titlebox]

@copyrightnotice [Scott E. Fahlman]
@end[titlepage]
@newpage
@section [Introduction]

@i[Planet of the Feebs] is a simulation game that is intended as a
training aid for beginning programmers in Common Lisp.  It is loosely
based on the "Maze War" game, written by Jim Guyton, Bruce Malasky, and
assorted others at Xerox PARC.  In @i[Planet of the Feebs], however, the
players do not control their creatures by hand.  Instead, they supply
programs which control the creatures as they move around the maze trying
to zap one another.  The game presents an open-ended challenge, and
advanced players may find themselves reaching deep into the AI bag of
tricks as they try to build ever more clever and adaptable creatures.

The current version of the program runs in CMU Common Lisp on the IBM RT
PC under the Mach operating system.  It uses the X window manager.  It
was designed by Scott Fahlman and programmed by Fahlman, Skef Wholey,
and Dan Kuokka.  Other members of the Spice Lisp group at CMU have
contributed ideas and have helped to tune and polish the system.

The Feebs environment is easily reconfigurable by changing certain
global parameters.  Normally, the parameters to be used in a given
contest are announced to the players before they begin writing their
programs, since very different strategies may be appropriate with
different parameters.  In this document, we will indicate the names of
those parameters in boldface, followed by the usual value of this
parameter in parentheses.  To see how any given feebs world is
configured, execute the function (list-parameter-settings), which will
create and return an A-list of all the interesting parameters:

@begin [example]
((*number-of-feebs* . 10)
 (*carcass-rot-probability* . 1/3) ...)
@end [example]

@section [Life Among The Feebs]

Deep below the surface of a medium-sized planet, in a long-forgotten
maze of tunnels, live a race of strange creatures, the feebs.  The
origin of the name "feebs" is lost in the mists of antiquity.  Some say
that the name is short for "feeble minded", which most of the creatures
certainly are; some say that the name is a contraction of the term
"flaming bogons", a phrase whose origins are also, unfortunately, lost
in antiquity; most likely, the name comes from the feebing noise that
the creatures occasionally make as they wander the maze.

Locked in their isolated world, the feebs compete fiercely for the
two types of available food: large mushrooms that sometimes appear at
certain places in the labyrinth, and each other.  Life is tough in the
labyrinth; it's every feeb for itself.  The goal is survival, and any
competitors must be ruthlessly eliminated.

Physically very similar to one another, the feebs must try to survive by
superior intelligence.  Each feeb has an electrical organ which can use
to "flame" at its competitors, emitting a lethal "fireball" -- actually
an unstable ball of high-energy plasma, similar to ball lightning.  The
fireball travels erratically down the tunnel in the direction the
creature is facing.  The flame is the feeb's only weapon.  The only
defense is to stay out of the line of fire or to outrun the oncoming
fireball, but a feeb who hides in a secluded corner of the maze will
eventually starve.  Unfortunately, the mushrooms tend to grow at tunnel
junctions and in dead-end corridors, places where feeding creatures run
a considerable risk of ambush or entrapment.

The walls of the maze emit a soft glow due to a coating of
phosphorescent (but inedible) fungi.  Even in this dim light, the feebs
can see well.  The feebs can peek around corners without exposing
themselves to hostile fire, but a peeking feeb can be seen by opponents
in the tunnel he is peeking into.

The simple feeb world does not allow for much variety of action.  Feebs
can turn by 90 or 180 degrees, move one step forward, flame, feed, peek
around corners, or stay put.  Feebs cannot back up or move sideways;
they must turn in the direction they want to go.  Some feebs are
moderately intelligent, but none of them has much coordination.
Consequently, a feeb can only perform one of these basic actions at
once.  (Very early in feeb evolution they learned not to chew gum.)  All
of the feebs suffer from the same limitations, so cleverness -- and
sometimes a bit of luck -- are all-important.

@section [The Game]

@subsection [Overview]

@i[Planet of the Feebs] is played by up to @b[*number-of-feebs*] (10)
human players at once.  Each player controls one of the feebs in the
maze.  This control is not exerted directly, but rather by supplying a
program, called the @i[behavior function], that controls the creature's
actions.  All of the programmed feebs are turned loose in the maze at
once, and the competition for survival begins.  The game runs for a
number of turns specified by @b[*game-length*] (1000).  Zapping an
opponent gives a feeb @b[*points-for-killing*] (+1) point, while being
zapped or dying of hunger gives it a penalty of @b[*points-for-dying*]
(-2) points.  Feebs that manage to shoot themselves do not get credit
for the kill.  A dead feeb is reincarnated as soon as his carcass has
rotted, which takes a few turns.  These newly reincarnated creatures
re-enter the maze at some randomly-chosen entry point.

Being highly evolved creatures, the feebs think using Common Lisp.  (At
one time there were feebs who thought using Pascal, but all of these
have attained a provably correct extinction.)  Each feeb's behavior, at
each turn of the game, is supplied by a Common Lisp function (which may
be a lexical closure).  This function receives as its arguments the
feeb's sensory inputs (including some internal sensations such as the
degree of hunger) and it returns one of the simple actions of which the
feeb is capable.

The game proceeds as a series of synchronous turns.  At the start of
each turn various natural phenomena occur: mushrooms grow, flame-balls
move (destroying things as they go), and feebs occasionally die of
starvation.  Next, the behavior function for each feeb receives its
sensory inputs and computes its desired action.  Occasionally an action
is turned to a no-op because the feeb has responded too slowly (see
below).  Finally, all remaining actions are executed at once.

The maze is laid out as a array of size @b[*maze-i-size*] by
@b[*maze-j-size*] (32 x 32).  Some locations or "squares" contain rock,
others empty space.  All tunnels are one unit wide.  There are T, L and
+ junctions, as well as dead ends, but no large rooms.  There are no
disconnected parts of the maze: it is possible to reach any tunnel
square from any other tunnel square.  The feebs may or may not know the
specific layout of the maze in advance (see the section on Contest
Rules), but a feeb in a strange maze may build up some sort of map as he
wanders around.  Feebs do have a mysterious navigational sense that
tells them their own I and J coordinates at any given time, as well as
the direction in which they are facing.

A feeb always faces North, South, East, or West, and never anything in
between.  Any number of feebs may occupy a given square at once.  A feeb
has a short-range sense of "proximity" that tells it what is in its own
square and certain adjacent squares.  However, a feeb cannot flame into
its own square or engage in hand-to-hand combat (feebs have no hands,
and their rubbery beaks are only strong enough to attack mushrooms and
flame-broiled opponents).  It is rather awkward for two feebs in the
same square to disengage without becoming targets for one another, but
it is also dangerous to remain together, since a successful shot into
this square by another feeb will kill all the occupants.  Mushrooms and
carcasses do not impede the movement of live feebs.

A maze normally contains @b[*number-of-feebs*] (10) feebs, usually
representing that many different players.  If fewer than the specified
number of players are participating, the remaining niches are filled by
the addition of "autofeebs" supplied by the system.  These creatures
come in several types, but all have rather simple behavior patterns.
Some are almost suicidally aggressive, for example, while others run
from trouble whenever possible.

The maze designer pre-designates a set of entry points.  These may be
close to one another, but no entry point will be visible from any other.
There must be at least as many of these entry points as there are feebs
in the maze.  At the start of the game, each feeb is assigned a
different entry point, randomly chosen from this predefined set, facing
in a randomly chosen direction.  A feeb being reincarnated will appear
at one of the original entry points, chosen at random.  This might
result in two feebs suddenly facing one another, but the new feeb is in
considerably greater danger than the existing ones, since it may well
materialize facing a wall or dead end -- yet another reason not to get
yourself killed in the first place.

@subsection [Food]

Feebs have to eat.  At each turn, the behavior function receives an
indication of the creature's current energy reserves, measured in Zots.
This energy reserve is decreased by one Zot after each turn, regardless
of what the creature did during that turn.  Flaming uses up an
additional @b[*flame-energy*] (10) Zots.  The energy is decremented when
the action is executed; if the remaining energy at the start of a turn
is zero or negative, the creature starves to death immediately, leaving
a carcass.

A feeb feeds by executing the :EAT-MUSHROOM or :EAT-CARCASS command
while in a square occupied by the specified type of food.  A feeding
feeb can do nothing else in that turn, and hence is rather vulnerable.
Feeding on a mushroom increments the feeb's energy reserve by
@b[*mushroom-energy*] (50) Zots, but destroys the mushroom.  Feeding on
a carcass increments the energy reserve by @b[*carcass-energy*] (30)
Zots per turn, and may go on for several turns before the carcass rots.
Feebs have a maximum capacity of @b[*maximum-energy*] (200) Zots --
gorging beyond that point does not increase the creature's reserves,
though it may be useful as a way of keeping food away from opponents.
Feebs start the game moderately hungry, with a reserve randomly chosen
between @b[*minimum-starting-energy*] (50) and
@b[*maximum-starting-energy*] (100) Zots.

The maze designer pre-designates certain squares as mushroom-growing
locations.  Typically, there will be @b[*number-of-mushrooms*] (10)
mushrooms in the maze at any given time and
@b[*number-of-mushroom-sites*] (30) places where they may appear.  The
initial locations of the mushrooms are chosen at random from this set of
locations.  Once a mushroom appears, it will remain in that square until
it is eaten or destroyed by a fireball.  Whenever a mushroom is
destroyed, a new one appears at some vacant mushroom-growing location,
chosen at random.  There can never be more than one mushroom in a square
at any given time.  Since an eaten mushroom reappears somewhere else and
an uneaten mushroom will stay around forever, as the game progresses the
mushrooms will tend to be found in parts of the maze that are seldom
visited or at exposed locations where feeding is too risky.

Whenever a feeb dies, either from incineration or starvation, a carcass
is left behind in the square where the death occurred.  Unlike
mushrooms, which disappear when eaten, a carcass may be fed upon
repeatedly, by any number of feebs, until it rots away.  A feeb killed
during Turn T will not begin to rot until turn T+N, where N is the value
of @b[*carcass-guaranteed-lifetime*] (3).  At the end of that turn, and
at the end of each turn thereafter, it has a
@b[*carcass-rot-probability*] (1/3) chance of rotting away (disappearing
completely).  The feeb that died is not reincarnated until after the
carcass has rotted away.  At that point, the feeb appears in one of the
starting squares, pointing in some random direction, and with energy
reserves computed as at the start of the game.

@subsection [Flaming]

When a feeb flames, the fireball travels down the tunnel in the direction
the feeb is facing, moving at one square per turn.  More precisely, if a
feeb gives the :FLAME command during turn T, the fireball appears in the
adjacent square at the start of turn T+1, destroying the contents.  (The
intended victim may, however, have moved away during turn T.)  The fireball
enters the next square at the start of turn T+2, and so on.

As the fireball tries to enter each new square, including the first one, it
has a certain probability of dissipating: this is controlled by
@b[*fireball-dissipation-probability*] (1/4).  When a fireball enters a
square containing a mushroom, that mushroom is destroyed.  When a fireball
enters a square containing a live feeb, that feeb is killed, leaving a
carcass.  Existing carcasses are already burned, so they are not affected
by subsequent fireballs.  A feeb will also be killed if it moves into a
square currently occupied by a fireball.

The fireball proceeds in this manner, killing everything in its path,
until it either dissipates or encounters the wall at the end of the
straight-line corridor.  When the fireball hits the wall, it often
dissipates, but it may be reflected back in the direction it came from
with probability @b[*fireball-reflection-probability*] (1/2).  More
precisely, if the fireball is in square S, the last space of a
corridor, at time T, and would move into a solid-rock square at T+1,
and it happens to be successfully reflected, its direction is reversed
and it appears to be entering square S from the solid rock square at
the start of time T+1.  In addition to the possibility that it will
not be reflected, the fireball must face the usual dissipation
probability on this turn as well.  A reflected fireball then proceeds
to travel back in the direction it came from until it dissipates or is
reflected again.  Fireballs pass through one another with no
interference.

The relatively slow motion of the fireballs has some interesting
consequences.  If a feeb sees a fireball coming, it may well be able
to outrun it or to duck into a side corridor, though time wasted in
turning may be fatal.  It is safe to dash past the mouth of a tunnel
in which another feeb is lurking unless the second feeb happens to
flame in anticipation of the move.  However, it is unwise to stop or
turn while in such an intersection.  Because of the rule that a feeb
moving into a square occupied by a fireball is killed, a feeb cannot
avoid a fireball by rushing toward it, attempting to swap places.
However, a feeb can safely move into the square of an enemy feeb that
is firing on the current turn.  It is very unhealthy for a feeb to
move forward in the turn immediately following one in which it fires.

After flaming, the feeb must wait an unpredictable amount of time
before it can flame again.  On the turn after it fires, a feeb
definitely will be unable to fire.  On every every turn thereafter,
the flamer will have a probability of @b[*flame-recovery-probability*]
(1/2) of recovering and being able to flame again.  A feeb can sense
whether it is ready to fire or not, but cannot tell whether an
opponent is able to fire.  Thus, a certain amount of bluffing is
possible.

@subsection [Timing]

It is dangerous for a feeb to spend too much time thinking about what
to do next.  The time taken by each of the behavior functions is
recorded.  The action ordered by the feeb has a chance of being
aborted (turned into a no-op) with a probability that is proportional
to the time the feeb took to generate the order.  This can be
particularly awkward in the middle of a shootout or when running from
a fireball.

More precisely, if @b[*slow-feeb-noop-switch*] (T) is non-nil, then
the probability of aborting a feeb's move is the product of the time
the feeb took and @b[*slow-feeb-noop-factor*] (.25), divided by the
total time taken by all feebs on this turn.  If
@b[*slow-feeb-noop-switch*] is NIL, this feature is disabled and no
moves are aborted; this mode is recommended when some of the feebs are
being controlled by hand.

@subsection [Actions]

Behavior functions are only called when the creature is alive.
A behavior function indicates its selected action by returning one of
the following keyword symbols:

@Begin[Description]
:TURN-LEFT @\Turn left by 90 degrees, staying in the current square.

:TURN-RIGHT @\Turn right by 90 degrees, staying in the current square.

:TURN-AROUND @\Turn around 180 degrees, staying in the current square.

:MOVE-FORWARD @\Move forward one square.

:FLAME @\Shoot a flame in the direction the creature is facing.

:EAT-MUSHROOM @\Feed on a mushroom if one is available in the current
square.  Otherwise, do nothing.

:EAT-CARCASS @\Feed on a carcass if at least one is available in the
current square.  Otherwise, do nothing.

:PEEK-LEFT @\The creature does not actually move, but the visual input
received next turn will be what the creature would see if it were to move
forward one square and turn left.  If the creature wishes to continue
peeking left, this command must be repeated each turn.

:PEEK-RIGHT @\Analogous to PEEK-LEFT.

:WAIT @\Stay put and do nothing.
@End[Description]

Any output that is not one of the symbols listed above is interpreted as
a :WAIT.

@subsection [Sensory Inputs]

The behavior function always receives five arguments, representing its
various sensory inputs.  The programmer can call these whatever he
likes, but I will call them STATUS, PROXIMITY, VISION, VISION-LEFT, and
VISION-RIGHT.  Each of these is a data structure that the program can
access, but not modify, using a variety of accessing functions.  The
system will destructively modify these structures from one turn to the
next.

The information in the STATUS structure can be accessed as follows:

@Begin[Description]

(name status) @\A string.  Whatever name the user supplied for this
creature at the start of the game.

(facing status) @\The direction the feeb is facing, encoded as a small
integer: 0 = north, 1 = east, 2 = south, 3 = west.  Note: for
convenience, NORTH, EAST, SOUTH, and WEST are defined as constants with
the integer values specified above.

(i-position status) @\An integer from zero (inclusive) to @b[*maze-i-size*]
(exclusive) indicating the creature's current I position.

(j-position status) @\Analogous to i-position.

(peeking status) @\One of :LEFT, :RIGHT, or NIL.  Indicates whether the
visual information coming in this turn is the result of a peek command
in the previous turn.

(line-of-sight status) @\An integer indicating the number of squares
that can be seen in the direction the feeb is facing or peeking.  Thus,
the number of valid entries in the VISION vector.

(energy-reserve status) @\An integer indicating how many energy units
the creature has, equivalent to the number of turns it can live (without
flaming) before it starves to death.

(score status) @\The creature's current score.

(kills status) @\The total number of rivals killed by this feeb (not
counting suicides).

(ready-to-fire status) @\T if the creature is able to fire this turn,
NIL if not.

(aborted status) @\T if the creature's last move was aborted because it
took too long.

(last-move status) @\The last move issued by the creature.  Any of the
action symbols listed above, or :DEAD if the creature has just been
reincarnated.
@End[Description]

The PROXIMITY input is also a read-only structure, with slots describing
the contents of the creature's current square and the adjacent ones to
its rear, left, and right.  The slots are accessed as follows:

@Begin[Description]
(my-square proximity) @\Returns the contents of the current square.  A
feeb does not see himself in this square, even though he is there.

(rear-square proximity) @\Returns the contents of the square behind the
creature.

(left-square proximity) @\Returns the contents of the square to the
creature's left.

(right-square proximity) @\Returns the contents of the square to the
creature's right.
@End[Description]

The values returned by these functions can be any of the following:

@Begin[Description]
NIL @\The square is empty space.

:ROCK @\A solid rock wall in this direction.

:MUSHROOM @\The square contains a mushroom.

:CARCASS @\The square contains a carcass.

A structure of type FEEB-IMAGE @\This represents a feeb.  One can call
(feeb-image-name x) on this image to get the feeb's name and
(feeb-image-facing x) to get the direction it is facing, expressed as an
integer from 0 to 3.

A structure of type FIREBALL-IMAGE @\This represents a fireball.  One
can call (fireball-image-direction x) on this image to get the direction
the fireball is moving, expressed as an integer from 0 to 3.

A list @\This is used when more than one item is in the square at once.
The list can contain any number of image structures, plus perhaps a
:MUSHROOM and/or :CARCASS symbol.
@End[Description]

The VISION argument receives a simple vector whose length is the maximum of
*maze-i-size* and *maze-j-size*.  Thus, it can hold any possible linear
line of sight that can occur in the maze.  However, only some of the
entries are really valid, as indicated by the value returned by the
(line-of-sight status) call; entries beyond the valid range may contain
garbage or hallucinations.

Each item in the VISION vector describes what is in a given cell, using
the same notation as for the PROXIMITY argument.  (svref vision 0)
should return information about the square into which the feeb is
facing, the same information that would be returned by (front-square
proximity).  If the feeb is looking straight ahead, (svref vision 1)
would return the contents of the next square in that direction, and so
on, until we reach (svref vision (line-of-sight status)), which contains
garbage.  If the feeb is peeking left or right, (svref vision 0)
still accesses the square physically in front of the feeb, but higher
indices access a line of squares to the left or right of that square.

The VISION-LEFT and VISION-RIGHT arguments are simple vectors whose
entries correspond to the entries in VISION.  Instead of indicating what
is in each square along the line of sight, however, they tell the
creature what is to the left or right of that square, along the
direction in which you are looking (or peeking).  Each entry will have
one of three values: NIL (an opening on that side), :ROCK (a solid rock
wall on that side), or :PEEKING (an opening from which some creature is
peeking into your corridor).  There is no way to tell whether the peeker
is peeking in your feeb's direction or the other way or whether there are
more than one of them.

@section [Contest Rules]

The rules for a feeb contest, including the settings of all the control
parameters, are announced in advance so that the programmers know what
kind of strategy to program into their behavior functions.  Knowledge
about the maze itself may or may not be available, depending on how
difficult the contest organizers want to make the game.  There are four
levels of complexity:

@begin [description]
Level 0:@\The players are told in advance what the maze will look like,
with or without information about the location of mushroom spaces and
starting spaces.  This information can be used in writing the behavior
functions.

Level 1:@\No information is available about the maze in advance, but
once the game begins a feeb can examine a map of the entire maze and
plan a global strategy.  The map is an array of dimensions *maze-i-size*
by *maze-j-size*, capable of holding any Lisp object in each element.
Initially, each array cell will be either :ROCK or NIL (open space).  To
obtain a map, call (GET-MAZE-MAP).  Each call produces a distinct map
array, so it is possible to write into the map array, perhaps to take
notes.

Level 2:@\No map is available.  If a feeb wants information about the
shape of the maze, it must explore the maze for itself.  Calls to
(GET-MAZE-MAP) return NIL.

Level 3:@\No map is available, and the feeb's location sense is
disabled.  The I-POSITION and J-POSITION cells of the status object will
always read 0.  This makes it much harder to create a map, since
sections of map developed during different incarnations must be fitted
together.
@end [description]

Each player will supply a feeb-creation function with some unique name.
This function, which should be compiled, is loaded and then called with
no arguments.  It returns two values: a behavior function for one feeb
and a string that will be used as that feeb's name.  To avoid name
conflicts, each player should choose a different package.  A complete
feeb definition file might look like this:

@begin [example]
;;; -*- Package: Darth-Feeb -*-

(in-package "DARTH-FEEB" :use '("LISP" "FEEBS"))

;;; This feeb probably won't do too well against any other feebs.

(defun darth-feeb-creator ()
  (values
    #'(lambda (status vision vision-left vision-right)
        (declare (ignore status vision vision-left vision-right))
        (svref '#(:turn-left :move-forward :flame) (random 3)))
    "Darth Feeb"))

@end [example]

In a new Lisp, load "feebs.fasl" and any files containing feeb creation
functions.  Then call (feebs:create-feeb <foo>) for each of the feeb
creation functions.  Finally type (feebs:feebs) to start the show.
Auto-feebs will be created as needed.  At the end, the final scores of
all the feebs will be printed.

Team play is possible, with a sort of telepathic communication between
feebs on the same team.  This is accomplished by calling each
feeb-creation function more than once.  All feebs "hatched" from the
same creation function are team-mates, and their scores can be added
together at the end of the game.  The creation function should return a
different name for each team member; it can return the same behavior
function for all team members, or a different function for each.  It is
possible for these behavior functions to communicate via shared lexical
variables.

@section [Ideas for Future Extensions]

It would be possible to create a large set of attributes and abilities
that feebs might possess, and to make some of the existing ones
optional.  Each of these abilities could be assigned a certain value,
and players could select which combination of abilities their feebs will
have from this menu.  There could be a fixed limit on the total value of
features chosen, or a feeb too heavily loaded with features might just
become slower or less energy-efficient than the others.

Among the features that might be on the menu would be the ability to
move backwards or sideways, the ability to peek, some ability to kill
opponents in one's own square, extended range for the omni-directional
proximity sense, a limited range for the visual sense, some ability to
survive a fireball hit, a sense of smell that indicates the distance to
the nearest food, the ability to see through a fireball (taken for
granted now), the ability to obtain a complete or partial map,
cold-bloodedness (a stationary feeb uses much less energy), and so on.

We might also create a much more diverse and interesting set of
non-player flora and fauna, and perhaps a more interesting landscape.

