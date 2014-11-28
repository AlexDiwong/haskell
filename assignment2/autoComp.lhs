Functional Music by Alexander Söderberg ain10aso and Max Åberg mat09mab
=======================================================================

This is our attempt at solving the assignment "functional Music".

In this assignment we are to write a program that writes an accompaniment to the song Twinkle Twinkle Little Star.

We begin by importing from haskore library.

> module AutoComp where
> import Haskore hiding (Major, Minor, Key)
> import Control.Applicative
> import Control.Arrow
> import Data.Ix
> import Data.List
> import Data.Ratio
> import Data.Maybe

A song can be broken down to a series of chords. These chords, played in the right order will produce a song.
Twinkle Twinkle's chord progression is:

C        C       F      C
Twinkle, twinkle little star.
F     C      G        C
How I wonder what you are.
C   F        C        G
Up above the world so high,
C      F       C      G
Like a diamond in the sky.
C        C       F      C
Twinkle, twinkle little star.
F     C      G        C
How I wonder what you are.

These can be broken down to three different repeating patterns

CCFG
FCGC
CFCG

And thus the song is CCFG FCGC CFCG CFCG CCFG FCGC or if you call the patterns 1,2,3 the song is: 1,2,3,3,1,2.

The melody of the song is in this case a series of quarter notes. Like so:

| C C G G | A A G - |
| F F E E | D D C - |
| G G F F | E E D - |
| G G F F | E E D - |
| C C G G | A A G - |
| F F E E | D D C - |


Bass Lines
==========

There are three different Bas Line-styles, Basic, Calypso and Boogie.

Basic Bas Line consists of two sounding half notes on the first and fifth beat out of eight beat. The tone played on the first beat is the root note and the tone played on the second beat is the fifth of that note.

Calypso bass line has two silent quarter notes followed by two sounding ones. The sounding notes are the root and the third note.

Boogie Bass line has a sounding quarter tone on every beat. The tones are -> root,fifth,sixth, fifth.


Lets create the different scale patterns.

> type scalePattern = [Int]

> ionian 		= [0, 2, 4, 5, 7, 9, 11]
> lydian 		= [0, 2, 4, 6, 7, 9, 11]
> mixolydian 	= [0, 2, 4, 5, 7, 9, 10]
> aeolian 		= [0, 2, 3, 5, 7, 8, 10]
> dorian 		= [0, 2, 3, 5, 7, 9, 10]
> phrygian 		= [0, 1, 3, 5, 7, 8, 10]

> autoBass :: BassStyle -> Key -> ChordProgression -> Music

> autoChord :: Key -> ChordProgression -> Music

