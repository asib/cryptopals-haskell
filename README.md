Haskell solutions to the Cryptopals challenges
==============================================

Crypto.hs is the module containing all the functions
used in completing the challenges.

The individual sxcy.hs files are individual problem
solutions that can be run using `runhaskell sxcy.hs`
where x is the set number and y is the challenge number.

Some of the functions are in desperate need of refactoring,
some due to actual flaws (perhaps the `score` function, which
scores a string based on the character frequencies, could be
developed further), others to be more idiomatic, and others
still to reduce some of the fluff.
