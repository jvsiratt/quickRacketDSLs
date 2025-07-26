# quickRacketDSLs

My quick reference for making lexers, parsers, and readers, in Racket.

*Acknowledgments* :
This repo is a personal learning resource, and I did not systematically track all the materials I learned from while creating it. In addition to the excellent official documentation for Racket and Typed Racket, and the Beautiful Racket web text, I’ve benefited greatly from various blogs, forum discussions, code snippets, and examples shared online. If something here closely resembles an existing public resource, it's likely because I studied it, learned from it, and adapted or recreated it for my own understanding.

## Justification

While I have been learning how to make Racket do strange and wonderful things, I've covered a lot of material rapidly. 
I have both a personal and professional interest in being able to use a variety of formal languages in Racket. 
That's actually one of the reasons I chose to learn Racket after years of working mainly in Common Lisp with an ML dialect thrown in here or there for flavor.

Racket really does make the process quick, but honestly my memory can't keep up with my own progress. 
There is a lot of documentation out there and some excellent tutorial content. 
But most often I've found myself referring back to my own code, thinking, "Hey, I did something like that last week... Now where did I put that?"

For my own benefit, I decided to distill what I've been learning down to some minimalistic examples.
At first, I was just keeping it on a USB stick, but I found myself wanting to check something while I was away from my computers.
I thought about saving it to the cloud when the obvious hit me -- just put it on Github, dummy.

So here I am, posting this mainly for my personal reference.
If someone else stumbles across this and it proves useful, that's even better.

# The Content

I've broken things down into bite-sized pieces, mainly to keep me on task, but also to avoid introducing too many new things at once.
At no point do we attempt to make anything really complex, but some programming experience is assumed (that's kind of implicit since I'm writing this for myself, or rather, for some future version of me that's spent months working on something else and has completely forgotten about all of this).

## Section 01 -- A Simple Reader

This covers a simple reader that takes a whitespace-separated word bank and creates a procedure to randomly select a word.

## Section 02 -- REPL Reading

Extending the theme started in the previous section, we combine the word list reader with a REPL reader (and some auxilliary code) to create a word guessing game (e.g., Hangman).

## Section 03 -- Some Minimal Boilerplate

We move into working with brag and create a minimalistic example that provides the bones for more complicated projects.

## Section 04 -- Operator Precedence

A demonstration of how to implement operator precedence, illustrated by a calculator based on template of the previous section.

## Section 05 -- Extending with Macros

Up until this point we've largely ignored or hand-waved macros, but they are a useful tool for adding extended functionality. Here we give an examples of typed macros to create a simple representation of inductive types.
