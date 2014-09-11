# Win-ston Drafts #

Win-ston drafts is a web application for two-person Magic: the
Gathering drafts.

## What is a Winston Draft? ##

A "winston draft" is a draft format specifically designed to be played
with two people, in contrast to traditional "booster drafts" that work
best with 4 or more players.

The first step of the winston draft is selecting the pool of cards to
be drafted.  The size can vary but 100 cards is fairly standard.  The
cards can obviously be anything, but for this application we allow the
user to submit a list of 100+ cards (a cube list works nicely) and
then select 100 cards at random to draft.

The top three cards of the pool are placed face down into 3 separate piles.  The
first player looks at the first pile and has the choice to either take
that pile, or pass.  If he passes, another card from the pool is added
to the pile face down.  This process is repeated with the second and
third piles.  If the player passes on the third pile, he receives the
top card of the pool.

The second player then looks at the first pile and goes through the
same process of choose/pass.  Once the pool is depleted (100 cards
have been drafted) the players assemble their decks.

## Starting a Game ##

By clicking on the "Draft Lobby" link in the sidebar, the player can
enter the Lobby and browse existing games or create a new one.  To
join an existing game, simply click the "Join" button next to the
game.  To create a new game, click "Create".

"Create" brings up a screen where the player can enter the name of the
game and the cards for the pool.  The pool must contain at least 100
cards -- if more than 100 cards are added a random set of 100 will be
chosen.  Cards can be entered by simply typing each card's name on a
separate name (or more likely, by exporting a cube list in plain text
format).

## Drafting ##

The drafting interface is fairly straightforward.  When it is your
turn, the cards in the first pile will be turned face up and you can
click "Choose" or "Pass".  When you choose a pile or pass the last
pile, it will be added to your "Main" area below.

Clicking on a card in "Main" will move it to your "Sideboard" and vice
versa.

To add basic lands, simply enter the number of lands you want in the
fields below and hit enter.

## Downloading a Deck ##

Once all the cards in the pool have been drafted, a button will appear
to download your deck.  Clicking this button will download a plain
text file that can be loaded directly into a program like Cockatrice
by saving it as a .dec file or using the "Load deck from clipboard"
option in Cockatrice.
