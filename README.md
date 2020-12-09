# Purpose
The goal of this small example project is to help anyone interested in writing Telegram bots in Haskell set up their "base camp" very quickly. The prototype features a very consice bot that echoes messages back to the sender.

The code is adapted from https://github.com/klappvisor/haskell-telegram-api with two key adjustments:
* much leaner dependencies
* updated code base to match 2020 Haskell dependencies
* restructured code base to focus on `stack` as the dependency manager. 

# How to use it
1. Make sure you have `stack` installed and on your `$PATH`.
2. Clone this repository. Then `cd` into it and run `stack build`.
3. Make sure the following environment variables are set:
* TELEGRAM_TOKEN (without the `bot` prefix)
* PAYMENTS_TOKEN (can be an empty string)
* PORT (as required by your server configuration)
Please do contact me for any question.
4. Run with `stack exec haskell-telegram-base-exe`. 
