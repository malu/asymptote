# Asymptote
Asymptote is a UCI (Universal Chess Interface) chess engine. Currently it does not implement the complete UCI specification, e.g. pondering is currently not supported.

Asymptote does not include a graphical user interface. You need a UCI-compliant GUI like Cutechess or Arena to play against Asymptote.

The UCI options currently supported (as reported by the `uci` command) are:
```
> uci
< id name Asymptote v0.4.2
< id author Maximilian Lupke
< option name Hash type spin default 1 min 0 max 2048
< option name ShowPVBoard type check default false
< uciok
```

The following options are not part of the UCI specification:
* `ShowPvBoard`: if set to `true`, Asymptote will print the board at the end of the current pv (principal variation) each time the pv is update.

## Rust version
Asymptote is developed on the Rust stable channel. There is not guaranteed minimum working version, except the latest stable release.

## Rating
Several versions of Asymptote have been tested by computer chess engine testers.

| Version | CCRL 40/4 | CCRL 40/40 |
| :------ | --------: | ---------: |
| v0.4.2  |      2600 |       2582 |
| v0.3    |      2488 |       2503 |
| v0.2.0  |      2315 |       2314 |
| v0.1.8  |      2173 |       2178 |

(last updated March 17, 2019)

Always up-to-date information can be found at the respective websites:
* [CCRL 40/4](http://ccrl.chessdom.com/ccrl/404/)
* [CCRL 40/40](http://ccrl.chessdom.com/ccrl/4040/)

Thanks to every tester!
