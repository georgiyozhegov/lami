<img width="120px" align="left" src="media/logo.png">

**_Lami_** is a functional programming language based on lambda-calculus.

I don't know why, but it's slow as fuck right now.

# Performance issues

Time before optimization.

```sh
$ time target/release/lami
Number(14)

________________________________________________________
Executed in    3.46 secs    fish           external
   usr time    2.92 secs    0.00 millis    2.92 secs
   sys time    0.54 secs    1.65 millis    0.54 secs
```

And after... I hope it'll be better.
