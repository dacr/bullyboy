# SHA1 hash brute force tester

The goal is to see how much time it takes to recover a *lost* password from a SHA-1 hash
when no salt has been used to generate the password hash. Various SHA-1 implementations
are tested, as well as several ways to try using all available CPU.

## Experimentations notes
 
- Introducted a NOP sha-1 implementation, that does nothing, just returns the pass. Allow me
  to easily estimate the overall overhead.
- default sha-1 message digester is not thread safe, to take advantage of all available CPU we 
  must pool several instances, at least for each available CORE (or chosen parallelism level).
  Two way to cache those objects :
  + Using an object pool : but with high impact on performances. 
  + Using Thread local variables : Quite faster than using an object pool.
- progress first implementation named StdoutProgress is fast but generates blocking as soon
  as we starts using parallel algorithms.
- First parallel implementations didn't improve performances ! it was even worst, for 
  nop sha-1, 110 days for classic => 740 days for parallel

## Some interesting external links

- [Concurrency in scala](https://twitter.github.io/scala_school/concurrency.html)
