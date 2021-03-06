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
  + The issue was linked to the password generator that was not fast enough, and was not 
    used by the default classic implementation !
- Sometimes it is required to restart several times "sbt run" to achieve better performances !
  + Why ?
- For parallel collections, ThreadPoolTaskSupport has been deprecated, but it has faster in
  my case, than ForkJoinTaskSupport.
  + But it can be replace hopefully by the following conversion :
    `new ExecutionContextTaskSupport(ExecutionContext.fromExecutor(impls.executor))`
  + Just use for example a FixedThreadPool for impls.executor..
  + But it looks like it doesn't achieve the same performance as for ThreadPoolTaskSupport
- For implementations based on parallel collections, a good grouping size can help to achieve
  the best performances. 100000 brings better performances than 10000 or 500000  
- 1.8.0_72 is almost two times faster than 1.7.0_80 ! => 650 days versus 1130 days !!
  + Context : ParallelBrutalizer with Sha1NativeThreadLocal - PasswordGenerator - StdoutAtomicProgress 

## Some interesting external links

- [Concurrency in scala](https://twitter.github.io/scala_school/concurrency.html)
