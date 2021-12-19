
fun sift a ns = filterSeq (fn n => n mod a <> 0) ns;

fun sieve(Cons(n, nt)) =
Cons(n, fn () => sieve(sift n (nt())));

val primes = sieve(seqFrom 2);