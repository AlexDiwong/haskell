\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim} 

> module ChildSong6 where
> import Haskore
>
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
> 
> -- repeat something n times
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
> 
> -- Baseline:
> b1 = lmap (fd dqn) [b  3, fs 4, g  4, fs 4]
> b2 = lmap (fd dqn) [b  3, es 4, fs 4, es 4]
> b3 = lmap (fd dqn) [as 3, fs 4, g  4, fs 4]
> 
> bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1
> 
> -- Main Voice:
> v1  = lmap (fd en) [c 5, c 5, c 5, c 5, f 4, f 4, g 4, g 4]
> v2  = lmap vol [f 4, f 4, c 5, c 5, g 4, g 4, c 5, c 5]
> v3  = lmap vol [c 5, c 5, f 4, f 4, c 5, c 5, g 4, g 4]
> 
> mainVoice = v1 :+: v2 :+: (times 2 v3) :+: v1 :+: v2
> 
> -- Putting it all together:
> childSong6 = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine :=: mainVoice))

\end{verbatim} }