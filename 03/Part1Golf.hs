main=getContents>>=print.sum.map((\l->x (init l)*10+x ((\a->drop 1.dropWhile(x a/=)$a) (init l)++[last l])).map g).lines
x=maximum
g c=read [c]
-- main=getContents>>=print.sum.map(v 0 12.map g).lines
-- u l=(x l,).(drop 1.dropWhile(x l/=))$l
-- v a n l=if n==1then s else v s(n-1)(r++(e.take(n-1).e$l))where
--  (f,r)=u$take(length l-n+1)l
--  s=a*10+f
-- e=reverse
-- x=maximum
-- g c=read[c]
