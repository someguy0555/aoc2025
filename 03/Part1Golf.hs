-- main=getContents>>=print.sum.map((\l->maximum(init l)*10+maximum((\a->drop 1.dropWhile(maximum a/=)$a)(init l)++[last l])).map(\c->read[c])).lines

-- main=getContents>>=print.sum.map((\l->x(init l)*10+x((\a->drop 1.dropWhile(x a/=)$a)(init l)++[last l])).map g).lines
-- x=maximum
-- g c=read[c]

main=getContents>>=print.sum.map(v 0 12.map g).lines
v a n l=if n==1then s else v s(n-1)(r++(e.take(n-1).e$l))where
 (f,r)=(\a->(x a,).(drop 1.dropWhile(x a/=))$a)$take(length l-n+1)l
 s=a*10+f
e=reverse
x=maximum
g c=read[c]
