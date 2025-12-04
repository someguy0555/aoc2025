main=getContents>>=print.g 0.(snd.foldl(\(i,acc1)a->(i+1,acc1++snd(foldl(\(j,acc2)b->(j+1,if b=='@'then acc2++[(i,j)]else acc2))(0,[])a)))(0,[])).lines
g c a=if d>0then g(c+d)o else c where
 o=filter(\x->f a x>=4)a
 d=t a-t o
f b e@(m,n)=foldl(\x y->if y then x+1else x)0$map(`elem`b)[(m-1,n-1),(m-1,n+0),(m-1,n+1),(m+0,n-1),(m+0,n+1),(m+1,n-1),(m+1,n+0),(m+1,n+1)]
t=length
