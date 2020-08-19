# Chapter Exercises



### Combinators

Determine whether each of the following functions are combinators. [Recall the definition of a combinator; a lambda expression that has no free variables. and serves only to combine provided arguments]

1. $ \lambda x.xxx$ is a combinator as it does not have free variables
2. $\lambda xy.zx$ is not a combinator. $z$ is a free variable as it is not bounded by the head of the lambda expression.



### Normal form or diverge?

Determine whether each of the following expressions can be reduced to a normal form or if they diverge:

1. $\lambda x. xxx$ is already in its beta normal form.

2. $(\lambda z.zz)(\lambda y.yy)$ is beta-reduced as follows:
$$
\begin{align*}
   (\lambda z.zz) (\lambda y.yy) &= [z := (\lambda y.yy)] \;zz \\
   															&= (\lambda y.yy)(\lambda y.yy) \\
   															&= \omega \; \text{as defined in Chapter 1.9}
   \end{align*}
$$
   Alternatively, because the expressions containing $z$ and $y$ are alpha-equivalent and variables are only bound within their own lambda expressions, we can change the variable name $y$ to $z$ for a straightforward "transformation" into the the $\omega$ function. As such, the lambda term described here is a diverging lambda term.



### Beta reduce

Evaluate (beta reduce) each of the following expressions to normal form.

1. $(\lambda abc.cba)zz(\lambda wv.w)$ :
$$
   \begin{align*}
   (\lambda abc.cba)\ z\ z\ (\lambda wv.w) &= (\lambda a.(\lambda b.(\lambda c.cba)))\ z \ z\ (\lambda wv.w) \\
   																				&= ([a:=z](\lambda b.(\lambda c.cba)))\ z\  (\lambda wv.w) \\
   																				&= (\lambda b.(\lambda c.cbz))\ z\ (\lambda wv.w) \\
   																				&= \dots \\
   																				&= (\lambda wv.w)\ z\ z \\
   																				&= \dots \\ 
   																				&= (\lambda v.z)\ z \\
   																				&= z
   \end{align*}
$$


2. $(\lambda xy.xyy)(\lambda a.a)b$ 

$$
\begin{align*}
   (\lambda xy.xyy)(\lambda a.a)b 	&= (\lambda y.(\lambda a.a)yy)b \ \small{\text{(applying the outermost lambda)}}\\
   																&= (\lambda a.a)bb \ \\
   																&= bb
   \end{align*}
$$


