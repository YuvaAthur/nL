NB. =================================================================== 
NB. J Operators for Relational Algebra
NB. Traditional Operators
NB. ---------------------------------------------------------
NB. setunion v create the union of two tables
NB. check: relations are same - dimension, columns
setunion =: ~. @: ([,])           NB. a setunion b

NB. ---------------------------------------------------------
NB. setintersection v find the common elements
NB. check: relations are same - dimension, columns
setintersection=: e.#[            NB. a setintersection b

NB. ---------------------------------------------------------
NB. setdifference v find the elements not in the other
NB. check: relations are same - dimension, columns
setdifference =: (-. @: e.)#[   NB. a setdifference b

NB. ---------------------------------------------------------
NB. setproduct v find the cartesian product
setproduct =: [: ,/ ,"1/          NB. a setproduct b

NB. Special Operators

NB. restrict=:                            NB. WHERE clause
NB.
NB. project =:                           NB. Column list in SELECT statement



NB. ---------------------------------------------------------
NB. join adv join based on columns specified
NB. algorithm based on cartesian product
jndx=: (0 , #@]) #: [: I.@,  =/       NB. join indices

join=: 1 : 0                NB. A (ia;ib;sa;sb) join B
:
 'ia ib sa sb'=. m
 'ja jb'=. |:(ia {"1 x) jndx ib {"1 y
 (sa {"1 ja { x) ,. sb {"1 jb { y
)



NB. ---------------------------------------------------------
NB. divide adv finds all the domain elements of the binary relation
NB. where the range is completely specified by the unary relation.
NB. divide =:   1 : 0  NB. A(ia;ib;sa) divide B 
NB.
NB.
NB. So far:
NB.
NB. ]bX =. (0{"1 X) <@~./. X NB. Boxes the  rows according to key
NB.
NB. ]r =. (/: k ){(k=. ~. 0{"1 Y) NB. unique range values that have to checked against
NB.
NB.  f1=. (0#{.)&.> NB. Create Empty box
NB.
NB. subset =. */ @ e.  NB. a subset b 
NB.
NB. Algorithm:
NB.
NB. 1) Create boxes of values of sa of X (note sa contains ia)
NB.
NB. 2) For each box, determine if all the range elements r are in   binary relation
NB.
NB. 2.1) If yes -> choose the domain element
NB.
NB. 2.2) If no -> skip and continue








NB. Note: First visualize the form then create the verbs.... 
NB.
NB. NB. =========================================================
NB. NB. ------------- divide operator ---------------------------
NB. NB. =========================================================
NB. NB. Form of the divide operator
NB. NB. - Need a binary relation and a unary relation
NB. NB. - Find domain elements of the binary relation that are 
NB. NB. --- complete with respect to the unary relation
NB. NB. - List of all such is the result of a divide
NB.
NB.    ]X=. 5 2  $ 3 | 10 ?. 50
NB. 1 1
NB. 1 2
NB. 0 2
NB. 2 0
NB. 0 0
NB.    ]Y =. 2 1$ 1 2
NB. 1
NB. 2
NB.
NB. NB. Then,
NB.  X (1;0;0) divide Y
NB. NB. does the following
NB. NB. a) choose column 1 of X as range of relation R
NB. NB. b) choose column 0 of Y as range of relation R
NB. NB. c) choose column 0 of X as domain of relation R
NB. NB. d) result should be a 1 column array
NB. NB. In this case, result is
NB.  Z=. 1 1 $ 1
NB.
NB.

 
NB. =========================================================
NB. --------  Study of various options ----------------------
NB. =========================================================
NB. =========================================================
NB. Special Operators of Relational Algebra (Codd)
NB. Join

rajoin =: 4 : 0
x1 =.  {: |: x			
x2 =.  }: |: x
y1 =. {. |: y
y2 =. }. |: y
pm =. x1 ="0 0 / y1  NB. Find the position-match array pm
'd e' =. $ pm
r =.  i. 0 0
for_k. ,pm do.		NB. use pm to guide the assembly of output
if.(1=k) do.
l=. <. k_index % e
n=. e | k_index
r=. r , ((l {"1 x2) ; (l { x1) ; (n {"1 y2))
end.
end.
r
)

NB. ---------------------------------------------------------
NB. Oleg's version

jndx=: (0 , #@]) #: [: I.@,  =/       NB. join indices

jndx2=: (0 , #@]) #: [: I.@,  (-: :: =)@;/ 

join=: 1 : 0                NB. A (ia;ib;sa;sb) join B
:
 'ia ib sa sb'=. m
 'ja jb'=. |:(ia {"1 x) jndx ib {"1 y
 (sa {"1 ja { x) ,. sb {"1 jb { y
)

join2=: 1 : 0                NB. A (ia;ib;sa;sb) join B
:
 'ia ib sa sb'=. m
 'ja jb'=. |:(ia {"1 x) jndx2 ib {"1 y
 (sa {"1 ja { x) ,. sb {"1 jb { y
)

NB. NB. ---------------------------------------------------------
NB. NB. understanding the jndx verb
NB. X =. 15 3 $ 12 | 45 ?. 200
NB. Y =. 10 4 $ 12 | 40 ?. 60
NB. X1 =. 2 {"1 X
NB. Y1 =. 0 {"1 Y
NB.
NB. v1 =. =/
NB. v2 =. I.@,
NB. sv1 =. [: I.@, =/
NB.
NB. NB. X1 sv1 Y1 <--> X1 [: v2 v1 Y1 <--> v2 X1 v1 Y1
NB. NB. In this case, the positions of all the matching positions, 
NB. mp =. X1 =/ Y1
NB. NB. is computed. 
NB. NB. On application of I.@, on mp the following happens
NB. mpr =. , mp
NB. NB. the mp matrix is unravelled.
NB. pos =. I. mpr
NB. NB. the positions are counted in Base 10.
NB. NB. Now to determine the positions in X1 & Y1, the positions have to be 
NB. NB. mapped modulo dimension of Y1.
NB. v3 =. (0 #@])
NB. NB. This verb determines the base to which the right array needs to be 
NB. NB. extracted. 0 indicates unrestrained, while #@] determines the dimension of Y1
NB. r =. (X1 v3 Y1) #: (X1 sv1 Y1)
NB. NB. Finally, the result is assembled in the above way. #: does the mapping to 
NB. NB. (0 dim(Y1)). Thus, the positions of matches are reduced to a 2-D array with
NB. NB. first column refering to position in X1 that matches with the corresponding
NB. NB. position in Y1
NB.
NB.
NB. NB. =========================================================
NB. NB. Performance Measurements
NB. X =. 15 3 $ 12 | 45 ?. 200
NB. Y =. 10 4 $ 12 | 40 ?. 60
NB. Ts 'X rajoin Y'
NB. NB. 0.000959658 9216
NB. Ts '   X (2;0;0 1 2; 0 1 2 3) join Y '
NB. NB. 7.18786e_5 6912
NB.
NB.
NB. NB. =========================================================
NB. NB.
NB. NB. f) J Forum posting - http://www.jsoftware.com/pipermail/general/2005-March/021149.html
NB. NB.
NB. NB.    sr          =:  2 : '(m.&=)`(,:&n.) }'
NB. NB.    enclose     =:  ({.@:[ , ] , {:@:[)
NB. NB.    join        =:  1 : '(-#m.)&}.@;@(,&m. &.>"1)'
NB.
NB.
