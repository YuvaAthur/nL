NB.simple voronoi

NB. (number of points) voronoi (shape)
NB. Generates an array of indices of the nearest point
NB. 5 vor00 10 10 
NB. optimized for viewmat
vor00 =: 4 :0
  p =. (x,2) ?@$ y
  d2 =. +/@:*:@:-"1  	NB. Euclidean
  d1=. +/@:|@:-"1		NB. Manhattan metric
NB.   (i.<./)@:(+/@:*:@:-"1&p)"1 ,"0/&i./ y
  (i.<./)@:(d1&p)"1 ,"0/&i./ y
)
 

NB. Test vor00
NB. load'viewmat'
NB. viewmat 25 vor00 500 500

vor01 =: verb define
)


NB. ---------------------------------------------------------
NB. Visualize using gnuplot -- does not yet work
FPATH=: jpath '~temp\'

gp=: ''&$: : (4 : 0)
 f =. FPATH,'vizsv.dat'
 y (1!:2 <) f
 gnuplot =: '/usr/local/bin/gnuplot'
 pfcall=: gnuplot, ' --persist -e "unset key ; set terminal qt ; plot ''',f,''' w l"'
 2!:0 pfcall
 EMPTY
)

gp01=: verb define
   gpexe =: '/usr/local/bin/gnuplot --persist -e '
   gpcmd1 =: ' ''plot sin(x)/x'''
   2!:1 gpexe, gpcmd1
)

gp02=: verb define
   gpexe =: '/usr/local/bin/gnuplot --persist -e '
   gpcmd1 =: ' ''plot ', y, ''''
   2!:1 gpexe, gpcmd1
)

NB. gp 25 voronoi 500 500
NB. ---------------------------------------------------------

NB. ---------------------------------------------------------
NB. convex hull
NB. Ref: http://kukuruku.co/hub/funcprog/introduction-to-j-programming-language-2004
convex_hull =: verb define
 s =: ({. , }. /: 12"_ o. }. - {.) @ /:~
 l =: 11"_ o. [: (* +)/ }. - {.
 rr =: (1"_ , (0"_ > 3: l\ ]) , 1"_) # ]
 hull =: [: rr^:_ s 
 hull y
)

points=: 1j_6 5j_4 7j_2 4j_2 10j_1 _2j0 9j0 5j1 7j2 2j4 8j5
convex_hull points