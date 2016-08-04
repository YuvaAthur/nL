NB.simple voronoi

NB. (number of points) voronoi (shape)
NB. Generates an array of indices of the nearest point
voronoi =: 4 :0
  p =. (x,2) ?@$ y
  d2 =. +/@:*:@:-"1  	NB. Euclidean
  d1=. +/@:|@:-"1		NB. Manhattan metric
NB.   (i.<./)@:(+/@:*:@:-"1&p)"1 ,"0/&i./ y
  (i.<./)@:(d1&p)"1 ,"0/&i./ y
)
 

load'viewmat'
NB. viewmat 25 voronoi 500 500


NB. ---------------------------------------------------------
NB. Visualize using gnuplot -- does not yet work
FPATH=: jpath '~temp\'

gp=: ''&$: : (4 : 0)
 f =. FPATH,'mesh.dat'
 y (1!:2 <) f
 gnuplot =: '/usr/local/bin/gnuplot'
 pfcall=: gnuplot, ' --persist -e "unset key ; set terminal qt ; plot ''',f,''' w l"'
 2!:0 pfcall
 EMPTY
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