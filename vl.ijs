NB. Created Jul 19 2016

NB. =========================================================
NB. History: 
NB. 	* Jul 19 2016 : First Implementation
NB. ---------------------------------------------------------

NB. =========================================================
NB. Implementation of numerical algorithm to find Voronoi 
NB. Languages
NB. Ref: http://www.sfs.uni-tuebingen.de/~gjaeger/cgi/publications.shtml
NB. ---------------------------------------------------------

N=:10		NB.number of words
T=:10		NB.total sample size -> approx T/N in each tile

R=:0		NB. Range for coordinates
x =: |: ,: N ?@$ R	NB. initial words - x coordinate $x = 1 N
y =: |: ,: N ?@$ R	NB. initial words - y coordinate $y = 1 N
lambda =: 0.95	NB. inertia (next period = lambda of last period
		NB. + (1-lambda) of current period)

F=:1000		NB. iterations
min =: 2		NB. initial minimal distance
number =: 0	NB. initial number of samples in each tile
xold =: N $ 0
yold =: N $ 0

w=: x,.y		NB. initial words
t=:|: (2,T) ?@$ R  NB. choose random set of terms

dist=: +/@:*:@:-	NB. (x1-x2)^2 + (y1-y2)^2
NB. Join functions
jndx=: (0 , #@]) #: [: I.@,  =/       NB. join indices
NB. usage X jndx Y where X & Y are 1 column tables

join=: 1 : 0                NB. A (ia;ib;sa;sb) join B
:
 'ia ib sa sb'=. m
 'ja jb'=. |:(ia {"1 x) jndx ib {"1 y
 (sa {"1 ja { x) ,. sb {"1 jb { y
)

NB. find the words distribution with respect to a random set of terms
wdist =: verb define
 'w t' =. y
 wb=. <"1 w	NB. dim N
 tb=: <"1 t	NB. dim T
 p=: tb dist"1 L:0 (|: (#tb) #"0 wb) 	NB. compute distance of all words from terms 
 pi=: (I.@:=  <./)@:; L:1 <"1 p 	NB. index of word closest to term 
 pip =: <"0;pi			NB. there is a hidden dimention that needs to be razed
 |: tb,: pip				NB. return word mapping to random terms
)

NB. ------- One Iteration : Begin ------
tbI =: wdist w;t

NB. this part of code is not required since I use join directly
votes =: (i. N) ([: +/"1 =/) (; _1{."1 tbI) NB. which words are close to terms
votesI =: I.@:(0&<) votes		NB. to find which words are relevant
wb =: <"1 w 
wt =: |: (<"0 i.N) , (<"0 votes),: wb     NB. table of index of w + votes
wo =: wt (0;1;0 1 2;0)join tbI	NB. find all the terms that map to a word

NB. TODO: Now do the lambda transformation to get new words
g =: ({."1 wo) </. ({:"1 wo)   	NB. Use first column as keys; collect last-column values
gsum =: (+/@:>) L:1 g	  	NB. sum up the coordinates
wl =: ((votesI{ lambda*(>wb)) + ((1-lambda)* (>gsum)))%(votesI { votes) NB. do lambda transform
comp_nw_ow_1 =: (<"0 votesI) ,. (<"1 wl) ,. (<"1 votesI { w) NB. compare new with old!
owI =: (i.N) -. votesI
comp_nw_ow_0 =:  (<"0 owI) ,. (<"1 owI { w) ,. (<"1 owI { w) NB. list of old
comp_nw_ow =: comp_nw_ow_0 , comp_nw_ow_1
nw =: > (1{"1 comp_nw_ow) /: ({."1 comp_nw_ow) NB. sort new words using first column

NB. ------- One Iteration : End ------

NB. use the numerical approx method to determine the new words
wNew =: verb define
 'w t' =. y
 tbI =. wdist w;t
 votes =. (i. N) ([: +/"1 =/) (; _1{."1 tbI) NB. which words are close to terms
 votesI =. I.@:(0&<) votes		NB. to find which words are relevant
 wb =. <"1 w 
 wt =. |: (<"0 i.N) , (<"0 votes),: wb     NB. table of index of w + votes
 wo =. wt (0;1;0 1 2;0)join tbI	NB. find all the terms that map to a word

 NB. Now do the lambda transformation to get new words
 g =. ({."1 wo) </. ({:"1 wo)   	NB. Use first column as keys; collect last-column values
 gsum =. (+/@:>) L:1 g	  	NB. sum up the coordinates
 nw =. ((votesI{ lambda*(>wb)) + ((1-lambda)* (>gsum)))%(votesI { votes)
 comp_nw_ow_1 =. (<"0 votesI) ,. (<"1 nw) ,. (<"1 votesI { w) NB. compare new with old!
 owI =: (i.N) -. votesI
 comp_nw_ow_0 =.  (<"0 owI) ,. (<"1 owI { w) ,. (<"1 owI { w) NB. list of old
 comp_nw_ow =. comp_nw_ow_0 , comp_nw_ow_1
 nw =. > (1{"1 comp_nw_ow) /: ({."1 comp_nw_ow) NB. sort new words using first column
)

NB. 	nw -: (wNew w;t)		NB. test wNew verb

wNewIter =: verb define
 'w t' =. y
  nw =. wNew y
  t=.|: (2,T) ?@$ R  NB. change random set of terms
  nw;t
)

NB. draw the voronoi evolution!
require (jpath '~Projects'),'/nl/vv.ijs'  

draw_voronoi w
draw_voronoi nw

NB. =========================================================

NB. MatLab Source Code
NB. The source code below runs with Matlab and implements the algorithm
NB. described in section 6.2
NB. N=100; %number of words
NB. T=100; %total sample size -> approx T/N in each tile
NB. x = rand(1,N); %initial words - x coordinate
NB. y = rand(1,N); %initial words - y coordinate
NB. lambda = 0.95; %inertia (next period = lambda of last period
NB. + (1-lambda) of current period)
NB. Voronoi Languages 47
NB. F=1000; %iterations
NB. min = 2; %initial minimal distance
NB. number = 0; %initial number of samples in each tile
NB. xold = zeros(1,N);
NB. yold = zeros(1,N);

NB. for f = 1:F
NB. 	v = rand(T,3); %first column x, second column y, third
NB. 	column index of closest interpretation.
NB. 	v(:,3) = 1;
NB. 	for s=1:T
NB. 		min = 2;
NB. 		for t=1:N;
NB. 			if ((v(s,1)-x(t))^2+(v(s,2)-y(t))^2 < min)
NB. 				v(s,3) = t;
NB. 				min = (v(s,1)-x(t))^2+(v(s,2)-y(t))^2;
NB. 			end
NB. 		end
NB. 	end

NB. 	%count vectors close to code
NB. 	for n = 1:N
NB. 		number = 0;
NB. 		for t = 1:T
NB. 			if (v(t,3) == n)
NB. 				number = number +1;
NB. 			end
NB. 		end
NB. 		%and construct new code
NB. 		if number > 0
NB. 			xold = x;
NB. 			yold = y;
NB. 			x(n) = 0;
NB. 			y(n) = 0;
NB. 			for t = 1:T
NB. 				if (v(t,3) == n)
NB. 					x(n) = x(n) + v(t,1);
NB. 					y(n) = y(n) + v(t,2);
NB. 				end
NB. 			end
NB. 			x(n) = xold(n)*lambda + (1-lambda)*x(n) / number;
NB. 			y(n) = yold(n)*lambda + (1-lambda)*y(n) / number;
NB. 		end
NB. 	end
NB. 	%draw figure
NB. 	if mod(f,50) == 0
NB. 		[f,F]
NB. 		if N>2
NB. 			voronoi(x,y)
NB. 			box on
NB. 			axis([0 1 0 1])
NB. 		else
NB. 			plot(x,y,’.’)
NB. 			box on
NB. 			axis([0 1 0 1])
NB. 		end
NB. 		pause(.01);
NB. 	end
NB. end