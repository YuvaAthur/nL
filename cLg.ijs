NB. Created Jul 07 2016

NB. =========================================================
NB. Notes: 
NB.	* Jul 13 2016 : Introduced Join function; 
NB. 			Modified operator list accordingly
NB. 	* Jul 12 2016 : Simplify cL grammar
NB.	* Jul 07 2016 : ChemLambda in grammar formalism
NB. ---------------------------------------------------------

NB. =========================================================
NB. Simplifying cL grammar
NB. Terminals:
NB. ---------------------------------------------------------
NB.	[p] 	: optional port {is a visualization help for edge - not required for processing}
NB.	v   	: variable in {AlphaNumeric set}
NB.       o   	: loop 
NB. Non-terminals: 
NB. ---------------------------------------------------------
NB.	Core
NB. ---------------------------------------------------------
NB.	L   	: Lambda core 		: LL
NB. 	A	: Application core		: AA
NB.	FO	: Fan-out core		
NB.	FOE	: Fan-out Extra
NB.	FI	: Fan-in
NB.	Ar	: Arrow
NB.	T	: Termination		: TT
NB. ---------------------------------------------------------
NB.	Internal representation based on reading mol file
NB. ---------------------------------------------------------
NB.	FRi	: Free In : Source variable appearing as "out" once in mol file
NB.	FRo	: Free Out: Target variable appearing as "in" once in mol file
NB. ---------------------------------------------------------
NB.	Port specifiers
NB. ---------------------------------------------------------
NB. 	mi	: middle in
NB.	mo 	: middle out
NB.	lo	: left out
NB.	ro	: right out
NB.	li	: lefi in
NB.	ri	: right in
NB. ---------------------------------------------------------
NB. Graphical Element
NB. ---------------------------------------------------------
NB.	L mi lo ro	NB. mi li ri ; mo ro lo
NB.	A li ri mo
NB.	FI li ri mo
NB.	FO mi lo ro
NB.	FOE mi lo ro
NB.	Ar mi mo
NB. 	T mi
NB.	FRi mo
NB.	FRo mi
NB. ---------------------------------------------------------
NB. Production Rules
NB. ---------------------------------------------------------
NB.      M mi + Ar mi mo --> M mo 
NB.		M == L A 


NB. ---------------------------------------------------------
NB. parse into cannonical form
NB. input = array of "graphical element" 
NB. output = array of M ; mi li ri mo lo ro
NB.		       k0 k1 k2 k3 k4 k5
NB. assume well formed molecules (==> no errors in input!)
NB.   pr =: smoutput - for debug printing
  pr =: ] NB. for no console printing

NB. =========================================================
NB. To create a 7 column table with Operator followed by mi li ri mo lo ro atoms
cannonical =: verb define
  pr 'in cannonical';y
  if. 4 = #y do. 'M i1 i2 i3' =. y
  elseif. 3 = #y do. 'M i1 i2' =. y
  elseif.  do. 'M i1' =. y
  end. 
  k0 =. k1 =. k2 =. k3 =. k4 =. k5 =. 0
  M =. ;M
  pr 'in cannonical M|$M';M ; ($M); (#M)
NB.   eq =. -: NB. m14=: *./@(= {.) NB. Phrase for "all atoms equal"
  if. 1 = #M do.
    eq =. = 
    if. M eq 'L' do.'M k0 k4 k5' =. 'LL'; ,&'#' L:0 i1;i2;i3
    elseif. M eq 'A' do. 'M k1 k2 k3' =. 'AA'; ,&'#' L:0 i1;i2;i3
    elseif. M eq 'T' do. 'M k0' =. 'TT';,&'#' i1
    end.
  else. 
    eq =. -:
    pr 'in cannonical M, test match for FO';(M -: 'FO')
    if. M eq 'FI' do. 'k1 k2 k3' =. ,&'#' L:0 i1;i2;i3
    elseif. M eq 'FO' do. 'k0 k4 k5' =. ,&'#' L:0 i1;i2;i3
    elseif. M eq 'FOE' do. 'k0 k4 k5' =. ,&'#' L:0 i1;i2;i3
    elseif. M eq 'Ar' do. 'k0 k3' =. ,&'#' L:0 i1;i2
    elseif. M eq 'FRi' do. k3 =. ,&'#' i1
    elseif. M eq 'FRo' do. k0 =. ,&'#' i1
    end.
  end.
  r =. (<M),k0;k1;k2;k3;k4;k5
  pr 'out cannonical';r
  r
)
NB. ---------------------------------------------------------


NB. =========================================================
NB. Notes: Jul 13 2016
NB. ---------------------------------------------------------
NB.
NB. Introduced Join Function to identify reaction sites
NB.

NB. = or e. function works best when there are atleast 2 characters! 
NB. needed for join to work right!
ML =: 'LL';'AA';'TT';'FI';'FO';'FOE';'Ar';'FRi';'FRo'
NB. Introduced these changes into cannonical function above

MLT =: ,. ML
NB. This make MLT a table to be used in join function

NB. Join functions
jndx=: (0 , #@]) #: [: I.@,  =/       NB. join indices
NB. usage X jndx Y where X & Y are 1 column tables

join=: 1 : 0                NB. A (ia;ib;sa;sb) join B
:
 'ia ib sa sb'=. m
 'ja jb'=. |:(ia {"1 x) jndx ib {"1 y
 (sa {"1 ja { x) ,. sb {"1 jb { y
)
NB. ---------------------------------------------------------


NB. =========================================================
NB. Notes: Jul 14 2016, Jul 18, 2016
NB. ---------------------------------------------------------
NB. Reference
NB. https://chorasimilarity.wordpress.com/2015/03/15/the-moves-of-chemlambda-v2-in-mol-format/


NB. ChemLamda Moves
NB. Cannonical form: M ; mi li ri mo lo ro

NB. COMB:  (assumes Ar respects the port substitution)
NB. 	M[.,.,.,a,.,.] + Ar[a,.,.,b,.,.] --> M[.,.,.,b,.,.]
NB. 	M[.,.,.,.,a,.] + Ar[a,.,.,b,.,.] --> M[.,.,.,.,b,.]
NB. 	M[.,.,.,.,.,a] + Ar[a,.,.,b,.,.] --> M[.,.,.,.,.,b]
NB.
NB. BETA:
NB. 	L[a,0,0,0,b,c] + A[0,c,d,e,0,0] --> Ar[a,0,0,e,0,0] 
NB. 					+ Ar[d,0,0,b,0,0]
NB.
NB. FI-FOE:
NB. 	FI[0,a,b,c,0,0] + FOE[c,0,0,0,b,e] --> Ar[a,0,0,e,0,0]  
NB. 					+ Ar[d,0,0,b,0,0]
NB.
NB. L-FO, L-FOE (aka DIST-L):
NB. 	L[a,0,0,0,b,c] + FO[c,0,0,0,d,e] --> FI[0,j,i,b,0,0]
NB. 					+ L[k,0,0,0,i,d]
NB. 					+ L[l,0,0,0,j,e]
NB. 					+ FOE[a,0,0,0,k,l]
NB.
NB. 	L[a,0,0,0,b,c] + FOE[c,0,0,0,d,e] --> FI[0,j,i,b,0,0]
NB. 					+ L[k,0,0,0,i,d]
NB. 					+ L[l,0,0,0,j,e]
NB. 					+ FOE[a,0,0,0,k,l]
NB.
NB. A-FO, A-FOE (aka DIST-A)
NB. 	A[0,a,b,c,0,0] + FO[c,0,0,0,d,e] --> FOE[a,0,0,0,i,j]
NB. 					+ A[0,i,k,d,0,0]
NB. 					+ A[0,j,l,e,0,0]
NB. 					+ FOE[b,0,0,0,k,l]
NB.
NB. 	A[0,a,b,c,0,0] + FOE[c,0,0,0,d,e] --> FOE[a,0,0,0,i,j]
NB. 					+ A[0,i,k,d,0,0]
NB. 					+ A[0,j,l,e,0,0]
NB. 					+ FOE[b,0,0,0,k,l]
NB.
NB. FI-FO (aka DIST-FI)
NB. 	FI[0,a,b,c,0,0] + FO[c,0,0,0,d,e] --> FO[a,0,0,0,i,j]
NB. 					+ FI[0,i,k,d,0,0]
NB. 					+ FI[0,j,l,e,0,0]
NB. 					+ FO[b,0,0,0,k,l]
NB.
NB. FO-FOE (aka DIST-FO)
NB. 	FO[a,0,0,0,b,c] + FOE[c,0,0,0,d,e] --> FI[0,j,i,b,0,0]
NB. 					+ FO[k,0,0,0,i,d]
NB. 					+ FO[l,0,0,0,j,e]
NB. 					+ FOE[a,0,0,0,k,l]
NB.
NB. PRUNING MOVES
NB. A[0,a,b,c,0,0] + T[c,0,0,0,0,0] --> T[a,0,0,0,0,0] + T[b,0,0,0,0,0]
NB. FI[[0,a,b,c,0,0] + T[c,0,0,0,0,0] --> T[a,0,0,0,0,0] + T[b,0,0,0,0,0]
NB. L[a,0,0,0,b,c] + T[c,0,0,0,0,0] --> T[a,0,0,0,0,0] + FRIN[0,0,0,b,0,0]
NB. FO[a,0,0,0,b,c] + T[b,0,0,0,0,0] --> Ar[a,0,0,c,0,0]
NB. FOE[a,0,0,0,b,c] + T[b,0,0,0,0,0] --> Ar[a,0,0,c,0,0]
NB. FO[a,0,0,0,b,c] + T[c,0,0,0,0,0] --> Ar[a,0,0,b,0,0]
NB. FOE[a,0,0,0,b,c] + T[c,0,0,0,0,0] --> Ar[a,0,0,b,0,0]


NB. prjM : project on Mth column 
NB.	to make it faster than join adverb
NB.	have to check performance :)!
NB. use: To deduce permissible moves
NB. prjM =: 4 : 0
NB.  idx =. x I.@:= M&{"1 y
NB.  idx { y
NB. )

NB. prj : project on mth column  (adverb)
NB. use: To deduce permissible moves project & then join appropriate ports
prj =: 1 : 0  NB. A (m) prj B
:
 idx =. x I.@:= m&{"1 y
 idx { y
)

NB. Column Index Constants
'step' =: i.1
'ID M'=: 1 + i.2 		NB. ID = Mol ID M = Any cL operator
'mi li ri mo lo ro' =: 3 + i.6	NB. Cannonical form constants
R =: i.9


NB. BETA-move:
NB. 	L[a,0,0,0,b,c] + A[0,c,d,e,0,0] --> Ar[a,0,0,e,0,0] 
NB. 					+ Ar[d,0,0,b,0,0]
NB. Output for one row of L-A reduction : 16 columns
NB. 	Ar | mi ; 0 ; 0; (mo+#R) ; 0 ; 0
NB. 	Ar | (ri+#R);0;0; (lo);0;0  
NB. ID is passed from outerloop
betaMove =: dyad define
 betaI =. y
 's id' =. x
 beta1 =. s;id;('Ar');(>betaI{"1~mi);0;0;(>betaI{"1~ (mo+#R));0;0
 beta2 =. s;(>:id);('Ar');(>betaI{"1~(ri+#R));0;0;(>betaI{"1~ lo);0;0 	
 beta1 ;< beta2  NB. to unlink use ;"1 ,.
)
 
NB. beta:
NB. Apply all beta-Moves in parallel
NB. Assume:
NB.	Every row is a possible reaction
NB.	Conflicts of L-A not eliminated 
NB.		(1-L with many-A) or (many-L with 1-A)
NB.	All beta reductions are applied together
beta =: verb define
 s =. >{."1 {. y 			NB. current step 
 bLL =. (<'LL') (M)prj y		NB. left operand
 bAA =. (<'AA') (M)prj y		NB. right operand
 betaI =. bLL (ro;li;R;R) join bAA    NB. reaction sites
 NB. increment step
 ns =.  s , (":>:".{:s)		NB. next step
 oid =. <"0 (#y) + 2 * i. #betaI	NB. new IDs of output molecules
 nsid =. (< ns),. oid		NB. stitch the new step & id range
 NB. output molecules - increment molecule ID
 o =. nsid betaMove"1"1 betaI	NB. apply row-wise
 o				NB. > o will unbox and create table
)

NB. =========================================================
NB. Notes: Jul 07 2016
NB. ---------------------------------------------------------
NB. Reference
NB. https://chorasimilarity.wordpress.com/2014/07/07/lambda-calculus-and-the-fixed-point-combinator-in-chemlambda-i/


NB. Grammar of ChemLamda

NB. BNF grammar to be transcribed to L system grammar
NB. <molecule> ::= <graphical-element> | <molecule> <molecule>
NB. <graphical-element> ::= <lambda> | <fanout> |  <appl> | <fanin> | <arrow>  | <loop> | <termin>
NB. <middle.in>::= port  variable
NB. <middle.out>::= port  variable
NB. <left.in> ::= port variable
NB. <left.out> ::= port variable
NB. <right.in> ::= port variable
NB. <right.out>::= port variable
NB. <lambda>: := L[<middle.in>,<left.out>,<right.out>]
NB. <fanout>::= FO[<middle.in>,<left.out>,<right.out>]
NB. <appl>::= A[<left.in>,<right.in>,<middle.out>]
NB. <fanin>::=FI[<left.in>,<right.in>,<middle.out>]
NB. <arrow>::= Arrow[<middle.in>,<middle.out>]
NB. <loop>::= loop
NB. <termin>::= T[<middle.in>]

NB. There is no simple grammar formalism for Lambda Calculus
NB. So, the possibility of coding ChemLamda as rewrite rules 
NB. would be non-trivial

NB. You might want to start with coding the python code in J 
NB. And then see if the parallel processing approach can be adopted
NB. There might be a significant improvement in speed!

NB. To Do
NB. 1) Port to J from Python to get same output 
NB. 2) Enhance in J for parallel execution
NB. 3) Simulate of Simplex Evolution based on ChemLamda 