NB. Created Jul 12 2016

NB. =========================================================
NB. Notes: 
NB.	* Jul 14 2016 : Making string compare work (to avoid encoding)
NB.	* Jul 13 2016 : Experiments with join function
NB. 	* Jul 12 2016 : Read Mol Files
NB. ---------------------------------------------------------

NB. require '.\conf.ijs' 'does not work :(!!
require 'task'

HOME  =: (jpath '~user'), '/projects/nL/'
MOL_F =: HOME, 'mol_files/'
TEST_MOL_FN =: 'betaMove.mol'
NB. TEST_MOL_FN =: 'fibo.mol'
NB. TEST_MOL_FN =: 'test.mol'

require HOME,'clg.ijs' NB. Don't know if there is a better way


NB. parse to get a box array of mol representation
NB. Form M (associated list of parameters as defined by M)!
a =. fread MOL_F,TEST_MOL_FN
b =. (<;._2 @: ,&' ') L:0 ,. (-.&a: <;._2 a,LF)
NB. <;._2 a,LF   means cut the input into strings with delimiter LF
NB. -.&a:        removes empty boxes from sentence list
NB. ,. stitch them together - gets the sentences in nice boxes
NB. (<;._2 @: ,&' ') parses & boxes each word seperated by ' '
NB. L:0 applies to the inner most level

c =. cannonical L:1 b NB. convert to cannonical form using the word boxes at Level 1
d =: ;"1 c NB. make one nice table with 7 cannonical columns and as many rows!
ue =: ~. , }. "1 d NB. collect & find unique end points 
NB. each end point is a potential reaction site
NB. end points that are not reaction sites are FRi & FRo nodes

sInp0 =: (<'0'),. (<"0 (i.#d) ) ,. MLT (0;0;0;1 2 3 4 5 6) join d
NB. testing join function 
NB. --> result is a sorted list based on MLT order with row number
NB. sInp0 = Input Soup! - Step 0 | row Number in Step 0 | Molecule

NB. Next Steps
NB. - Find all reaction sites
NB. - Find permissible soups of molecules = that can occur together
NB.	- Protocol = reaction site & adjacency
NB. - For each perssible soup, execute a reaction step
NB.	- Decision = local reaction (can change topology)
NB. - Collect the soup of output molecules and repeat!
