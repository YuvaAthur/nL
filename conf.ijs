NB. Created Jul 12 2016

NB. =========================================================
NB. Notes: Jul 12 2016
NB. ---------------------------------------------------------
NB. Settings

require 'task'

HOME  =: (jpath '~user'), '/projects/nL/'
MOL_F =: HOME, 'mol_files/'
TEST_MOL_FN =: 'betaMove.mol' NB.'fibo.mol'

shell 'cd ',HOME