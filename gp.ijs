NB. init

require' task'

coclass 'pgnuplot'
NB. gnuplot
NB.
NB. Supports creating gnuplot plot files from J.
NB.
NB. This has been tested with gnuplot 4.0.
NB.
NB. For example under Windows, try:
NB.
NB.    load 'graphics/gnuplot'
NB.
NB.    GNUPLOTBIN_pgnuplot_=: 'c:\gnuplot\bin\'
NB.
NB.    load 'graphics/gnuplot/gpdemo.ijs'
NB.
NB.    gp1''

NB. =========================================================
NB.*GNUPLOTBIN n gnuplot bin directory (under Windows)
NB.
NB. This is used to determine the executable, wgnuplot.exe
NB. and the configuration file, gnuplot.ini.
NB.
NB. example:
NB.
NB.   GNUPLOTBIN_pgnuplot_=: 'c:\gnuplot\bin\'

NB. =========================================================
NB.*GNUPLOTOUT n directory for gnuplot graphics files
NB.
NB. This defaults to ~temp
GNUPLOTOUT=: jpath '~temp\'

NB. =========================================================
NB. gnuplot command buffer
SETBUFFER=: ''
NB. defs

NB. =========================================================
NB. global SETTINGS has possible gnuplot set names
SETTINGS=: 0 : 0
angles
arrow
autoscale
border
boxwidth
clabel
clip
cntrparam
contour
data style
dgrid3d
dummy
format
function style
functions
grid
hidden3d
isosamples
key
label
logscale
mapping
offsets
output
parametric
polar
rrange
samples
size
style
surface
terminal
tics
time
title
trange
urange
variables
view
vrange
xlabel
xrange
xtics
xdtics
xmtics
xzeroaxis
ylabel
yrange
ytics
ydtics
ymtics
yzeroaxis
zero
zeroaxis
zlabel
zrange
ztics
zdtics
zmtics
)

NB. =========================================================
NB. global STYLES has possible gnuplot styles
STYLES=: 0 : 0
boxes
boxerrorbars
dots
errorbars
impulses
lines
linespoints
points
steps
)
NB. gnuplot utils

deb=: #~ (+. 1: |. (> </\))@(' '&~:)
fexist=: 1:@(1!:4)@boxopen ::0:
fslash=: 3 : '''/'' (I. y=''\'') } y'
isboxed=: 0: < L.
itemize=: ,:^:(2: > #@$)

NB. =========================================================
cutstring=: ','&$: : (4 : 0)
if. 0 = #y do. '' return. end.
if. L. y do. y return. end.
txt=. y, {.x
msk=. txt = ''''
com=. (txt e. x) > ~: /\ msk
msk=. (msk *. ~:/\msk) < msk <: 1 |. msk
deb each (msk # com) <;._2 msk # txt
)

NB. =========================================================
getbinfiles=: 3 : 0
res=. '';''
if. 0 ~: 4!:0 <'GNUPLOTBIN' do. return. end.
if. 0 = #GNUPLOTBIN do. return. end.
bin=. jhostpath GNUPLOTBIN
bin=. bin, PATHSEP_j_ -. {: bin
exe=. bin,'wgnuplot.exe'
ini=. bin,'gnuplot.ini'
exe=. (fexist exe) # exe
ini=. (fexist ini) # ini
exe;ini
)

NB. =========================================================
gpflatten=: [: _2&}. [: ; ,&CRLF@, each @ < @ (,"1&CRLF @ ":)"2

NB. =========================================================
NB. getdata
NB.
NB. this function mimics the behaviour of J Plot
getdata=: 4 : 0
dat=. y
p3d=. 's' e. x

NB. ---------------------------------------------------------
if. p3d do.
  if. isboxed dat do.
    select. #dat
    case. 1 do.
      z=. 0 pick dat
      x=. i.#z
      y=. i.{:$z
    case. 2 do.
      'y z'=. dat
      x=. i.#z
    case. 3 do.
      'x y z'=. dat
    end.
  else.
    z=. itemize dat
    y=. i. {:$ z
    x=. i. # z
  end.
  (x ,"0/ y),"1 0 z
  
NB. ---------------------------------------------------------
else.
  if. isboxed dat do.
    select. #dat
    case. 1 do.
      y=. 0 pick dat
      x=. i.{:$y
    case. 2 do.
      'x y'=. dat
    end.
  else.
    y=. itemize dat
    x=. i. {:$ y
  end.
  x ,. |: y
end.
)

NB. =========================================================
NB. following builds a shell script for running gnuplot
NB. but cannot get this to work.  cdb dec 2006
gphost=: 3 : 0
return.
f=. jpath '~temp/shell.sh'
y=. y, LF -. {:y
y 1!:2 <f
'rwx------' 1!:7 <f
2!:0 f
1!:55 <f
)

NB. gplot main functions

NB. =========================================================
NB. gnuplot      - main plotting function
gnuplot=: 4 : 0
'cmd dat'=. y

NB. ---------------------------------------------------------
if. IFWIN do.
  'exe ini'=. getbinfiles''
else.
  exe=. '/usr/local/bin/gnuplot'
  ini=. ''
end.

NB. ---------------------------------------------------------
NB. files:
pfx=. fslash GNUPLOTOUT,'gnu'
fdat=. pfx,'.dat'
fplt=. pfx,'.plt'

NB. ---------------------------------------------------------
dat=. x getdata dat
shp=. $dat

dat gpwrite fdat

NB. ---------------------------------------------------------
if. #ini do.
  txt=. 'load "',(fslash ini),'"',LF
else.
  txt=. ''
end.
txt=. txt, gpsetcommand''
txt=. txt, x gpcommand cmd;shp;fdat
txt 1!:2 <fplt

NB. ---------------------------------------------------------
if. IFWIN *. 0 < #exe do.
  fork_jtask_ '"',exe,'" "',fplt,'" -'
else.
  gphost exe,' "',fplt,'" -'
end.

empty''
)

NB. =========================================================
NB. gpcommand
NB. returns gplot command
gpcommand=: 4 : 0
'cmd shp file'=. y

file=. '"',file,'"'
cmd=. cutstring cmd
spl=. 's' e. x

NB. assume only 1 data set if surface
cls=. spl { (<:{:shp), 1

msk=. #~ -.@(+. ~:/\)@(e.&'''"')
txt=. msk each cmd

if. '[' e. j=. >{.txt do.
  b=. +./\.j=']'
  range=. b#first=. >{.cmd
  cmd=. (<(-.b)#first),}.cmd
else.
  range=. ''
end.

NB. ---------------------------------------------------------
if. #cmd do.
  b=. (1: e. 'title'&E.) &> txt
  cmd=. (b{'notitle ';'') ,each cmd
  cmd=. ' ' ,each cmd
else.
  cmd=. <' notitle'
end.

NB. ---------------------------------------------------------
if. 's' e. x do.
  cmd=. ; cmd
  def=. file
else.
  cmd=. cls {. cmd, cls $ {: cmd
  cmd=. cmd ,&> <',\',LF
  def=. (file,"1 ' using 1:')&, &> ": each 2 + i.cls
end.

NB. ---------------------------------------------------------
x,'plot ',range, ' ', _3 }. ,def ,"1 cmd
)

NB. =========================================================
NB. gset
NB. build set arguments in SETBUFFER
NB. if y is empty, return and reset buffer
gset=: 3 : 0
r=. empty ''
if. #y do.
  y=. y, ';' -. {: y   NB. ensure trailing ;
  SETBUFFER=: SETBUFFER,y
else.
  if. 0=4!:0 <'SETBUFFER' do.
    r=. SETBUFFER
  end.
  SETBUFFER=: ''
end.
r
)

NB. =========================================================
gpsetcommand=: 3 : 0
;< @ ('set '&, @ (,&LF)) ;._2 gset''
)

NB. =========================================================
NB. gpwrite   - write data to file as text
NB. dat gpwrite file
gpwrite=: 4 : 0
dat=. gpflatten x
dat=. '-' (I. dat='_') } dat
dat 1!:2 boxopen y
)
NB. run

NB. =========================================================
gplot=: 3 : 0
'' gplot y
:
'' gnuplot x;<y
)

NB. =========================================================
gsplot=: 3 : 0
'' gsplot y
:
's' gnuplot x;<y
)

NB. zdef
NB.
NB. cover functions defined in z

NB. =========================================================
NB.*gplot v main verb for gnuplot
NB.
NB. form:  options gplot data
NB.
NB. example:
NB.
NB. 'with line' gplot sin steps 0 5 60
gplot_z_=: gplot_pgnuplot_

NB. =========================================================
NB.*gsplot v main verb for surface gnuplot
NB.
NB. form:  options gsplot data
NB.
NB. example:
NB.
NB. gsplot sin */~ steps 0 5 60
gsplot_z_=: gsplot_pgnuplot_

NB. =========================================================
NB.*gset v set gnuplot options
NB.
NB. form:  gset data
NB.
NB. example:
NB.
NB.    gset 'grid'
NB.    gset 'title "sin(exp) vs cos(exp)"'
NB.    gplot mydata
gset_z_=: gset_pgnuplot_
