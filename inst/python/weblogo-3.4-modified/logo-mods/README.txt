To modify weblogo, first download the weblogo code, then modify it.

0. WebLogo requires Python 2.6, 2.7, 3.2, 3.3 or 3.4 and the python array
   package 'numpy' (http://www.scipy.org/Download)

1. Download the weblogo code from https://github.com/WebLogo/weblogo/releases
   To install, run "python setup.py install" or "./setup.py install".

2. Modify the weblogo code.
   To do this, see the instructions below (patch two files and rerun setup.py).
   

DETAILS

I changed two python files in weblogo-3.4/weblogolib, as described below.

The changes are summarized as diff output files in weblogolib.
To use them, simply change to that directory and edit the files using patch:
  patch __init.py__ __init.py__-DIFF
  patch colorscheme.py colorscheme.py-DIFF
NB: Either type the full path to the DIFF file or copy it into the
  same directory as the weblogolib source before running patch.

Note that you have to rebuild the library after modifying it; i.e. 
  (sudo?) weblogo-3.4/setup.py install

I also changed template.eps but do not recall now whether the change is beneficial.

---

The file sourceme.txt runs weblogo on the file logo-test.fasta, as an example.

TO BUILD AND INSTALL weblogo, cd to its subdirectory and run 
"./setup.py build; ./setup.py install" 
(You may need administrative privileges or an alternative install path.)



diff colorscheme.py colorscheme.py-original
124,126d123
<     ColorGroup( "O",   "magenta", "PNGsite"  ), # pth
<     ColorGroup( "-*#",     "grey",   "unknown"), # pth
<     ColorGroup( ".",     "white",   "transmitted"), # pth
128c125
<     alphabet = seq.generic_alphabet
---
>     alphabet = seq.unambiguous_protein_alphabet
136d132
<   ColorGroup( "O",     "cyan",   "PNGsite"),
138d133
<   ColorGroup( ".",     "white",   "transmitted"), # pth
147,149d141
<     ColorGroup( "-*#",     "grey",   "unknown"), # pth
<     ColorGroup( ".",     "white",   "transmitted"), # pth
<     ColorGroup( "O", "cyan", "PNGsite") , # pth
151c143
<     alphabet = seq.generic_alphabet
---
>     alphabet = seq.unambiguous_protein_alphabet
156,157d147
<     ColorGroup( "-*#",     "grey"), # pth
<     ColorGroup( ".",     "white"), # pth
170d159
<     ColorGroup( 'O', '#00FFFF'), # pth
181c170
<     alphabet = seq.generic_alphabet
---
>     alphabet = seq.unambiguous_protein_alphabet




diff __init__.py __init__.py-original 
164,165c164,165
< release_date ="$Date: 2012-08-15 12:53:57 -0700 (Wed, 15 Aug 2012) $".split()[1]
< release_build = "$Revision: 155 $".split()[1]
---
> release_date ="$Date: 2012-07-02 19:28:12 -0700 (Mon, 02 Jul 2012) $".split()[1]
> release_build = "$Revision: 145 $".split()[1]
214d213
<                 "-dUseCIEColor", # pth 11102013
865,869d863
< #here: always place '-' and characters last
<             if c1[1] == '-' : return -1
<             if c2[1] == '-' : return 1
<             if c1[1] == '#' : return -1
<             if c2[1] == '#' : return 1
1055c1049
<         seqs.alphabet = Alphabet(alphabet) 
---
>         seqs.alphabet = alphabet 
