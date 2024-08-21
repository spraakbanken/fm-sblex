# FM-SBLEX

## Introduction

FM-SBLEX consists of three computational morphology tools for modern Swedish ([SALDO](http://spraakbanken.gu.se/saldo)), for 19th century Swedish ([Dalin](http://spraakbanken.gu.se/forskning/swefn/dalin)), and for Old Swedish. FM-SBLEX has been developed using the [Functional Morphology library](http://www.cse.chalmers.se/alumni/markus/FM/) **dead link**.

Migrated to git from [svn](https://svn.spraakbanken.gu.se/repos/sblex/pub/fm/sblex).

All tools in FM-SBLEX provide:

* an inflection engine
* a compiler to many standard lexicon formats
* an analyzer
* a synthesizer

## Download and install

FM-SBLEX is licensed under
[![GPL v3](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl.html)

Retrieve the source code from this repository.

```bash
git clone https://github.com/spraakbanken/fm-sblex
cd fm-sblex
```

You compile the software with the following commands. The compilation requires [The Glasgow Haskell Compiler](http://www.haskell.org/ghc/).

```bash
./configure
make
sudo make install
```

This installs three binaries in `/usr/local/bin` named `saldo`, `dalin`, and `fsv`.

FM-SBLEX is also available at hackage: [![Hackage](https://img.shields.io/hackage/v/FM-SBLEX)](https://hackage.haskell.org/package/FM-SBLEX)

Install with:

```bash
cabal install FM-SBLEX
```

Retrieve the development versions of the dictionaries (UTF-8 encoded):

* [saldo.dict](https://svn.spraakdata.gu.se/repos/sblex/pub/fm/dicts/saldo.dict)
* [dalin.dict](https://svn.spraakdata.gu.se/repos/sblex/pub/fm/dicts/dalin.dict)
* [fsv.dict](https://svn.spraakdata.gu.se/repos/sblex/pub/fm/dicts/fsv.dict)

Try it out (these commands print the dictionaries in linewise JSON format):

```bash
saldo saldo.dict -p lex
dalin dalin.dict -p lex
fsv fsv.dict -p lex
```

### Using FM-SBLEX with a PoS tagger

We describe how to use saldo together with a PoS tagger to reduce the number of analyses (e.g., for lemmatization). It is analogous for the other tools.

* Download [Hunpos](http://code.google.com/p/hunpos/), a free POS tagger, together with an utf8-coded language model: [suc2\_parole\_utf8.hunpos.gz](http://demo.spraakdata.gu.se/markus/suc2_parole_utf8.hunpos.gz) trained on [SUC2](http://www.ling.su.se/staff/sofia/suc/suc.html).
* Run the tagger on the input data. The data should be arranged with one word per line and sentence boundaries marked with blank lines.

```bash
cat data.txt | hunpos-tag suc2\_parole\_utf8.hunpos > data.txt.hunpos
```

* Run the data through saldo with the result of the POS tagger as an argument:

```bash
cat data.txt | saldo saldo.dict -t norm -e parole -r data.txt.hunpos > data\_saldo.txt
```

Example of an output. The sign '+' expresses alternative.

```txt
   Jag     jag..pn.1:PF@00S@S
   kan     kunna..vb.1:V@IPAS
   tänka   tänka..vb.1:V@N0AS
   mig     jag..pn.1:PF@00O@S
   att     att..sn.1:CSS
   en      en..al.1:D0@US@S
   massa   massa..nn.1+massa..nn.2:NCUSN@IS
   bedömare        bedömare..nn.1:NCUPN@IS
   och     och..kn.1:CCS
   politiska       politisk..av.1:AQP0PN0S
   aktörer aktör..nn.1:NCUPN@IS
```

## Development

* Update generated code:
  * `cd src/lib`
  * `bnfc -m --haskell -p Dict Dict.cf`
    * `-m` creates a makefile in current folder
    * `-p Dict` creates package `Dict`
  * Rename the generated files:
    * `mv Dict/AbsDict.hs Dict/Abs.hs`
    * `mv Dict/LexDict.x Dict/Lex.x`
    * `mv Dict/ParDict.y Dict/Par.y`
    * `mv Dict/PrintDict.hs Dict/Print.hs`
  * Update the source files and the Makefile with the new names.
  * Run the created `Makefile` to check the generated code with `make`
