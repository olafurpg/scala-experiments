## Large-scale semantic analysis of Scala source code

This repo contains a minimal skeleton to run semantic analysis on a corpus of 2
million lines of Scala code.
A breakdown of which projects are included in the corpus and how many lines of
code each project contains can be found here
https://docs.google.com/spreadsheets/d/1btkCiF30Wb9MJti6LDc9og788XqXBKgwEhIdKb9aloc/edit#gid=1799045603

Analyses run fast since we don't have to re-compile the corpus to run a new analysis.
Simple analyses on the full corpus  run in 5 seconds on my laptop, for example to
count the number of call-sites to a particular symbol.
Larger analyses that require parsing can take ~60 seconds, YMMV.

To run analyses locally, first download https://drive.google.com/open?id=0B5BBsTPBxGcMYnlXSk5KUk40Q0k

```
cd scala-experiments
unzip -d target/v13 path/to/semanticdb.v13.zip
sbt run
```
A report is printed out to console and written to markdown file under
`target/report-<TIMESTAMP>.md`.
