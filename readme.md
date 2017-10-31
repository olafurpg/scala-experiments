## Large-scale semantic analysis of Scala source code

This repo contains a minimal skeleton to run semantic analysis on
a corpus of 2.5 million lines of Scala code.
Simple analyses on the full corpus (for example, count the number of
call-sites to a particular symbol)
run in 5 seconds for the entire corpus on my laptop.
Larger analyses that require parsing can take ~60 seconds, YMMV.

First, download https://drive.google.com/open?id=0B5BBsTPBxGcMYnlXSk5KUk40Q0k

```
cd scala-experiments
unzip -d target/v12 path/to/semanticdb.v12.zip
sbt run
```
A report is printed out to console and written to markdown file under
`target/report-<TIMESTAMP>.md`.
