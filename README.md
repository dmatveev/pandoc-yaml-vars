# pandoc-yaml-vars
Pandoc filter which substitutes %variables% in the input file with values from supplied YAML file.

Usage:
```
VARSFILE=SOMEFILE.yaml pandoc --filter=./pandoc-yaml-vars INFILE -o OUTFILE
```

The variable dictionary should follow simple YAML-like format:
```
variable-name : Variable value
```

Object structures also supported:
```
---
othervar123 : foo bar baz
123othervar : bar baz foo
...
```

In this version, only actual text in document nodes is being processed (attributes are skipped).
