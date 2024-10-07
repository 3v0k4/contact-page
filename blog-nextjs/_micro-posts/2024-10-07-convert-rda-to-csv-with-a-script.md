---
title: Convert RDA to CSV with a script
description:
author: Riccardo
tags:
  - Statistics
---

Throw the following into `rda2csv.r`:

```r
#!/usr/bin/env Rscript

argv <- commandArgs(TRUE)
inFile <- toString(argv[1])
print(paste("Reading:", inFile))

outFile <- gsub(".rda$", ".csv", inFile)
print(paste("Writing:", outFile))

inData <- get(load(inFile))
write.csv(inData, file=outFile)
```

Make it executable: `chmod +x rda2csv.r`

And run it on an RDA file: `./rda2csv.r path/to/file.rda`
