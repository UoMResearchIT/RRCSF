
## Requirements

Install required packages manually:

```{r}
install.packages("argparser")
install.packages("doParallel")
install.packages("doFuture")
install.packages("foreach")
```

Or use `renv` to detect and install depenedencies:

```{r}
install.packages("renv")
renv::init()
```

## Running tests

From an interactive session, run:

```{r}
Rscript serial.R [--iter 24] [--sleep 0.1]
Rscript parallel.R [--iter 24] [--sleep 0.1] [--cores NA]
```

The default `--cores NA` will use all available cores on a local machine,
or `$NSLOTS` on a cluster. 

