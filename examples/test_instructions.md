
## Requirements / Setup

Use `renv` to install dependencies from the `DESCRIPTION` file:

```{r}
install.packages("renv")
renv::init(settings = list(snapshot.type="explicit"))
```

Or install required packages manually:

```{r}
install.packages("argparser", dependencies=TRUE)
install.packages("doFuture", dependencies=TRUE)
install.packages("foreach")
install.packages("progressr")
```

## Running tests

From an interactive session, run:

```{r}
Rscript parallel_tester.R [--iter 24] [--sleep 0.1] [--cores NA]
```

The default `--cores NA` will use all available cores on a local machine,
or `$NSLOTS` on a cluster. 

