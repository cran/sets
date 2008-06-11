### * as.list.function

## Make sure that as.list() on functions always returns an object of
## length one, so that LABELS() remains happy.

as.list.function <-
function(x, ...)
    list(x)

### * .cartesian_product

.cartesian_product <-
function(x)
{
    ## Cf. expand.grid().
    out <- vector("list", length(x))
    rep_fac <- 1L
    d <- sapply(x, length)
    orep <- prod(d)
    for(i in seq_along(x)) {
        nx <- d[i]
        orep <- orep / nx
        out[[i]] <-
            as.list(x[[i]])[rep.int(rep.int(seq_len(nx),
                                            rep.int(rep_fac, nx)), orep)]
        rep_fac <- rep_fac * nx
    }
    out
}

### * .make_set_of_tuples_from_list_of_lists

.make_set_of_tuples_from_list_of_lists <-
function(x)
{
    ret <- unlist(x, recursive = FALSE)
    dim(ret) <- c(length(x[[1L]]), length(x))
    as.set(apply(ret, 1L, as.tuple))
}

### make sure that list elements are not destroyed during set unions

.make_list_elements <-
function(i)
    if (!is.gset(i)) list(i) else i

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***

### exact_match

.exact_match <-
function(x, table)
{
    table <- as.list(table)
    FUN <- function(i) {
        ind <- unlist(lapply(table, identical, i))
        if (any(ind)) seq_along(ind)[ind][1] else NA
    }
    unlist(lapply(as.list(x), FUN))
}
