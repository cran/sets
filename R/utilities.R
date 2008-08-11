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
            .as.list(x[[i]])[rep.int(rep.int(seq_len(nx),
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
    if (!is.cset(i)) list(i) else i

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***

### exact_match

.make_matchfun_from_equalityfun <-
function(equalityfun)
{
    equalityfun <- match.fun(equalityfun)
    function(x, table)
    {
        table <- .as.list(table)
        FUN <- function(i) {
            ind <- unlist(lapply(table, equalityfun, i))
            if (any(ind)) seq_along(ind)[ind][1] else NA
        }
        ret <- lapply(.as.list(x), FUN)
        if (length(ret) < 1L) integer() else unlist(ret)
    }
}

.exact_match <-
    .make_matchfun_from_equalityfun(identical)

### .list_order/sort/unique

.list_order <-
function(x, decreasing = FALSE, ...) {
    .as.character <-
        function(x) if(is.factor(x)) as.character(x) else x
    ch <- as.character(lapply(.as.list(x), .as.character))

    if (capabilities("iconv")) {
        loc <- Sys.getlocale("LC_COLLATE")
        warn <- options()$warn
        on.exit({Sys.setlocale("LC_COLLATE", loc); options(warn = warn)})
        options(warn = -1)
        Sys.setlocale("LC_COLLATE", "C")
        ch <- iconv(ch, to = "UTF-8")
    }

    order(sapply(x, length),
          sapply(x, typeof),
          ch,
          decreasing = decreasing, ...)
}

.list_sort <-
function(x, decreasing = FALSE, ...)
    .as.list(x)[.list_order(x, decreasing = decreasing, ...)]

.list_unique <-
function(x)
    .as.list(x)[!duplicated(x)]

