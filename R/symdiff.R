set_symdiff <-
function(...)
{
    len <- length(l <- list(...))
    SD <- function(x, y) set_union(set_complement(x, y), set_complement(y, x))

    if (len < 1L)
        gset()
    else if (len < 2L)
        l[[1L]]
    else
        Reduce(SD, l)
}

"%D%" <-
gset_symdiff <-
function(...)
{
    len <- length(l <- list(...))
    SD <- function(x, y) gset_union(gset_complement(x, y), gset_complement(y, x))
    if (len < 1L)
        gset()
    else if (len < 2L)
        l[[1L]]
    else
        Reduce(SD, l)
}

