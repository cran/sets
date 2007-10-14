set_outer <-
function(X, Y, FUN = "*", ..., SIMPLIFY = TRUE)
{
    ## convenience
    nx <- deparse(substitute(X))
    if(missing(Y)) {
        Y <- X
        ny <- nx
    } else if(is.function(Y) || is.character(Y)) {
        FUN <- Y
        Y <- X
        ny <- nx
    } else ny <- deparse(substitute(Y))

    FUN <- match.fun(FUN)

    ## loop
    xrep <- rep(unclass(X), times = (ylen <- length(Y)))
    yrep <- rep(unclass(Y), each = (xlen <- length(X)))
    ret <- mapply(FUN, xrep, yrep, MoreArgs = list(...), SIMPLIFY = FALSE)

    ## simplify if sensible
    if(SIMPLIFY && all(sapply(ret, is.atomic)))
        ret <- unlist(ret, recursive = FALSE)

    ## make matrix
    dim(ret) <- c(xlen, ylen)
    dimnames(ret) <- list(LABELS(X), LABELS(Y))
    ret
}

gset_outer <-
    function(...) set_outer(...)

