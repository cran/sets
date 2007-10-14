### * extractors
gset_memberships <-
function(x)
{
    if (!is.gset(x))
        stop("Argument 'x' must be a generalized set.")
    .get_memberships(x)
}

gset_support <-
function(x)
{
    if (!is.gset(x))
        stop("Argument 'x' must be a generalized set.")
    as.set(.get_support(x))
}

gset_charfun <-
function(x)
{
    if (!is.gset(x))
        stop("Argument 'x' must be a generalized set.")
    function(e) {
        ret <- match(e, as.list(x))
        vals <- !is.na(ret)
        if (any(vals))
            ret[vals] <- .get_memberships(x)[ ret[vals] ]
        if (is.list(ret) && (length(ret) < 2L))
            ret <- ret[[1]]
        ret
    }
}

## internal stuff

.get_memberships <-
function(x)
{
    m <- attr(x, "memberships")
    if (is.null(m)) rep(1, length(unclass(x))) else m
}

.get_support <-
function(x)
{
    unlist(x)
}

