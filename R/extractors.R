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
        if (is_element(e))
            e <- list(e)
        ret <- .exact_match(e, x)
        vals <- !is.na(ret)
        if (any(vals))
            ret[vals] <- .get_memberships(x)[ ret[vals] ]
        if (is.list(ret) && (length(ret) < 2L))
            ret <- ret[[1L]]
        ret
    }
}

## internal stuff

.get_memberships <-
function(x)
{
    m <- attr(x, "memberships")
    if (is.null(m)) rep(1L, length(as.list(x))) else m
}

.get_fuzzy_multi_memberships <-
function(x)
{
    if (gset_is_multiset(x))
        lapply(.get_memberships(x), function(i) gset(1L, i))
    else
        .get_memberships(x)
}

.get_support <-
function(x)
{
    ret <- unlist(x, recursive = FALSE)
    if (is.null(ret))
        list()
    else if (is.recursive(ret))
        as.list(x)
    else
        ret
}

