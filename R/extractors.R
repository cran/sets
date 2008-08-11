## extractors

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
    ret <- function(e) {
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
    structure(ret, class = "gset_charfun")
}

print.gset_charfun <-
function(x, ...)
{
    writeLines("The characteristic function of a generalized set.")
    invisible(x)
}

### * cset extractors

cset_memberships <-
function(x)
{
    if (!is.cset(x))
        stop("Argument 'x' must be a customizable set.")
    .get_memberships(x)
}

cset_support <-
function(x)
{
    if (!is.cset(x))
        stop("Argument 'x' must be a customizable set.")
    as.set(.get_support(x))
}

cset_charfun <-
function(x)
{
    if (!is.cset(x))
        stop("Argument 'x' must be a customizable set.")
    matchfun <- cset_matchfun(x)
    ret <- function(e) {
        if (is_element(e))
            e <- list(e)
        ret <- matchfun(e, x)
        vals <- !is.na(ret)
        if (any(vals))
            ret[vals] <- .get_memberships(x)[ ret[vals] ]
        if (is.list(ret) && (length(ret) < 2L))
            ret <- ret[[1L]]
        ret
    }
    structure(ret, class = "cset_charfun")
}

print.cset_charfun <-
function(x, ...)
{
    writeLines("The characteristic function of a customizable set.")
    invisible(x)
}

cset_orderfun <-
function(x)
{
    FUN <- .orderfun(x)
    if (is.null(FUN))
        .list_order
    else
        FUN
}

"cset_orderfun<-" <-
function(x, value) {
    attr(x, "orderfun") <- value
    x
}

cset_matchfun <-
function(x)
{
    FUN <- .matchfun(x)
    if (is.null(FUN))
        .exact_match
    else
        FUN
}

"cset_matchfun<-" <-
function(x, value)
    cset(x, matchfun = value, orderfun = .orderfun(x))

## internal stuff

.get_memberships <-
function(x)
{
    m <- attr(x, "memberships")
    if (is.null(m)) rep(1L, length(.as.list(x))) else m
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
    ## simplify, if all elements are of same class
    cl <- unlist(lapply(x, class))
    ret <- if (all(cl[1] == cl))
        unlist(x, recursive = FALSE)
    else
        x

    if (is.null(ret))
        list()
    else if (is.recursive(ret))
        .as.list(x)
    else
        ret
}

.orderfun <-
function(x)
    attr(x, "orderfun")

.matchfun <-
function(x)
    attr(x, "matchfun")

