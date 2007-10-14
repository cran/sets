## set predicates

is.set <-
function(x)
    inherits(x, "set")

set_is_empty <-
function(x)
{
    if(is.set(x))
        length(x) < 1L
    else
        sapply(x, length) < 1L
}

set_is_subset <-
function(x, y)
{
    .help <- function(a, b) set_is_empty(set_complement(b, a))
    if(is.set(x))
        .help(x, y)
    else
        Vectorize(.help)(x, y)
}

set_is_proper_subset <-
function(x, y)
{
    set_is_subset(x, y) &
    if(is.set(x))
        length(x) != length(y)
    else
        sapply(x, length) != sapply(y, length)
}

set_is_equal <-
function(x, y)
{
    .help <- function(a, b)
        ((length(a) == length(b))
         && (length(set_intersection(a,b)) == length(a)))
    if(is.set(x))
        .help(x, y)
    else
        Vectorize(.help)(x, y)
}

"%e%" <-
function(e, x)
    set_contains_element(x, e)

set_contains_element <-
function(x, e)
{
    if(set_is_empty(x))
        return(FALSE)
    if(is.set(e)) {
        if(!any(sapply(x, is.set)))
            stop("Set to look into does not contain any set.")
        e <- list(e)
    }
    if(is.tuple(e))
        e <- list(e)
    e %in% x
}



## gset-predicates

is.gset <-
function(x)
    inherits(x, c("gset", "set"))

gset_is_empty <-
function(x)
{
    if(is.gset(x))
        length(x) == 0
    else
        sapply(x, length) == 0
}

gset_is_subset <-
function(x, y)
{
    .help <- function(a, b) gset_is_empty(gset_complement(b, a))
    if(is.gset(x))
        .help(x, y)
    else
        Vectorize(.help)(x, y)
}

gset_is_proper_subset <-
function(x, y)
{
    gset_is_subset(x, y) &
    if(is.gset(x))
        length(x) != length(y)
    else
        sapply(x, length) != sapply(y, length)
}

gset_is_equal <-
function(x, y)
{
    .help <- function(a, b)
        ((length(a) == length(b))
         && (length(gset_intersection(a,b)) == length(a)))
    if(is.gset(x))
        .help(x, y)
    else
        Vectorize(.help)(x, y)
}

"%e%" <-
function(e, x)
    gset_contains_element(x, e)

gset_contains_element <-
function(x, e)
{
    if(gset_is_empty(x))
        return(FALSE)
    if(is.gset(e)) {
        if(!any(sapply(x, is.gset)))
            stop("Set to look into does not contain any (g)set.")
        e <- list(e)
    }
    if(is.tuple(e))
        e <- list(e)
    e %in% x
}

gset_is_multiset <-
function(x)
{
     m <- .get_memberships(x)
    !is.list(m) && any(m > 1)
}

gset_is_fuzzy_set <-
function(x)
{
    m <- .get_memberships(x)
    !is.list(m) && (any(m < 1))
}

gset_is_fuzzy_multiset <-
function(x)
    is.list(.get_memberships(x))

gset_is_set <-
function(x)
{
    m <- .get_memberships(x)
    !is.list(m) && all(m == 1)
}

gset_is_crisp <-
function(x)
{
     m <- .get_memberships(x)
    !is.list(m) && all(m >= 1)
}

