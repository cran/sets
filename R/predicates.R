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

set_contains_element <-
function(x, e)
{
    if(set_is_empty(x))
        return(FALSE)
    if(is.tuple(e) || is.gset(e) || is_element(e))
        e <- list(e)
    !is.na(.exact_match(e, x))
}

## gset-predicates

is_element <-
function(e)
    inherits(e, "element")

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

gset_contains_element <-
function(x, e)
    set_contains_element(as.set(.make_list_of_elements_from_cset(x)), e(e))

gset_is_multiset <-
function(x)
{
    m <- .get_memberships(x)
    !is.list(m) && any(m > 1)
}

gset_is_crisp <-
gset_is_set_or_multiset <-
function(x)
{
    m <- .get_memberships(x)
    !is.list(m) && all(m >= 1)
}

gset_is_fuzzy_set <-
function(x)
{
    m <- .get_memberships(x)
    !is.list(m) && (any(m < 1))
}

gset_is_set_or_fuzzy_set <-
function(x)
{
    m <- .get_memberships(x)
    !is.list(m) && (all(m <= 1))
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

## cset-predicates

is.cset <-
function(x)
    inherits(x, c("cset", "gset", "set"))

cset_is_empty <-
function(x)
{
    if(is.cset(x))
        length(x) == 0
    else
        sapply(x, length) == 0
}

cset_is_subset <-
function(x, y)
{
    .help <- function(a, b) cset_is_empty(cset_complement(b, a))
    if(is.cset(x))
        .help(x, y)
    else
        Vectorize(.help)(x, y)
}

cset_is_proper_subset <-
function(x, y)
{
    cset_is_subset(x, y) &
    if(is.cset(x))
        length(x) != length(y)
    else
        sapply(x, length) != sapply(y, length)
}

cset_is_equal <-
function(x, y)
{
    .help <- function(a, b)
        ((length(a) == length(b))
         && (length(cset_intersection(a,b)) == length(a)))
    if(is.cset(x))
        .help(x, y)
    else
        Vectorize(.help)(x, y)
}

cset_contains_element <-
function(x, e)
{
    if(cset_is_empty(x))
        return(FALSE)
    matchfun <- cset_matchfun(x)
    x <- .make_list_of_elements_from_cset(x)
    e <- e(e)
    if(is.tuple(e) || is.cset(e) || is_element(e))
        e <- list(e)
    ind <- matchfun(e, x)
    if(is.na(ind)) return(FALSE)
    .get_memberships(x[[ind]]) == .get_memberships(e[[1]])
}

cset_is_multiset <-
function(x)
    gset_is_multiset(x)

cset_is_crisp <-
cset_is_set_or_multiset <-
function(x)
    gset_is_crisp(x)

cset_is_fuzzy_set <-
function(x)
    gset_is_fuzzy_set(x)

cset_is_set_or_fuzzy_set <-
function(x)
    gset_is_set_or_fuzzy_set(x)

cset_is_fuzzy_multiset <-
function(x)
    gset_is_fuzzy_multiset(x)

cset_is_set <-
function(x)
    gset_is_set(x)

## contains_element operator dispatch

"%e%" <-
function(e, x)
{
    if(is.set(x))
        set_contains_element(x, e)
    else if(is.gset(x))
        gset_contains_element(x, e)
    else
        cset_contains_element(x, e)
}

