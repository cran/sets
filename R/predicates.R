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

"%e%" <-
function(e, x) {
    if (is.set(x))
        set_contains_element(x, e)
    else
        gset_contains_element(x, e)
}


gset_contains_element <-
function(x, e)
    set_contains_element(.make_list_of_elements_from_gset(x), e(e))

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

