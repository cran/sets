### * converters

### set converters

as.set <-
function(x)
    UseMethod("as.set")

as.set.default <-
function(x)
    stop("Not implemented.")

as.set.set <- .identity

as.set.gset <-
function(x)
    gset_support(x)

as.set.tuple <-
function(x)
    do.call(set, unclass(x))

as.set.numeric <-
as.set.factor <-
as.set.character <-
as.set.integer <-
as.set.ordered <-
as.set.logical <-
function(x)
    .set_unique(as.list(x))

as.set.list <-
function(x)
    .set_unique(.make_set_from_list(x))

as.set.data.frame <-
function(x)
    .make_set_from_list(lapply(split(x, seq_len(nrow(x))), as.tuple))

format.set <-
function(x, ...) {
    .format_set_or_tuple(x, "{", "}")
}

as.list.set <-
function(x, ...)
    unclass(x)

### gset converters

### for the time being, just call as.set() via the gset() default

as.gset <-
function(x)
    UseMethod("as.gset")

as.gset.default <-
function(x)
    gset(x)

as.gset.gset <- .identity

as.gset.tuple <-
function(x)
    as.gset(unclass(x))

as.gset.numeric <-
function(x)
{
    ## floating point numbers are a real mass!
    x <- as.character(x)
    gset(as.numeric(sort(unique(x))), memberships = table(x))
}

as.gset.factor <-
as.gset.character <-
as.gset.integer <-
as.gset.ordered <-
as.gset.logical <-
function(x)
    gset(sort(unique(x)), memberships = table(x))

as.gset.list <-
function(x)
{
    uni <- unique(x)
    count <- colSums(set_outer(x, uni, identical))
    .make_gset_from_support_and_memberships(support = uni, memberships = count)
}

as.gset.data.frame <-
function(x)
    as.gset(as.set(x))

as.list.gset <-
function(x, ...)
    unclass(x)

