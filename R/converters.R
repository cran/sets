### * converters

### SET CONVERTERS
### We basically have make_set_with_order converters
### which return the resulting set and the original order
### to allow the permutation of some associated meta information
### according to the new order.
### (Examples: memberships of generalized sets and incidences of relations.)
### The as.set converter calls these internally and
### strips the ordering information.

make_set_with_order <-
function(x)
    UseMethod("make_set_with_order")

make_set_with_order.default <-
function(x)
    stop("Not implemented.")

make_set_with_order.set <-
function(x)
    .make_set_with_order(x)

make_set_with_order.gset <-
function(x)
    .make_set_with_order(.make_set_from_list(.get_support(x)))

make_set_with_order.numeric <-
make_set_with_order.factor <-
make_set_with_order.character <-
make_set_with_order.integer <-
make_set_with_order.ordered <-
make_set_with_order.logical <-
make_set_with_order.tuple <-
make_set_with_order.list <-
function(x)
{
    x <- .list_unique(x)
    O <- .list_order(x)
    .make_set_with_order(.make_set_from_list(x[O]), O)
}

make_set_with_order.data.frame <-
function(x) {
    x <- unique(x)
    O <- do.call(order, x)
    .make_set_with_order(.make_set_from_list(lapply(split(x, rownames(x)),
                                                    as.tuple)), O)
}

.make_set_with_order <-
function(set, order = seq_along(set))
    list(set = set, order = order)

### High-Level converter

as.set <-
function(x)
    make_set_with_order(x)$set

### canonicalizer

canonicalize_set_and_mapping <-
function(x, mapping = NULL, margin = NULL)
{
    x <- make_set_with_order(x)
    if (!is.null(mapping))
        mapping <- if (is.array(mapping) || is.data.frame(mapping)) {
            D <- dim(mapping)
            L <- length(x$set)
            if (is.null(margin))
                margin <- which(D == L)
            permute <- rep.int(list(seq_len(L)), length(D))
            permute[margin] <- rep(list(x$order), length(margin))
            do.call("[", c(list(mapping), permute, list(drop = FALSE)))
        } else
            mapping[x$order]

    list(set = x$set, mapping = mapping, order = x$order)
}

###

format.set <-
function(x, ...) {
    .format_set_or_tuple(x, "{", "}", ...)
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

as.gset.gset <- identity

as.gset.tuple <-
function(x)
    as.gset(as.list(x))

as.gset.numeric <-
function(x)
{
    ## floating point numbers are a real mess!
    tab <- table(as.character(x))
    .make_gset_from_support_and_memberships(as.numeric(names(tab)),
                                            as.vector(tab))
}

as.gset.factor <-
as.gset.character <-
as.gset.integer <-
as.gset.ordered <-
as.gset.logical <-
function(x)
    .make_gset_from_support_and_memberships(.list_sort(.list_unique(x)),
                                            as.vector(table(x)))

as.gset.list <-
function(x)
{
    uni <- unique(x)
    count <- colSums(set_outer(x, uni, identical))
    .make_gset_from_support_and_memberships(uni, count)
}

as.gset.data.frame <-
function(x)
    as.gset(lapply(split(x, rownames(x)), as.tuple))

as.list.gset <-
function(x, ...)
    unclass(x)

### tuple converters

as.tuple <-
function(x)
    UseMethod("as.tuple")

as.tuple.default <-
function(x)
    stop("Not implemented!")

as.tuple.tuple <- identity

as.tuple.numeric <-
as.tuple.factor <-
as.tuple.character <-
as.tuple.integer <-
as.tuple.ordered <-
as.tuple.logical <-
function(x)
    .make_tuple_from_list(as.list(x))

as.tuple.set <-
as.tuple.list <-
function(x)
    do.call(tuple, x)

as.tuple.data.frame <-
function(x)
{
    ret <- as.list(x)
    attributes(ret) <- NULL
    names(ret) <- colnames(x)
    .make_tuple_from_list(ret)
}

as.list.tuple <-
function(x, ...)
    unclass(x)

as.list.function <-
function(x, ...)
    list(x)

