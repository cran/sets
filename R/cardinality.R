set_cardinality <-
length.set <-
function(x)
    length(.as.list(x))

gset_cardinality <-
function(x, type = c("absolute", "relative"))
{
    type <- match.arg(type)

    ret <- if (gset_is_set(x))
        length.set(x)
    else if (gset_is_multiset(x) || gset_is_fuzzy_set(x))
        sum(.get_memberships(x))
    else
        sum(sapply(.get_memberships(x),
                   function(i) crossprod(as.double(i), .get_memberships(i))))

    if (type == "absolute")
        ret
    else
        ret / length.set(x)
}

length.gset <-
length.cset <-
function(x)
    gset_cardinality(x)

cset_cardinality <-
function(x, type = c("absolute", "relative"))
    gset_cardinality(x, type)

