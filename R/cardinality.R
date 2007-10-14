gset_cardinality <-
length.gset <-
function(x)
{
    if (gset_is_set(x))
        length(unclass(x))
    else if (gset_is_multiset(x) || gset_is_fuzzy_set(x))
        sum(.get_memberships(x))
    else
        sum(sapply(.get_memberships(x),
                   function(i) crossprod(as.double(i), .get_memberships(i))))
}

