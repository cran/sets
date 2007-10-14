set_combn <-
function(x, m)
{
    if (m == 0)
        set()
    else
        do.call(set, apply(combn(as.list(x), m), 2L, as.set))
}

gset_combn <-
function(x, m)
{
    if (m == 0)
        gset()
    else {
        support <- apply(combn(unclass(x), m), 2L, as.set)
        memberships <- unlist(apply(combn(.get_memberships(x), m), 2L, list),
                              recursive = FALSE)
        .make_gset_from_list(Map(gset, support, memberships))
    }
}

