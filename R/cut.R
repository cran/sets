cut.gset <-
function(x, level = 1, ...)
{
    if (gset_is_crisp(x))
        return(x)

    support <- as.list(.get_support(x))
    memberships <- .get_memberships(x)

    if (gset_is_fuzzy_set(x)) {
        ind <- memberships >= level
        if (length(ind) > 0L)
            .make_set_from_list(support[ind])
        else
            set()
    } else {
        M <- sapply(memberships, function(i) sum(unlist(i) >= level))
        ind <- M > 0
        if (!any(ind))
            gset()
        else if (all(M == 1L))
            .make_set_from_list(support[ind])
        else
            .make_gset_from_support_and_memberships(support[ind], M[ind])
    }
}

