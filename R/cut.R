cut.gset <-
function(x, level = 1, type = c("alpha", "nu"), strict = FALSE, ...)
{
    type <- match.arg(type)

    support <- .as.list(.get_support(x))
    memberships <- .get_memberships(x)

    if (type == "alpha") {
        if (gset_is_crisp(x))
            return(x)

        if (gset_is_fuzzy_set(x)) {
            ind <- if (strict)
                memberships > level
            else
                memberships >= level

            if (length(ind) > 0L)
                .make_set_from_list(support[ind])
            else
                set()
        } else {
            M <- sapply(memberships, if (strict)
                        function(i) sum(unlist(i) > level)
            else
                        function(i) sum(unlist(i) >= level)
                        )
            ind <- M > 0
            if (!any(ind))
                gset()
            else if (all(M == 1L))
                .make_set_from_list(support[ind])
            else
                .make_gset_from_support_and_memberships(support[ind], M[ind])
        }
    } else {
        if (gset_is_set_or_fuzzy_set(x)) {
            if (level > 1)
                set()
            else
                x
        } else if (gset_is_multiset(x)) {
            ind <- if (strict)
                memberships > level
            else
                memberships >= level

            if (length(ind) > 0L)
                .make_set_from_list(support[ind])
            else
                set()

        } else {
            M <- sapply(memberships,
                        function(i) .expand_membership(i)[level + strict])
            M[is.na(M)] <- 0
            .make_gset_from_support_and_memberships(support, M)
        }
    }
}

cut.cset <-
function(x, level = 1, type = c("alpha", "nu"), strict = FALSE, ...)
    cut.gset(x, level = level, ...)
