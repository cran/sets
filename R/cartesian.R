set_cartesian <-
function(...)
{
    if (nargs() < 2L)
        return(..1)
    l <- list(...)
    if (!all(len <- sapply(l, length)))
        return(set())
    .make_set_of_tuples_from_list_of_lists(.cartesian_product(l))
}

gset_cartesian <-
function(...)
{
    ## handle arguments
    if (nargs() < 2L)
        return(..1)
    l <- lapply(list(...), as.list)
    if (all(sapply(l, gset_is_set)))
        return(as.gset(set_cartesian(...)))

    ## handle empty sets
    if (!all(sapply(l, length)))
        return(gset())

    ## compute cartesian products of support and memberships
    support <- .make_set_of_tuples_from_list_of_lists(.cartesian_product(l))
    memberships <- lapply(l, .get_memberships)
    memberships <- do.call(Map, c("list", .cartesian_product(memberships)))


    ## compute tuple memberships by applying the T-norm to the components
    memberships <-
        if (all(sapply(l, gset_is_crisp)))
            sapply(memberships, function(i) prod(unlist(i)))
        else if (all(sapply(l, gset_is_fuzzy_set))) {
            sapply(memberships, function(i) Reduce(.T., unlist(i)))
        } else {
            lapply(memberships, function(i) {
                ## normalize memberships
                maxlen <- max(sapply(i, length))
                m <- lapply(i, .expand_membership, len = maxlen, rep = FALSE)
                mult <- unlist(do.call(Map, c(list(prod),
                                              lapply(m, .get_memberships)
                                              ))
                               )

                ## compute T-norm
                S <- Reduce(.T., m)
                .make_gset_from_support_and_memberships(as.list(S), mult)
            })
        }
    ## create target set
    .make_gset_from_support_and_memberships(support, memberships)
}

cset_cartesian <-
function(...)
{
    ## handle arguments
    if (nargs() < 2L)
        return(..1)
    l <- list(...)

    ## check match and order-functions
    matchfun <- .check_matchfun(l)
    orderfun <- .check_orderfun(l)

    ## delegate to gset_cartesian and transform afterwards
    cset(gset_cartesian(...), matchfun = matchfun, orderfun = orderfun)
}
