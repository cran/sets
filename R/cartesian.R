set_cartesian <-
function(...)
{
    if (nargs() < 2L)
        return(..1)
    l <- list(...)
    if (!all(len <- lengths(l)))
        return(set())
    as.set(.make_list_of_tuples_from_list_of_lists(.cartesian_product(l)))
}

### FIXME: does it make sense to add a na.rm argument here?
### For fuzzy multisets, we are using .T. which has no na.rm ...
gset_cartesian <-
function(...)
{
    ## handle arguments
    if (nargs() < 2L)
        return(..1)
    ll <- list(...)
    if (isTRUE(all(gset_is_set(ll))))
        return(as.gset(set_cartesian(...)))

    ## handle empty sets
    if (any(vapply(ll, gset_cardinality, double(1L)) == 0, na.rm = TRUE))
        return(gset())

    l <- lapply(ll, as.list)

    ## compute cartesian products of support and memberships
    support <- .make_list_of_tuples_from_list_of_lists(.cartesian_product(l))
    memberships <- lapply(l, .get_memberships)
    memberships <- do.call(Map, c("list", .cartesian_product(memberships)))


    ## compute tuple memberships by applying the T-norm to the components
    memberships <-
        if (all(gset_is_crisp(ll), na.rm = TRUE))
            vapply(memberships, function(i) prod(unlist(i)), integer(1L))
        else if (all(gset_is_fuzzy_set(ll), na.rm = TRUE)) {
            vapply(memberships, function(i) Reduce(.T., unlist(i)), double(1L))
        } else {
            lapply(memberships, function(i) {
                ## normalize memberships
                maxlen <- max(vapply(i, gset_cardinality, double(1L), na.rm = TRUE))
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
    gset_cartesian(...)
