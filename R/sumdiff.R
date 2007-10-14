gset_sum <-
function(...)
{
    .gset_sum <- function(x, y)
    {
        ## compute resulting support
        support <- set_union(x, y)
        ind_x <- match(support, .get_support(x))
        ind_y <- match(support, .get_support(y))

        ## compute memberships
        memberships <-
            if (gset_is_crisp(x) && gset_is_crisp(y))
                ## handle simple (crisp) case
                colSums(rbind(.get_memberships(x)[ind_x],
                              .get_memberships(y)[ind_y]),
                        na.rm = TRUE)
            else  ## handle the most general case
                Map(.gset_sum,
                    .get_memberships(x)[ind_x],
                    .get_memberships(y)[ind_y])

        .make_gset_from_support_and_memberships(support = support,
                                                memberships = memberships)
    }
    Reduce(.gset_sum, list(...))
}

gset_difference <-
function(x, y)
{
    ## compute resulting support
    sup_x <- .get_support(x)
    ind_y <- match(sup_x, .get_support(y))

    ## compute memberships
    memberships <-
        if (gset_is_crisp(x) && gset_is_crisp(y))
            ## handle simple (crisp) case
            memberships <- pmax(0,
                                colSums(rbind(.get_memberships(x),
                                              - .get_memberships(y)[ind_y]),
                                        na.rm = TRUE)
                                )
        else ## handle the most general case
            Map(gset_difference,
                .get_memberships(x),
                .get_memberships(y)[ind_y])

    .make_gset_from_support_and_memberships(support = as.set(sup_x),
                                            memberships = memberships)
}

