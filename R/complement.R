set_complement <-
function(x, y)
{
    if (missing(y))
        return(set())
    y <- as.list(y)
    ind <- unique(na.omit(.exact_match(x, y)))
    .make_set_from_list(if(length(ind)) y[-ind] else y)
}

gset_complement <-
function(x, y = NULL)
{
    ## auto-complement
    if (is.null(y)) {
        if (gset_is_crisp(x))
            return(gset())
        else if (gset_is_fuzzy_set(x))
            return(.make_gset_from_support_and_memberships(.get_support(x),
                                                          .N.(.get_memberships(x))))
        else {
            memberships <- lapply(.get_memberships(x), function(i) {
                .make_gset_from_support_and_memberships(lapply(.get_support(i), .N.),
                                                       .get_memberships(i))
            })
            return(.make_gset_from_support_and_memberships(.get_support(x),
                                                           memberships))
        }
    }

    ## For "ordinary sets", call set function
    if (gset_is_set(x) && gset_is_set(y))
        return(set_complement(x, y))

    ## compute target support
    support <- set_union(x, y)

    ## extract memberships, normalized for target support
    m_x <- .memberships_for_support(x, support)
    m_y <- .memberships_for_support(y, support)

    memberships <-
    if (gset_is_fuzzy_set(x) && gset_is_fuzzy_set(y)) {
        .T.(.N.(m_x), m_y)
    } else {
        ## expand memberships
        maxlen <- max(if (is.list(m_x)) sapply(m_x, length) else m_x,
                      if (is.list(m_y)) sapply(m_y, length) else m_y)
        m_x <- lapply(m_x, .expand_membership, len = maxlen)
        m_y <- lapply(m_y, .expand_membership, len = maxlen)

        ## compute target memberships
        lapply(seq_along(m_x),
               function(i) .T.(.N.(m_x[[i]]), m_y[[i]]))
    }

    ## simplify multiset/multiset-case
    if (gset_is_crisp(x) && gset_is_crisp(y))
        memberships <- sapply(memberships, sum)

    ## return resulting gset
    .make_gset_from_support_and_memberships(support,
                                            .canonicalize_memberships(memberships))
}

