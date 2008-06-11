set_intersection <-
function(...)
{
    len <- length(l <- list(...))
    if(len < 1L)
        set()
    else if(len < 2L)
        l[[1L]]
    else if(len < 3L)
        .make_set_from_list(as.list(l[[2L]])[unique(na.omit(.exact_match(l[[1L]],
                                                                         l[[2L]])))])
    else
        do.call(Recall, c(l[1L], list(do.call(Recall, l[-1L]))))
}

gset_intersection <-
function(...)
{
    l <- list(...)

    ## compute target support
    support <- do.call(set_intersection, l)

    ## handle the "ordinary set" case
    if (all(sapply(l, gset_is_set)))
        return(support)

    ## apply connector
    .make_gset_from_list_of_gsets_and_support_and_connector(l, support, .T.)
}

