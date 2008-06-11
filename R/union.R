c.set <-
set_union <-
function(...)
    do.call(set,
            unlist(lapply(list(...), .make_list_elements), recursive = FALSE))

c.gset <-
gset_union <-
function(...)
{
    l <- list(...)

    ## compute target support
    support <- do.call(set_union, l)

    ## handle the "ordinary set" case
    if (all(sapply(l, gset_is_set)))
        return(support)
    
    ## apply connector
    .make_gset_from_list_of_gsets_and_support_and_connector(l, support, .S.)
}

