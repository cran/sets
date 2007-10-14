c.set <-
set_union <-
function(...)
    .set_unique(do.call(c, lapply(list(...), unclass)))

c.gset <-
gset_union <-
function(...)
{
    l <- list(...)

    ## compute target support
    support <- do.call(set_union, l)

    ## apply connector
    .make_gset_from_list_of_gsets_and_support_and_connector(l, support, .S.)
}

