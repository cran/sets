gset_sum <-
function(...)
{
    l <- list(...)

    ## determine connector
    CON <- if (all(sapply(l, gset_is_crisp)))
        `+`
    else
        function(x, y) pmin(1, x + y)

    ## compute target support
    support <- do.call(set_union, l)

    ## apply connector
    .make_gset_from_list_of_gsets_and_support_and_connector(l, support, CON)

}

gset_difference <-
function(...)
{
    l <- list(...)

    ## connector
    CON <- function(x, y) pmax(0, x - y)

    ## compute target support
    support <- do.call(set_union, l)

    ## apply connector
    .make_gset_from_list_of_gsets_and_support_and_connector(l, support, CON)

}

