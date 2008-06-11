reduction <-
function(x, operation, ...)
    UseMethod("reduction")

reduction.default <-
function(x, operation, ...)
    stop("Not implemented.")

reduction.set <-
function(x, operation = c("union", "intersection"), ...)
{
    if (length(x) < 2L) return(x)
    if (!all(sapply(x, is.gset)))
        stop("reduction only defined on set of (g)sets.")

    if (all(sapply(x, is.set))) {

        dom <- as.list(do.call(set_union, x))
        x <- lapply(x, .make_list_elements)

        members <-
            binary_reduction(do.call(rbind, lapply(x, function(i) dom %in% i)),
                             operation)

        .make_set_from_list(.list_sort(apply(members, 1L,
                                             function(i) .make_set_from_list(dom[i])
                                             )
                                       )
                            )
    } else {
        mem <- gset_memberships(x)
        sup <- gset_support(x)
        if (length(sup) < 3L) return(x)
        operation <- match.arg(operation)
        clo <- closure(x, operation)
        FUN <- function(e) !gset_is_equal(closure(gset_difference(x, e),
                                                  operation), clo)
        index <- sapply(Map(gset,
                            lapply(x, set),
                            if (gset_is_fuzzy_multiset(x)) lapply(mem, list) else mem),
                        FUN)
        gset(as.list(sup)[index], mem[index])

    }
}

binary_reduction <-
function(x, operation = c("union", "intersection"))
    .Call("R_reduction", x,
          pmatch(match.arg(operation), c("union", "intersection")))
