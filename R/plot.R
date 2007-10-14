### * plot function
plot.gset <-
function(x, ylim = NULL, ...)
{
    m <- .get_memberships(x)
    if (gset_is_fuzzy_multiset(x)) {
        maxlen <- max(sapply(m, length))
        m <- sapply(m, .expand_membership, len = maxlen)
    }

    if (is.null(ylim) && (gset_is_fuzzy_set(x) || gset_is_fuzzy_multiset(x)))
        ylim <- c(0,1)

    barplot(m,
            ylim = ylim,
            names.arg = LABELS(x),
            beside = TRUE,
            ...)
}

