######################
### Generalized Sets ###
########################

## The gset class extends 'ordinary' sets by allowing
## membership values for each element. Strictly speaking,
## a generalized set is a pair (A, mu) with support A and
## membership function mu, where mu is a mapping from
## A into 2 ^ ([0, 1] -> N). Special cases are:
## mu : A -> {0, 1} ("ordinary sets")
## mu : A -> [0, 1] ("fuzzy sets")
## mu : A -> 2 ^ {0, 1} ("multi sets")

### Basic stuff (constructors, print/summary methods)

## * gset generator

gset <-
function(support = NULL, memberships = NULL, charfun = NULL, elements = NULL)
{
    ### some checks
    if (!is.null(elements) &&
        !(is.null(support) && is.null(memberships) && is.null(charfun)))
        stop("'elements' needs to be specified alone.")
    if (is.null(support) &&
        (!is.null(memberships) || !is.null(charfun)))
        stop("Need 'support' with 'membership' or 'charfun'.")
    if (!is.null(memberships) && !is.null(charfun))
        stop("Need either 'memberships' or 'charfun' with 'support'.")

    ### element specification:
    ## split support and memberships, and proceed
    if (!is.null(elements)) {
        support <- sapply(elements, .remove_element_class_and_memberships)
        memberships <- lapply(elements, .get_memberships)
        if (all(sapply(memberships, length) == 1L))
            memberships <- do.call(c, memberships)
    }

    ## make sure we have a valid support
    support <- if (is.null(support))
        set()
    else
        as.set(support)

    ### support & charfun specification:
    ## create memberships from charfun, and proceed
    if (!is.null(charfun)) {
        memberships <- sapply(support, charfun)
        .stop_if_memberships_are_invalid(memberships,
                                         "Membership function invalid.\n  ")
    }

    ### support & memberships
    if (!is.null(memberships)) {
        memberships <- .canonicalize_memberships(memberships)
        .stop_if_memberships_are_invalid(memberships)
        if (length(memberships) != length(support))
            stop("Length of support must match length of memberships.")
    }

    .make_gset_from_support_and_memberships(support = support,
                                            memberships = memberships)
}

print.gset <-
function(x, ...)
{
    writeLines(strwrap(format(x), exdent = 1L))
    invisible(x)
}

summary.gset <-
function(object, ...)
{
    len <- length(object)
    out <- if (len == 0L)
        gettext("The empty set.")
    else if (len == 1L)
        gettext("A generalized set with 1 element.")
    else
        gettextf("A generalized set with cardinality %g.", len)
    structure(out, class = "summary.set")
}

print.summary.gset <-
function(x, ...)
{
    writeLines(x)
    invisible(x)
}

format.gset <-
function(x, ...) {
    x <- if (gset_is_set(x))
        lapply(x, .remove_element_class)
    else
        mapply(e, as.list(x), .get_memberships(x), SIMPLIFY = FALSE)
    .format_set_or_tuple(x, "{", "}")
}

sort.gset <-
function(x, decreasing = FALSE, ...)
{
    D <- .get_support(x)
    O <- order(LABELS(D), decreasing = decreasing, ...)
    .make_gset_from_support_and_memberships(support = as.set(D[O]),
                                            memberships = .get_memberships(x)[O])
}

### operators

## Disable numeric subscripting (as sets are "unordered" collections of
## elements).  Note that iterating via for() and lapply() still works,
## the former because this [currently, 2007-09-16] directly uses the
## internal list representation and the latter because we provide an
## as.list() method.

`[.gset` <-
function(x, i)
{
    if(is.numeric(i))
        stop("Numeric subscripting of generalied sets is not defined.")
    .make_gset_from_list(NextMethod("["))
}

`[[.gset` <-
function(x, i)
{
    if(is.numeric(i))
        stop("Numeric subscripting of generalized sets is not defined.")
    NextMethod("[[")
}

Ops.gset <-
function(e1, e2)
{
    if (nargs() == 1L) {
        if(!(as.character(.Generic) %in% "!"))
            stop(gettextf("Unary '%s' not defined for \"%s\" objects.",
                          .Generic, .Class))
        return(gset_complement(e1))
    }

    if(!(as.character(.Generic)
         %in% c("<", "<=", ">", ">=", "==", "!=",
                "&", "|", "*", "+", "-", "^")))
        stop(gettextf("Generic '%s' not defined for \"%s\" objects.",
                      .Generic, .Class))

    if(as.character(.Generic) == "^") {
        if(is.gset(e1) &&
            ((trunc(e2) != e2) || (e2 < 1L)))
            stop("Cartesian product only defined for positive integers.")
        if(is.gset(e2) && (e1 != 2L))
            stop("Operator not defined.")
    }

    switch(.Generic,
           "+"  = gset_sum(e1, e2),
           "|"  = gset_union(e1, e2),
           "-"  = gset_difference(e1, e2),
           "&"  = gset_intersection(e1, e2),
           "*"  = gset_cartesian(e1, e2),
           "<"  = gset_is_proper_subset(e1, e2),
           "<=" = gset_is_subset(e1, e2),
           ">"  = gset_is_proper_subset(e2, e1),
           ">=" = gset_is_subset(e2, e1),
           "==" = gset_is_equal(e1, e2),
           "!=" = !gset_is_equal(e1, e2),
           "^"  = {
               if(is.gset(e2))
                   gset_power(e2)
               else
                   do.call(gset_cartesian, rep(list(e1), e2))}
           )

}

### internal stuff

.make_gset_from_list <-
function(list)
    structure(list, class = "gset")

.make_gset_from_support_and_memberships <-
function(support, memberships)
{
    # remove entries with 0 memberships
    if (!is.null(memberships)) {
        z <- if (is.list(memberships)) {
            memberships <- lapply(memberships, function(i) {
                z <- unlist(i) == 0
                if (all(z))
                    list()
                else
                    .make_gset_from_support_and_memberships(i[!z],
                                                            .get_memberships(i)[!z]
                                                            )
            })
            sapply(memberships, length) == 0
        } else {
            memberships == 0
        }
        if (all(z)) {
            support <- set()
            memberships <- NULL
        } else {
            support <- support[!z]
            memberships <- memberships[!z]
        }
    }

    structure(support,
              memberships = memberships,
              class = "gset")
}

.remove_element_class <-
function(x)
{
    s <- setdiff(class(x), "element")
    if (length(s) < 1)
        s <- NULL
    class(x) <- s
    x
}

.remove_element_class_and_memberships <-
function(x)
{
    attr(x, "memberships") <- NULL
    .remove_element_class(x)
}

.stop_if_memberships_are_invalid <-
function(memberships, errmsg = NULL)
{
    if (is.list(memberships)) {
        for (i in memberships) {
            if (!gset_is_crisp(i))
                stop("For fuzzy multisets, the memberships must be (multi)sets.",
                     call. = FALSE)
            if (!all(sapply(i,
                            function(j) is.numeric(j) && (j >= 0) && (j <= 1))))
                stop("For fuzzy multisets, the memberships must be multisets over the unit interval.", call. = FALSE)
        }
    } else {
        if (any(memberships < 0))
            stop(paste(errmsg,
                       "Memberships must be positive.", sep = ""), call. = FALSE)
        if (any(memberships > 1) && any(memberships != trunc(memberships)))
            stop(paste(errmsg,
                       "Memberships must be either all integer (multisets),\n  or all in the unit interval (fuzzy sets).", sep = ""), call. = FALSE)
    }
}

.make_gset_from_list_of_gsets_and_support_and_connector <-
function(l, support, connector)
{
    ## handle the "ordinary set" case
    if (all(sapply(l, gset_is_set)))
        return(support)

    ## extract memberships according to support
    m <- .list_of_normalized_memberships_from_list_of_gsets_and_support(l, support)

    ## apply connector to memberships
    memberships <-
        if (all(sapply(l, function(i) gset_is_multiset(i) || gset_is_fuzzy_set(i)))) {
            ## - for multisets and fuzzy sets, just use normalized memberships
            Reduce(connector, m)
        } else {
            ## - in the general case, we first need to group by support
            ##   and expand the memberships

            ## determine max. membership cardinality
            is_list <- sapply(m, is.list)
            mlen <- max(if (any(is_list)) sapply(m[is_list], sapply, length),
                        if (any(!is_list)) sapply(m[!is_list], max))

            ## group by support & expand memberships
            lapply(seq_along(support), function(i) {
                Reduce(connector,
                       lapply(m, function(j) .expand_membership(j[[i]], len = mlen)))
            })
        }

    ## create resulting gset
    .make_gset_from_support_and_memberships(support = support,
                                            memberships = .canonicalize_memberships(memberships))
}

