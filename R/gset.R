########################
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
function(support = NULL, memberships = NULL, charfun = NULL,
         elements = NULL, universe = NULL, bound = NULL)
{
    Universe <- universe
    if(is.null(universe))
        universe <- sets_options("universe")

    ### some checks
    if (!is.null(elements) &&
        !(is.null(support) && is.null(memberships) && is.null(charfun)))
        stop("'elements' needs to be specified alone.")
    if (is.null(support) && !is.null(memberships))
        stop("'membership' requires 'support'.")
    if (is.null(universe) && !is.null(charfun))
        stop("'charfun' requires 'universe' (or a default universe).")
    if (!is.null(memberships) && !is.null(charfun))
        stop("Need either 'memberships' and 'support', or 'charfun' and 'universe'.")

    ### element specification:
    ## split support and memberships, and proceed
    if (!is.null(elements)) {
        support <- unlist(elements, recursive = FALSE)
        memberships <- lapply(elements, .get_memberships)
    }

    ### universe & charfun specification:
    ## create memberships from charfun, and proceed
    if (!is.null(universe))
        universe <- as.set(eval(universe))
    if (!is.null(charfun)) {
        if (is.charfun_generator(charfun))
            charfun <- charfun()
        memberships <- if (.domain_is_numeric(universe))
            charfun(unlist(universe))
        else
            lapply(universe, charfun)
        support <- universe
        .stop_if_memberships_are_invalid(memberships,
                                         "Membership function invalid.\n  ")
    }

    ## handle empty set
    if (is.null(support))
        return(set())

    ### support & membership specification:
    ## just check memberships
    if (!is.null(memberships))
        .stop_if_memberships_are_invalid(memberships)

    ## check support against universe
    if (!is.null(universe) && !set_is_subset(as.set(support),
                                             as.set(universe)))
        stop("Support must be a subset of the universe.")

    ### canonicalize memberships & create gset
    .make_gset_from_support_and_memberships(support, memberships, Universe,
                                            bound)
}

print.gset <-
function(x, ...)
{
    writeLines(strwrap(format(x, ...), exdent = 1L))
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
    structure(out, class = "summary.gset")
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
        .as.list(x)
    else
        .make_list_of_elements_from_cset(x)
    .format_set_or_tuple(x, "{", "}", ...)
}

### operators

## Disable numeric subscripting (as sets are "unordered" collections of
## elements).  Note that iterating via for() and lapply() still works,
## the former because this [currently, 2007-09-16] directly uses the
## internal list representation and the latter because we provide an
## as.list() method.

`[.gset` <-
function(x, i = x)
{
    ind <- .lookup_elements(x, i)
    gset(.as.list(x)[ind], gset_memberships(x)[ind])
}

`[[.gset` <-
function(x, i)
{
    as.set(x)[[i]]
}

`[<-.gset` <-
function(x, i = x, value)
{
    gset(`[<-`(.as.list(x), .lookup_elements(x, i), value),
         memberships = gset_memberships(x))
}

`[[<-.gset` <-
function(x, i, value)
{
    if (!is.character(i) || length(i) > 1L) i <- list(i)
    gset(`[[<-`(.as.list(x), .lookup_elements(x, i), value),
         memberships = gset_memberships(x))
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
                   do.call(gset_cartesian, rep.int(list(e1), e2))}
           )

}

### internal stuff

.make_gset_from_list <-
function(list)
    structure(list, class = "gset")

.make_gset_from_support_and_memberships <-
function(support, memberships, universe = NULL, bound = NULL)
{
    ## simplify memberships:
    if (!is.null(memberships)) {
        ## canonicalize (i.e., make sure that fuzzy multiset memberships
        ## are valid gset objects)
        memberships <- .canonicalize_memberships(memberships)

        ## check length
        if (length.set(memberships) != length.set(support))
            stop("Length of support must match length of memberships.")

        ## for fuzzy multisets, remove elements in membership with
        ## zero support
        if (is.list(memberships)) {
            # find zero elements in list
            z <- lapply(memberships, sapply, `>`, 0)
            # empty sets should have length 0
            z <- lapply(z, function(i) if (is.list(i)) 0 else i)
            # filter 0 elements
            memberships <-
                Map(.make_gset_by_support_and_memberships,
                    Map("[", lapply(memberships, .as.list), z),
                    Map("[", lapply(memberships, gset_memberships), z))
        }

        ## compute index of entries with 0 memberships.
        z <- if (is.list(memberships))
            sapply(memberships, length) == 0
        else
            memberships == 0

        ## all zero? return empty set.
        if (all(z)) return(set())

        ## remove entries with zero membership.
        support <- .as.list(support)[!z]
        memberships <- memberships[!z]
    }

    ## if gset has no or trivial membership:
    if (is.null(memberships) || is.atomic(memberships) &&
        all(memberships == 1)) {
        support <- .list_sort(.as.list(support))
        ## if a universe exists, return gset
        if (!is.null(universe))
            return(.make_gset_by_support_and_memberships(support = support,
                                                         memberships = NULL,
                                                         universe = universe))
        else
            return(.make_set_from_list(support))
    }

    ## if gset has fuzzy multiset representation, but
    ## is really only multi or fuzzy, simplify memberships.
    tmp <- lapply(memberships, .as.list)
    if (all(sapply(tmp, length) == 1L)) {
        if (all(unlist(memberships) == 1L))
            memberships <- sapply(memberships, .get_memberships)
        else if (all(sapply(memberships, .get_memberships) == 1L)) {
            memberships <- unlist(memberships)
        }
    }

    ## check memberships against bound, if any.
    bd <- bound
    if (is.null(bd))
        bd <- sets_options("bound")
    if (!is.null(bd) && (max(.multiplicities(memberships)) > bd))
        stop("Memberships must not exceed bound!")

    ## convert support to set. Reorder memberships accordingly, if needed.
    S <- canonicalize_set_and_mapping(x = support, mapping = memberships)

    ## return gset.
    .make_gset_by_support_and_memberships(support = S$set,
                                          memberships = S$mapping,
                                          universe = universe,
                                          bound = bound)
}

.make_gset_by_support_and_memberships <-
function(support, memberships, universe = NULL, bound = NULL)
{
    if (is.null(universe) && (is.null(memberships) ||
        is.atomic(memberships) && all(memberships == 1)))
        .make_set_from_list(support)
    else
        structure(support,
                  memberships = memberships,
                  universe = universe,
                  bound = bound,
                  class = c("gset", "cset"))
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

.apply_connector_to_list_of_gsets_and_support <-
function(l, support, connector, matchfun = .exact_match,
         enforce_general_case = FALSE)
{
    ## extract memberships according to support
    m <- lapply(l, .memberships_for_support, support, matchfun)

    ## apply connector to memberships
    if (!enforce_general_case &&
        (all(sapply(l, gset_is_crisp)) ||
         all(sapply(l, gset_is_set_or_fuzzy_set))))
        ## for multisets and fuzzy sets, just use normalized memberships
        Reduce(connector, m)
    else {
        ## in all other cases, expand memberships first:

        ## determine max. membership cardinality
        is_list <- sapply(m, is.list)
        mlen <- max(if (any(is_list)) sapply(m[is_list], sapply, length),
                    if (any(!is_list)) sapply(m[!is_list], max))

        ## group by support, expand memberships, and apply connector
        lapply(seq_along(support), function(i) {
            Reduce(connector,
                   lapply(m, function(j) .expand_membership(j[[i]],
                                                            len = mlen)))
        })
    }
}

.make_gset_from_list_of_gsets_and_support_and_connector <-
function(l, support, connector, matchfun = .exact_match,
         enforce_general_case = FALSE)
{
    ## compute memberships
    memberships <-
        .apply_connector_to_list_of_gsets_and_support(l, support, connector,
                                                      matchfun,
                                                      enforce_general_case)

    ## create resulting gset
    .make_gset_from_support_and_memberships(support, memberships)

}

.make_list_of_elements_from_cset <-
function(x)
    Map(.make_element_from_support_and_memberships,
        .as.list(x),
        .as.list(.get_memberships(x)))

