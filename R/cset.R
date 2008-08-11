#########################
### Customizable Sets ###
#########################

### generator

cset <-
function(gset,
         orderfun = set_options("orderfun"),
         matchfun = set_options("matchfun"))
{
    gset <- as.gset(gset)

    ## recreate gset according to user-specified match-fun
    if (!is.null(matchfun)) {
        uni <- !.duplicated_by_matchfun(gset, matchfun)
        gset <-
            .make_gset_by_support_and_memberships(.as.list(.get_support(gset)[uni]),
                                                  .get_memberships(gset)[uni])
    }

    ## create cset-object
    .make_cset_from_gset_and_orderfun_and_matchfun(gset,
                                                   orderfun,
                                                   matchfun)
}

## convenience function generator for non-vectorized equality predicates
make_matchfun <-
function(FUN)
    .make_matchfun_from_equalityfun(FUN)

## Disable numeric subscripting (as sets are "unordered" collections of
## elements).  Note that iterating via for() and lapply() still works,
## the former because this [currently, 2007-09-16] directly uses the
## internal list representation and the latter because we provide an
## as.list() method.

`[.cset` <-
function(x, i)
{
    if(!is.character(i))
        stop("Subscripting of customizable sets is only defined by labels.")
    cset(.make_gset_from_list(NextMethod("[")),
         .orderfun(x),
         .matchfun(x))
}

`[[.cset` <-
function(x, i)
{
    if(!is.character(i))
        stop("Subscripting of customizable sets is only defined by labels.")
    NextMethod("[[")
}

`[<-.cset` <-
function(x, i, value)
{
    if(!is.character(i))
        stop("Subassignment of customizable sets is only defined by labels.")
    cset(.make_gset_from_list(NextMethod("[<-")),
         .orderfun(x),
         .matchfun(x))
}

`[[<-.cset` <-
function(x, i, value)
{
    if(!is.character(i))
        stop("Subassignment of customizable sets is only defined by labels.")
    NextMethod("[[<-")
}

### Ops-method

Ops.cset <-
function(e1, e2)
{
    if(nargs() == 1L) {
        if(!(as.character(.Generic) %in% "!"))
            stop(gettextf("Unary '%s' not defined for \"%s\" objects.",
                          .Generic, .Class))
        return(cset_complement(e1))
    }

    if(!(as.character(.Generic)
         %in% c("<", "<=", ">", ">=", "==", "!=",
                "&", "|", "*", "+", "-", "^")))
        stop(gettextf("Generic '%s' not defined for \"%s\" objects.",
                      .Generic, .Class))

    if(as.character(.Generic) == "^") {
        if(is.cset(e1) &&
            ((trunc(e2) != e2) || (e2 < 1L)))
            stop("Cartesian product only defined for positive integers.")
        if(is.cset(e2) && (e1 != 2L))
            stop("Operator not defined.")
    }

    switch(.Generic,
           "+"  = cset_sum(e1, e2),
           "|"  = cset_union(e1, e2),
           "-"  = cset_difference(e1, e2),
           "&"  = cset_intersection(e1, e2),
           "*"  = cset_cartesian(e1, e2),
           "<"  = cset_is_proper_subset(e1, e2),
           "<=" = cset_is_subset(e1, e2),
           ">"  = cset_is_proper_subset(e2, e1),
           ">=" = cset_is_subset(e2, e1),
           "==" = cset_is_equal(e1, e2),
           "!=" = !cset_is_equal(e1, e2),
           "^"  = {
               if(is.cset(e2))
                   cset_power(e2)
               else
                   do.call(cset_cartesian, rep(list(e1), e2))}
           )

}

### print methods

print.cset <-
function(x, ...)
    print.gset(x, ...)

print.summary.cset <-
function(x, ...)
    print.summary.gset(x, ...)

### format method

format.cset <-
function(x, ...) {
    FUN <- cset_orderfun(x)
    x <- if (gset_is_set(x))
        .as.list(x)
    else
        .make_list_of_elements_from_cset(x)
    if (is.function(FUN))
        x <- x[FUN(x)]
    else if(is.integer(FUN) && (length(x) == FUN))
        x <- x[FUN]
    .format_set_or_tuple(x, "{", "}", ...)
}

## summary method

summary.cset <-
function(object, ...)
{
    len <- length(object)
    out <- if (len == 0L)
        gettext("The empty set.")
    else if (len == 1L)
        gettext("A customizable set with 1 element.")
    else
        gettextf("A customizable set with cardinality %g.", len)
    if(!is.null(attr(object, "matchfun")) && !is.null(attr(object, "orderfun")))
        out <- paste(out, "The match and order functions are non-standard.")
    else if(!is.null(attr(object, "matchfun")))
        out <- paste(out, "The match function is non-standard.")
    else if(!is.null(attr(object, "orderfun")))
        out <- paste(out, "The order function is non-standard.")

    structure(out, class = "summary.cset")
}

### internal stuff

.make_cset_from_gset_and_orderfun_and_matchfun <-
function(gset, orderfun = NULL, matchfun = NULL)
{
    ## make sure that default orderfun and default matchfun are never stored
    if (identical(orderfun, .list_order))
        orderfun <- NULL
    if (identical(matchfun, .exact_match))
        matchfun <- NULL

    ## promote to gset, if only default-funs are specified
    if (is.null(matchfun) && is.null(orderfun))
        return(gset)

    ## create structure (including overwriting gset-class)
    structure(gset,
              orderfun = orderfun,
              matchfun = matchfun,
              class = "cset")
}

.duplicated_by_matchfun <-
function(x, matchfun)
    duplicated(.as.list(x)[matchfun(x, x)])

.list_unique_by_matchfun <-
function(x, matchfun)
    .as.list(x)[!.duplicated_by_matchfun(x, matchfun)]

.check_matchfun <-
function(l)
{
    matchfun <- cset_matchfun(l[[1]])
    if (!all(sapply(l, function(i) identical(cset_matchfun(i), matchfun))))
        stop("Need same match functions (or none) for all elements.")
    matchfun
}

.check_orderfun <-
function(l)
{
    orderfun <- cset_orderfun(l[[1]])
    if (!all(sapply(l, function(i) identical(cset_orderfun(i), orderfun))))
        NULL
    else
        orderfun
}
