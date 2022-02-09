## set predicates

.is.listonly  <-
function(x) is.list(x) && is.null(attributes(x))

.is_list_of_sets <-
function(x) .is.listonly(x) && all(vapply(x, is.set, logical(1L)))

is.set <-
function(x)
    inherits(x, "set")

set_is_empty <-
function(x)
{
    if (is.cset(x))
        set_cardinality(x) < 1L
    else if (.is_list_of_sets(x))
        vapply(x, set_cardinality, integer(1L)) < 1L
    else stop("Argument must be a set or a list of sets.")
}

.vectorize_set_op <-
function(x, y, predicate)
{
    if (is.set(x) && is.set(y))
        predicate(x, y)
    else if(.is.listonly(x) && is.set(y))
        Vectorize(predicate)(x, list(y))
    else if(is.set(x) && .is.listonly(y))
        Vectorize(predicate)(list(x), y)
    else if(.is_list_of_sets(x) && .is_list_of_sets(y))
        Vectorize(predicate)(x, y)
    else stop("Predicate only implemented for sets or list of sets.")
}

set_is_subset <-
function(x, y)
{
    .help <- function(a, b) set_is_empty(set_complement(b, a))
    .vectorize_set_op(x, y, .help)

}

set_is_proper_subset <-
function(x, y)
{
    .help <- function(a, b) set_is_empty(set_complement(b, a)) && length(a) != length(b)
    .vectorize_set_op(x, y, .help)

}

set_is_equal <-
function(x, y)
{
    .help <- function(a, b)
        identical(a, b) ||
        ((length(a) == length(b))
         && (length(set_intersection(a,b)) == length(a)))
    .vectorize_set_op(x, y, .help)
}

set_contains_element <-
function(x, e)
{
    if (is_element(e) || is.tuple(e) || is.gset(e))
        e <- list(e)
    if (is.set(x))
        x <- list(x)
    if(!.is_list_of_sets(x))
        stop("Predicate only implemented for a set or a list of sets.")

   .help <- function(x, e) {
       if(set_is_empty(x))
           return(FALSE)
       if(is_element(e))
           e <- e[[1]]
       !is.na(.exact_match(list(e), x))
    }

    Vectorize(.help)(x, e)
}

## gset-predicates

.is_list_of_gsets <-
function(x) .is.listonly(x) && all(vapply(x, is.gset, logical(1L)))

is_element <-
function(e)
    inherits(e, "gset_element")

is.gset <-
function(x)
    inherits(x, c("gset", "set"))

gset_is_empty <-
function(x, na.rm = FALSE)
{
    if (is.cset(x))
        gset_cardinality(x, na.rm = na.rm) == 0
    else if (.is_list_of_gsets(x))
        vapply(x, gset_cardinality, double(1L), na.rm = na.rm) == 0
    else stop("Argument must be a gset or a list of gsets.")
}

.vectorize_gset_op <-
function(x, y, predicate)
{
    if(is.gset(x) && is.gset(y))
        predicate(x, y)
    else if(.is.listonly(x) && is.gset(y))
        Vectorize(predicate)(x, list(y))
    else if(is.gset(x) && .is.listonly(y))
        Vectorize(predicate)(list(x), y)
    else if(.is_list_of_gsets(x) && .is_list_of_gsets(y))
        Vectorize(predicate)(x, y)
    else stop("Arguments must be gsets or list of gsets.")
}

gset_is_subset <-
function(x, y, na.rm = FALSE)
{
    .help <-
        function(a, b) set_is_subset(gset_support(a), gset_support(b)) &&
    all(unlist(.apply_connector_to_list_of_gsets_and_support(list(a, b),
                                                             .get_support(a),
                                                             `<=`)),
        na.rm = na.rm)
    .vectorize_gset_op(x, y, .help)

}

gset_is_proper_subset <-
function(x, y, na.rm = FALSE)
{
    .help <- function(x, y) gset_is_subset(x, y, na.rm = na.rm) &&
        cset_cardinality(x, na.rm = na.rm) != cset_cardinality(y, na.rm = na.rm)
    .vectorize_gset_op(x, y, .help)
}

gset_is_equal <-
function(x, y, na.rm = FALSE)
{
    .help <- function(a, b)
        identical(a, b) ||
        cset_cardinality(a, na.rm = na.rm) ==
            cset_cardinality(b, na.rm = na.rm) &&
            cset_cardinality(gset_intersection(a, b), na.rm = na.rm) ==
            cset_cardinality(a, na.rm = na.rm)
    .vectorize_gset_op(x, y, .help)

}

gset_contains_element <-
function(x, e) {
    if (is.tuple(e) || is.gset(e))
        e <- list(list(e))
    if (is_element(e))
        e <- list(gset(elements = list(e)))
    if (is.gset(x))
        x <- list(x)
    if(!.is_list_of_gsets(x))
        stop("Predicate only implemented for a gset or a list of gsets.")

    .help <- function(x, e) gset_is_subset(as.gset(e), x)

    Vectorize(.help)(x, e)
}

.vectorize_unary_gset_op <-
function(x, predicate)
{
    if (is.cset(x))
        predicate(x)
    else if (.is_list_of_csets(x))
        vapply(x, predicate, logical(1L))
    else stop("Predicate only implemented for a [cg]set or a list of [cg]sets.")
}

gset_is_multiset <-
function(x, na.rm = FALSE)
{
    .help <- function(x) {
        m <- .get_memberships(x)
        !is.list(m) && (na.rm && any(m > 1, na.rm = TRUE) ||
                        !na.rm && any(m > 1) && !any(m < 1, na.rm = TRUE))
    }
    .vectorize_unary_gset_op(x, .help)
}

gset_is_crisp <-
gset_is_set_or_multiset <-
function(x, na.rm = FALSE)
{
    .help <- function(x) {
        m <- .get_memberships(x)
        !is.list(m) && (na.rm && all(m >= 1, na.rm = TRUE) ||
                        !na.rm && all(m >= 1) && !any(m < 1, na.rm = TRUE))
    }
    .vectorize_unary_gset_op(x, .help)

}

gset_is_fuzzy_set <-
function(x, na.rm = FALSE)
{
    .help <- function(x) {
        m <- .get_memberships(x)
        !is.list(m) && (na.rm && any(m < 1, na.rm = TRUE) ||
                        !na.rm && any(m < 1) && !any(m > 1, na.rm = TRUE))
    }
    .vectorize_unary_gset_op(x, .help)
}

gset_is_set_or_fuzzy_set <-
function(x, na.rm = FALSE)
{
    .help  <- function(x) {
        m <- .get_memberships(x)
        !is.list(m) && (na.rm && all(m <= 1, na.rm = TRUE) ||
                        !na.rm && all(m <= 1) && !any(m > 1, na.rm = TRUE))
    }
    .vectorize_unary_gset_op(x, .help)

}

gset_is_fuzzy_multiset <-
function(x)
{
    .help <- function(x) is.list(.get_memberships(x))
    .vectorize_unary_gset_op(x, .help)

}

gset_is_set <-
function(x, na.rm = FALSE)
{
    .help <- function(x) {
        m <- .get_memberships(x)
        !is.list(m) && all(m == 1, na.rm = na.rm)
    }
    .vectorize_unary_gset_op(x, .help)

}

gset_has_missings <-
function(x)
{
    .help <- function(x) {
        M <- .get_memberships(x)
        if (is.list(M))
            any(is.na(unlist(M))) || any(is.na(unlist(lapply(M, .get_memberships))))
        else
            any(is.na(M))
    }
    .vectorize_unary_gset_op(x, .help)
}

## cset-predicates

.is_list_of_csets <-
function(x) .is.listonly(x) && all(vapply(x, is.cset, logical(1L)))

is.cset <-
function(x)
    inherits(x, c("cset", "gset", "set"))

cset_is_empty <-
function(x, na.rm = FALSE)
{
    if(is.cset(x))
        cset_cardinality(x, na.rm = na.rm) == 0
    else
        all(vapply(x, cset_cardinality, double(1L), na.rm = na.rm) == 0)
}

.vectorize_cset_op <-
function(x, y, predicate)
{
    if(is.cset(x) && is.cset(y))
        predicate(x, y)
    else if(.is.listonly(x) && is.cset(y))
        Vectorize(predicate)(x, list(y))
    else if(is.cset(x) && .is.listonly(y))
        Vectorize(predicate)(list(x), y)
    else if(.is_list_of_csets(x) && .is_list_of_csets(y))
        Vectorize(predicate)(x, y)
    else stop("Comparison not implemented.")
}

cset_is_subset <-
function(x, y, na.rm = FALSE)
{
    .help <-
        function(a, b) {
            .check_matchfun(list(a, b))
            set_is_empty(.set_complement_using_matchfun(cset_support(b),
                                                       cset_support(a),
                                                       cset_matchfun(a))) &&
    all(unlist(.apply_connector_to_list_of_gsets_and_support(list(a, b),
                                                             .get_support(a),
                                                             `<=`,
                                                             cset_matchfun(a))),
        na.rm = na.rm
        )
    }
    .vectorize_cset_op(x, y, .help)

}

cset_is_proper_subset <-
function(x, y, na.rm = FALSE)
{
    .help <- function(a, b)
         cset_is_subset(a, b, na.rm = na.rm) &&
             cset_cardinality(x, na.rm = na.rm) !=
             cset_cardinality(y, na.rm = na.rm)
    .vectorize_cset_op(x, y, .help)
}

cset_is_equal <-
function(x, y, na.rm = FALSE)
{
    .help <- function(a, b)
        identical(a, b) ||
        cset_cardinality(a, na.rm = na.rm) ==
            cset_cardinality(b, na.rm = na.rm) &&
    cset_cardinality(cset_intersection(a, b), na.rm = na.rm) ==
        cset_cardinality(a, na.rm = na.rm)
    .vectorize_cset_op(x, y, .help)

}

cset_contains_element <-
function(x, e)
{
    if(cset_is_empty(x))
        return(FALSE)
    matchfun <- cset_matchfun(x)
    x <- .make_list_of_elements_from_cset(x)
    e <- e(e)
    if(is.tuple(e) || is.cset(e) || is_element(e))
        e <- list(e)
    ind <- matchfun(e, x)
    if(is.na(ind)) return(FALSE)
    .get_memberships(x[[ind]]) == .get_memberships(e[[1]])
}

cset_is_multiset <-
function(x, na.rm = FALSE)
    gset_is_multiset(x, na.rm = na.rm)

cset_is_crisp <-
cset_is_set_or_multiset <-
function(x, na.rm = FALSE)
    gset_is_crisp(x, na.rm = na.rm)

cset_is_fuzzy_set <-
function(x, na.rm = FALSE)
    gset_is_fuzzy_set(x, na.rm = na.rm)

cset_is_set_or_fuzzy_set <-
function(x, na.rm = FALSE)
    gset_is_set_or_fuzzy_set(x, na.rm = na.rm)

cset_is_fuzzy_multiset <-
function(x)
    gset_is_fuzzy_multiset(x)

cset_is_set <-
function(x, na.rm = FALSE)
    gset_is_set(x, na.rm = na.rm)

cset_has_missings <-
function(x)
    gset_has_missings(x)

## contains_element operator dispatch

"%e%" <-
function(e, x)
{
    if(is.set(x) || .is_list_of_sets(x))
        set_contains_element(x, e)
    else if(is.gset(x) || .is_list_of_gsets(x))
        gset_contains_element(x, e)
    else if(is.cset(x) || .is_list_of_csets(x))
        cset_contains_element(x, e)
    else if(is.interval(x))
        interval_contains_element(x, e)
    else
        stop("Predicate undefined.")
}

