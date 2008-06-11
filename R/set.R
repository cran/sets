############
### Sets ###
############

### Basic stuff (constructors, print/summary methods)
set <-
function(...)
    .make_set_from_list(.list_sort(.list_unique(list(...))))

print.set <-
function(x, ...)
{
    writeLines(strwrap(format(x, ...), exdent = 1L))
    invisible(x)
}

summary.set <-
function(object, ...)
{
    len <- length(object)
    out <- if (len == 0L)
        gettext("The empty set.")
    else if (len == 1L)
        gettext("A set with 1 element.")
    else
        gettextf("A set with %d elements.", len)
    structure(out, class = "summary.set")
}

print.summary.set <-
function(x, ...)
{
    writeLines(x)
    invisible(x)
}

### operators

Ops.set <-
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
        if(is.set(e1) &&
            ((trunc(e2) != e2) || (e2 < 1L)))
            stop("Cartesian product only defined for positive integers.")
        if(is.set(e2) && (e1 != 2L))
            stop("Operator not defined.")
    }

    switch(.Generic,
           "+"  = gset_sum(e1, e2),
           "|"  = set_union(e1, e2),
           "-"  = gset_difference(e1, e2),
           "&"  = set_intersection(e1, e2),
           "*"  = set_cartesian(e1, e2),
           "<"  = set_is_proper_subset(e1, e2),
           "<=" = set_is_subset(e1, e2),
           ">"  = set_is_proper_subset(e2, e1),
           ">=" = set_is_subset(e2, e1),
           "==" = set_is_equal(e1, e2),
           "!=" = !set_is_equal(e1, e2),
           "^"  = {
               if(is.set(e2))
                   set_power(e2)
               else
                   do.call(set_cartesian, rep(list(e1), e2))}
           )

}

### Subscript methods

## Disable numeric subscripting (as sets are "unordered" collections of
## elements).  Note that iterating via for() and lapply() still works,
## the former because this [currently, 2007-09-16] directly uses the
## internal list representation and the latter because we provide an
## as.list() method.

`[.set` <-
function(x, i)
{
    if(!is.character(i))
        stop("Subscripting of sets is only defined using labels.")
    .make_set_from_list(NextMethod("["))
}

`[[.set` <-
function(x, i)
{
    if(!is.character(i))
        stop("Subscripting of sets is only defined using labels.")
    NextMethod("[[")
}

### internal stuff

.list_order <-
function(x, decreasing = FALSE, ...) {
    ch <- as.character(x)

    if (capabilities("iconv")) {
        loc <- Sys.getlocale("LC_COLLATE")
        warn <- options()$warn
        on.exit({Sys.setlocale("LC_COLLATE", loc); options(warn = warn)})
        options(warn = -1)
        Sys.setlocale("LC_COLLATE", "en_US.UTF-8")
        ch <- iconv(ch, to = "UTF-8")
    }

    order(sapply(x, length),
          sapply(x, typeof),
          ch,
          decreasing = decreasing, ...)
}

.list_sort <-
function(x, decreasing = FALSE, ...)
    as.list(x)[.list_order(x, decreasing = decreasing, ...)]

.list_unique <-
function(x)
    as.list(x)[!duplicated(x)]

.make_set_from_list <-
function(x)
    structure(x, class = c("set", "gset"))

.format_set_or_tuple <-
function(x, left, right, ...)
{
    nms <- names(x)
    names(x) <- NULL
    SEP <- rep.int("", length(x))
    if (!is.null(nms))
      SEP[nms != ""] <- " = "
    paste(left,
          if (length(x) > 0)
              paste(nms, SEP, LABELS(as.list(x), ...),
                    sep = "", collapse = ", "),
          right,
          sep = "")
  }

