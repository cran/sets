## internal functions to handle memberships

.expand_membership <-
function(x, decreasing = TRUE, len = NA, rep = TRUE)
{
    if (is.null(x))
        x <- 0
    M <- .get_memberships(x)

    ## create vector from gset
    ret <- if (rep) {
        if (is.list(x))
            rep.int(as.numeric(x), M)
        else if (x > 1)
            rep.int(1, x)
        else
            x
    } else {
        as.numeric(x)
    }

    ## optionally, sort values
    if (!is.null(decreasing)) {
        O <- order(ret, decreasing = decreasing)
        ret <- ret[O]
        if (!rep) M <- M[O]
    }

    ## optionally, fill up with 0s
    if (!is.na(len)) {
        ret <- c(ret, rep(0, len - length(ret)))
        if (!rep)
            M <- c(M, rep(0, len - length(M)))
    }

    if (rep)
        ret
    else
        structure(ret, memberships = M)
}

.list_of_normalized_memberships_from_list_of_gsets_and_support<-
function(l, support)
    lapply(l, .memberships_for_support, support)

.memberships_for_support <-
function(x, support)
{
    tmp <- .get_memberships(x)[match(support, .get_support(x))]
    tmp[is.na(tmp)] <- 0
    tmp
}

.canonicalize_memberships <-
function(memberships)
{
    if (is.list(memberships))
        lapply(memberships, as.gset)
    else
        memberships
}

