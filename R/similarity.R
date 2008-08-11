set_similarity <-
function(x, y, method = "Jaccard")
{
    method <- match.arg(method)

    if (method == "Jaccard")
        length(set_intersection(x, y)) / length(set_union(x, y))
}

gset_similarity <-
function(x, y, method = "Jaccard")
{
    method <- match.arg(method)

    if (method == "Jaccard")
        length(gset_intersection(x, y)) / length(gset_union(x, y))
}

cset_similarity <-
function(x, y, method = "Jaccard")
{
    method <- match.arg(method)

    if (method == "Jaccard")
        length(cset_intersection(x, y)) / length(cset_union(x, y))
}

