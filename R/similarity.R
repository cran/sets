gset_similarity <-
function(x, y, method = "Jaccard")
{
    method <- match.arg(method)

    if (method == "Jaccard")
        length(gset_intersection(x, y)) / length(gset_union(x, y))
}

