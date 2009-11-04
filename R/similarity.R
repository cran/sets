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

set_dissimilarity <-
function(x, y, method = c("Jaccard", "symdiff", "L1", "L2"))
{
    method <- match.arg(method)

    switch(method,
           Jaccard =,
           symdiff = 1 - set_similarity(x, y, "Jaccard"),
           L1 = sqrt(length(set_symdiff(x, y))),
           L2 = length(set_symdiff(x, y))
           )
}

gset_dissimilarity <-
function(x, y, method = c("Jaccard", "symdiff", "L1", "L2"))
{
    method <- match.arg(method)

    switch(method,
           Jaccard =,
           symdiff = 1 - gset_similarity(x, y, "Jaccard"),
           L1 = .distfunL1(x, y),
           L2 = .distfunL2(x, y)
           )
}

cset_dissimilarity <-
function(x, y, method = c("Jaccard", "symdiff", "L1", "L2"))
{
    method <- match.arg(method)

    switch(method,
           Jaccard =,
           symdiff = 1 - cset_similarity(x, y, "Jaccard"),
           L1 = .distfunL1(x, y),
           L2 = .distfunL2(x, y)
           )
}

### internal

.distfunL2 <- function(x, y) {
    if (cset_is_multiset(x, na.rm = TRUE))
        sqrt(.distfunL1(x, y))
    else
        sqrt(sum(unlist(.apply_connector_to_list_of_gsets_and_support(list(x, y),
             set_union(x, y), function(i,j) crossprod(i - j)[,,drop = T]))))
}

.distfunL1 <- function(x, y)
    sum(unlist(.apply_connector_to_list_of_gsets_and_support(list(x, y),
                    set_union(x, y), function(i,j) sum(abs(i - j)))))

