\name{fuzzydocs}
\title{Documents on Fuzzy Theory}
\alias{fuzzy_docs}
\description{
  Occurence of three terms (neural networks, fuzzy, and image)
  in 30 documents retrieved
  from a Japanese article data base on fuzzy theory and systems.
}
\usage{
data("fuzzy_docs")
}
\format{
  \code{fuzzy_docs} is a list of 30 fuzzy multisets, representing the
  occurence of the terms \dQuote{neural networks}, \dQuote{fuzzy}, and
  \dQuote{image} in each document. Each term appears with up to
  three membership values representing weights,
  depending on whether the term occured
  in the abstract (0.2), the keywords section (0.6), and/or the title
  (1). The first 12 documents concern neural networks, the remaining 18
  image processing. In the reference, various clustering methods have
  been employed to recover the two groups in the data set.
}
\source{
  K. Mizutani, R. Inokuchi, and S. Miyamoto (2008),
  Algorithms of Nonlinear Document Clustering Based on Fuzzy Multiset
  Model,
  \emph{International Journal of Intelligent Systems},
  \bold{23}, 176--198.
}
\examples{
data(fuzzy_docs)

## compute distance matrix using Jaccard dissimilarity
d <- as.dist(set_outer(fuzzy_docs, gset_dissimilarity))

## apply hierarchical clustering (Ward method)
cl <- hclust(d, "ward")

## retrieve two clusters
cutree(cl, 2)

## -> clearly, the clusters are formed by docs 1--12 and 13--30, respectively.

}
\keyword{datasets}
