\name{similarity}
\alias{set_similarity}
\alias{gset_similarity}
\alias{cset_similarity}
\alias{set_dissimilarity}
\alias{gset_dissimilarity}
\alias{cset_dissimilarity}
\title{Similarity and Dissimilarity Functions}
\description{Similarities and dissimilarities for (generalized) sets.}
\usage{
set_similarity(x, y, method = "Jaccard")
gset_similarity(x, y, method = "Jaccard")
cset_similarity(x, y, method = "Jaccard")

set_dissimilarity(x, y, method = c("Jaccard", "Manhattan", "Euclidean", "L1", "L2"))
gset_dissimilarity(x, y, method = c("Jaccard", "Manhattan", "Euclidean", "L1", "L2"))
cset_dissimilarity(x, y, method = c("Jaccard", "Manhattan", "Euclidean", "L1", "L2"))
}
\arguments{
  \item{x, y}{Two (generalized/customizable) sets.}
  \item{method}{Character string specifying the proximity method (see below).}
}
\details{
  For two generalized sets \eqn{X} and \eqn{Y}, the
  \code{Jaccard} similarity is \eqn{|X \cap Y| / |X \cup Y|}{|X intersect Y| /
    |X U Y|} where \eqn{|\cdot|}{|.|} denotes the cardinality for
  generalized sets (sum of memberships). The \code{Jaccard} 
  dissimilarity is 1 minus the similarity.

  The \code{L1} (or \code{Manhattan}) and \code{L2} (or
  \code{Euclidean}) 
  dissimilarities are defined as
  follows. For two fuzzy multisets \eqn{A} and \eqn{B} on a
  given universe \eqn{X} with elements \eqn{x}, let
  \eqn{M_A(x)} and \eqn{M_B(x)} be functions returning the memberships of an
  element \eqn{x} in sets \eqn{A} and \eqn{B}, respectively. The
  memberships are returned in \emph{standard form},
  i.e. as an infinite vector of decreasing membership
  values, e.g. \eqn{(1, 0.3, 0, 0, \dots)}{(1, 0.3, 0, 0, ...)}.
  Let \eqn{M_A(x)_i} and \eqn{M_B(x)_i} denote the \eqn{i}th components of these
  membership vectors. Then the L1 distance is defined as:
  \deqn{d_1(A, B) = \sum_{x \in X}\sum_{i=1}{\infty}|M_A(x)_i -
  M_B(x)_i|}{d1(A, B) = sum_x sum_i |M_A(x)_i - M_B(x)_i|} and the L2
  distance as: \deqn{d_2(A, B) = \sqrt{\sum_{x \in
  X}\sum_{i=1}{\infty}|M_A(x)_i - M_B(x)_i|^2}}{d2(A, B) = sqrt{sum_x sum_i |M_A(x)_i - M_B(x)_i|^2}}
  
}
\value{
  A numeric value (similarity or dissimilarity, as specified).
}
\seealso{
  \code{\link{set}}.
}
\source{
  T. Matthe, R. De Caluwe, G. de Tre, A. Hallez, J. Verstraete, M. Leman,
  O. Cornelis, D. Moelants, and J. Gansemans (2006),
  Similarity Between Multi-valued Thesaurus
  Attributes: Theory and Application in Multimedia Systems,
  \emph{Flexible Query Answering Systems},
  Lecture Notes in Computer Science, Springer, 331--342.
		  
  K. Mizutani, R. Inokuchi, and S. Miyamoto (2008),
  Algorithms of Nonlinear Document Clustering Based on Fuzzy Multiset
  Model,
  \emph{International Journal of Intelligent Systems},
  \bold{23}, 176--198.
}
\examples{
A <- set("a", "b", "c")
B <- set("c", "d", "e")
set_similarity(A, B)
set_dissimilarity(A, B)

A <- gset(c("a", "b", "c"), c(0.3, 0.7, 0.9))
B <- gset(c("c", "d", "e"), c(0.2, 0.4, 0.5))
gset_similarity(A, B, "Jaccard")
gset_dissimilarity(A, B, "Jaccard")
gset_dissimilarity(A, B, "L1")
gset_dissimilarity(A, B, "L2")

A <- gset(c("a", "b", "c"), list(c(0.3, 0.7), 0.1, 0.9))
B <- gset(c("c", "d", "e"), list(0.2, c(0.4, 0.5), 0.8))
gset_similarity(A, B, "Jaccard")
gset_dissimilarity(A, B, "Jaccard")
gset_dissimilarity(A, B, "L1")
gset_dissimilarity(A, B, "L2")
}
\keyword{math}
