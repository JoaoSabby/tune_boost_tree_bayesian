\name{SplitDataBoostTreeFolds}
\alias{SplitDataBoostTreeFolds}
\title{Create Stratified Boosted Tree Folds}
\usage{SplitDataBoostTreeFolds(yData, nFolds = 10L, seed = 42L)}
\arguments{
\item{yData}{Binary 0/1 outcome vector.}
\item{nFolds}{Integer number of folds.}
\item{seed}{Integer random seed.}
}
\description{Creates stratified folds for binary 0/1 outcome data.}
\value{A list of fold index vectors.}
