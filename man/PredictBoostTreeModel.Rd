\name{PredictBoostTreeModel}
\alias{PredictBoostTreeModel}
\title{Predict with a Boosted Tree Model}
\usage{PredictBoostTreeModel(modelObj, newdata, threshold = NULL, engine_boost_tree = NULL)}
\arguments{
\item{modelObj}{Model object returned by \code{FitBoostTreeModel()}.}
\item{newdata}{New data frame containing all predictor columns.}
\item{threshold}{Positive-class probability threshold. When \code{NULL}, uses \code{modelObj$threshold} if present, otherwise 0.5.}
\item{engine_boost_tree}{Optional engine override; defaults to \code{modelObj$engine}.}
}
\description{Returns predicted class labels and both binary class probabilities for a fitted boosted tree model, using the tuned threshold by default when available.}
\value{A data frame with predicted class and class probabilities.}
