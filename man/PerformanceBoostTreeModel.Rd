\name{PerformanceBoostTreeModel}
\alias{PerformanceBoostTreeModel}
\title{Evaluate Boosted Tree Performance}
\usage{PerformanceBoostTreeModel(modelObj, testData, formula)}
\arguments{
\item{modelObj}{Model object returned by \code{FitBoostTreeModel()}.}
\item{testData}{Test data frame containing predictors and outcome.}
\item{formula}{A two-sided formula identifying the outcome.}
}
\description{Computes PR-AUC, a confusion summary, and predictions for a fitted boosted tree model on test data.}
\value{A list with PR-AUC, confusion summary, and predictions.}
