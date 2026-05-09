\name{FitBoostTreeModel}
\alias{FitBoostTreeModel}
\title{Fit a Tuned Boosted Tree Model}
\usage{
FitBoostTreeModel(
  formula,
  dataTrain,
  hyperparameters,
  featureTypes = NULL,
  targetLevels = NULL,
  scalePosWeight = NULL,
  nThreads = 8L,
  seed = 42L,
  verbose = 0L,
  engine_boost_tree = "lightgbm"
)
}
\arguments{
\item{formula}{A two-sided formula with one binary factor outcome and numeric predictors.}
\item{dataTrain}{Training data frame. The target must be a factor with exactly two levels.}
\item{hyperparameters}{Named list from \code{TuneBoostTree()} or equivalent canonical names. If \code{threshold} is present, it is stored for default prediction-time classification.}
\item{featureTypes}{Optional XGBoost feature type vector.}
\item{targetLevels}{Optional two-level target ordering.}
\item{scalePosWeight}{Optional positive-class weight.}
\item{nThreads}{Integer engine threads.}
\item{seed}{Integer random seed.}
\item{verbose}{Engine verbosity.}
\item{engine_boost_tree}{Engine name, \code{"xgboost"} or \code{"lightgbm"}.}
}
\description{Fits a final LightGBM or XGBoost binary boosted tree model from tuned hyperparameters and stores class metadata plus the tuned decision threshold when supplied.}
\value{A model object list containing the native model and metadata for prediction.}
