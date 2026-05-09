\name{PerformanceBoostTreeModel}
\alias{PerformanceBoostTreeModel}
\title{Avaliar performance de modelo boosted tree}
\usage{PerformanceBoostTreeModel(modelObj, testData, formula)}
\arguments{
\item{modelObj}{Objeto de modelo retornado por \code{FitBoostTreeModel()}.}
\item{testData}{data.frame de teste contendo preditores e variável resposta.}
\item{formula}{Fórmula de duas faces que identifica a variável resposta.}
}
\description{Chama \code{PredictBoostTreeModel()} internamente e calcula PR-AUC e uma tabela de confusão resumida.}
\value{Lista com \code{prAuc}, \code{confusionSummary} e \code{predictions}. \code{predictions} é o tibble retornado por \code{PredictBoostTreeModel()}.}
