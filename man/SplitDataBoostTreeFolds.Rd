\name{SplitDataBoostTreeFolds}
\alias{SplitDataBoostTreeFolds}
\title{Dividir dados em folds estratificados para boost-tree}
\usage{SplitDataBoostTreeFolds(yData, nFolds = 10L, seed = 42L)}
\arguments{
\item{yData}{Vetor binário inteiro ou lógico da variável resposta.}
\item{nFolds}{Inteiro com número de folds.}
\item{seed}{Inteiro usado como semente aleatória.}
}
\description{Cria folds estratificados para dados binários 0/1, usando a mesma lógica de estratificação do tuner.}
\value{Lista de vetores inteiros. Cada elemento contém os índices de teste de um fold; os índices de treino são obtidos pelo complemento.}
