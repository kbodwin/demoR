% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo_chunk.R
\name{code_from_editor}
\alias{code_from_editor}
\title{Converts raw editor text to a string of code}
\usage{
code_from_editor(.contents, label)
}
\arguments{
\item{.contents}{}
}
\value{
chunk text
}
\description{
Raw editor text has been taken from an active RStudio session via \code{rstudioapi::getSourceEditorContext()}.  Chunk delimiters and html is removed, all formatting is otherwise perserved.
}
