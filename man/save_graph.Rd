% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creation.R
\name{save_graph}
\alias{save_graph}
\title{Saves a graph to file}
\usage{
save_graph(g, file, filetype = "graphml", highlight_path = NULL)
}
\arguments{
\item{g}{The nctx graph}

\item{file}{The filename to save the graph to}

\item{filetype}{Choose the filetype of the file. This can be either \code{graphml} or \code{dot}.}

\item{highlight_path}{A path that will be highlighted in the output file. Note that this is only considered when exporting to \code{dot}}
}
\description{
This saves a nctx graph to a file. When exporting to \code{dot}, you can specify a path to be highlighted.
}
\details{
For file formats, \code{dot} and \code{GraphML} are currently supported. Use \code{GraphML} if you wish to export attribute information associated to notes (see also \code{\link{set_vertex_attributes}} how to associate information to vertices for export).

See https://en.wikipedia.org/wiki/GraphML for further information about the \code{GraphML} format and https://en.wikipedia.org/wiki/DOT_(graph_description_language) for more information about the \code{dot} format.
}
