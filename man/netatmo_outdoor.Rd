% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obj_sensor_out.R
\name{netatmo_outdoor}
\alias{netatmo_outdoor}
\title{netatmo_outdoor object R6}
\description{
netatmo_outdoor object R6

netatmo_outdoor object R6
}
\examples{
dir_csv <- "f:/r/netatmo/csv"
file_lst <- netatmo_file_to_load(dir_csv, pattern = list("indoor", "outdoor"))
db_lst <- load_csv(file_lst)
boo <- netatmo_outdoor$new(db_lst$db_netatmo_outdoor)
}
\section{Super class}{
\code{\link[Rnetatmo:default_data_frame]{Rnetatmo::default_data_frame}} -> \code{netatmo_outdoor}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{netatmo_outdoor$new()}}
\item \href{#method-finalize}{\code{netatmo_outdoor$finalize()}}
\item \href{#method-clone}{\code{netatmo_outdoor$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="head">}\href{../../Rnetatmo/html/default_data_frame.html#method-head}{\code{Rnetatmo::default_data_frame$head()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="print">}\href{../../Rnetatmo/html/default_data_frame.html#method-print}{\code{Rnetatmo::default_data_frame$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="subset">}\href{../../Rnetatmo/html/default_data_frame.html#method-subset}{\code{Rnetatmo::default_data_frame$subset()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="summary">}\href{../../Rnetatmo/html/default_data_frame.html#method-summary}{\code{Rnetatmo::default_data_frame$summary()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="tail">}\href{../../Rnetatmo/html/default_data_frame.html#method-tail}{\code{Rnetatmo::default_data_frame$tail()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="view">}\href{../../Rnetatmo/html/default_data_frame.html#method-view}{\code{Rnetatmo::default_data_frame$view()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Rnetatmo" data-topic="default_data_frame" data-id="write_file">}\href{../../Rnetatmo/html/default_data_frame.html#method-write_file}{\code{Rnetatmo::default_data_frame$write_file()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{netatmo_outdoor$new(dataset)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataset}}{dataset}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-finalize}{}}}
\subsection{Method \code{finalize()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{netatmo_outdoor$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{netatmo_outdoor$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
