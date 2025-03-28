% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_irccc.R
\name{gg_irccc}
\alias{gg_irccc}
\title{visualize IRCCC from mirt or ltm object.}
\usage{
gg_irccc(
  object,
  item,
  theta = c(-3, 3),
  breaks = 100,
  grm = TRUE,
  monochrome = FALSE,
  use_ltm = FALSE,
  output.data = FALSE
)
}
\arguments{
\item{object}{mirt or grm object. If you use the grm object, you must set ltm = TRUE.}

\item{item}{numeric. The number of the item to be visualized. The length must be 1.}

\item{theta}{vector. The length must be 2. Specify the range of theta in the IRCCC to be visualized using c().}

\item{breaks}{numeric. Specifies the number of divisions of theta. For example, if you specify 1000, the range specified by theta will be divided into 1000 parts. The default is 100.}

\item{grm}{logical. The default is TRUE. If the object is GRM, set it to TRUE, and if it is a binary type, set it to FALSE.}

\item{monochrome}{logical. The default is FALSE, and IRCCC is output in color. If TRUE, IRCCC is output in black and white.}

\item{use_ltm}{logical. The default is FALSE. If you are using the mirt object, set this to FALSE. If you are using the grm object from the ltm package, set this option to TRUE. When use_ltm = TRUE, the scale factor D is corrected to 1.701.}

\item{output.data}{logical. The default is FALSE. If TRUE, instead of visualizing IRCCC, the data that forms the basis of IRCCC is output in long format.}
}
\value{
Visualize the IRCCC of the specified item from the mirt or ltm object (output.data = FALSE). Output data that can visualize the IRCCC of the specified item from the mirt or ltm object (output.data = TRUE).
}
\description{
\code{gg_irccc} visualize IRCCC from mirt or ltm object.
}
\examples{
#library(mirt)
#data(Bock1997)
#dat <- Bock1997[1:3]
#res <- mirt(dat, model = 1, itemtype = "graded")

#gg_irccc(object = res, item = 1)
#plot1 <- gg_irccc(object = res, item = 1)
#plot1 + theme_bw()

#library(ltm)
#res <- grm(dat[1:3], IRT.param = TRUE)

#gg_irccc(object = res, item = 1, use_ltm = TRUE)
#d <- gg_irccc(object = res, item = 1, output.data = TRUE)
}
