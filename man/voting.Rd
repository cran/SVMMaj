% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{voting}
\alias{voting}
\alias{voting.tr}
\alias{voting.te}
\title{Congressional Voting Records Data Set}
\format{
\code{X} is a data frame with 434 congress members and 16 attributes: 16 key
votes identified by the Congressional Quarterly Almanac (CQA). All
attributes are binary values, with \code{1=} yes and \code{0=} no.

\tabular{ll}{ 
 \code{X1} \tab handicapped-infants \cr 
 \code{X2} \tab water-project-cost-sharing \cr 
 \code{X3} \tab adoption-of-the-budget-resolution \cr 
 \code{X4} \tab physician-fee-freeze \cr 
 \code{X5} \tab el-salvador-aid \cr 
 \code{X6} \tab religious-groups-in-schools \cr 
 \code{X7} \tab anti-satellite-test-ban \cr
 \code{X8} \tab aid-to-nicaraguan-contras \cr 
 \code{X9} \tab mx-missile \cr
 \code{X10} \tab immigration \cr 
 \code{X11} \tab synfuels-corporation-cutback \cr 
 \code{X12} \tab education-spending \cr 
 \code{X13} \tab superfund-right-to-sue \cr 
 \code{X14} \tab crime \cr 
 \code{X15} \tab duty-free-exports \cr 
 \code{X16} \tab export-administration-act-south-africa \cr 
 }

\code{y} consists factors which denotes whether the congress member is a
\code{Republican} or a \code{Democrat}.

The training set \code{voting.tr} contains a randomly selected set of 300
subjects, and \code{voting.te} contains the remaining 134 subjects.
\code{voting} contains all 434 objects.
}
\source{
Chih-Chung Chang and Chih-Jen Lin, LIBSVM : a library for support
vector machines, 2001. Software available at
\url{https://www.csie.ntu.edu.tw/~cjlin/libsvm/}.
}
\description{
1984 United Stated Congressional Voting Records; Classify as Republican or
Democrat.
}
\details{
This data set includes votes for each of the U.S. House of Representatives
Congressmen on the 16 key votes identified by the CQA. The CQA lists nine
different types of votes: voted for, paired for, and announced for (these
three simplified to yea), voted against, paired against, and announced
against (these three simplified to nay), voted present, voted present to
avoid conflict of interest, and did not vote or otherwise make a position
known (these three simplified to an unknown disposition).
}
\examples{

attach(voting)
summary(X)
summary(y)

}
\keyword{datasets}
