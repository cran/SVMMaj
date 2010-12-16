\name{AusCredit}
\alias{AusCredit}
\alias{AusCredit.tr}
\alias{AusCredit.te}
\docType{data}
\title{ Australian Credit Approval Dataset}
\description{
    This file concerns credit card applications of 690 households.  
}    

\usage{
AusCredit
AusCredit.tr
AusCredit.te
}

\format{
This data set has been split into two components for the convenience of the model
training.

\code{data.frame}-object \code{X} consists of with 6 numerical and 8 categorical attributes. The labels
have been changed for the convenience of the statistical algorithms. For example,
attribute 4 originally had 3 labels p,g,gg and these have been changed to labels 1,2,3.

Factor \code{y} indicates whether the application has been \code{Accepted} or \code{Rejected}

The training set \code{AusCredit.tr} contains a randomly selected set of 400 subjects, 
and \code{AusCredit.te} contains the remaining 290 subjects. \code{AusCredit} contains 
all 690 objects.
}

\details{    
    All attribute names
    and values have been changed to meaningless symbols to protect
    confidentiality of the data.

    This dataset is interesting because there is a good mix of
    attributes -- continuous, nominal with small numbers of
    values, and nominal with larger numbers of values.  There
    are also a few missing values.
    
}

\source{
Chih-Chung Chang and Chih-Jen Lin, LIBSVM : a library for support vector machines, 2001. 
Software available at \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}.

}

\examples{
attach(AusCredit)
summary(X)
summary(y)
detach(AusCredit)
}

\keyword{datasets}
