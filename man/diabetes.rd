\name{diabetes}
\alias{diabetes}
\alias{diabetes.tr}
\alias{diabetes.te}
\docType{data}
\title{ Pima Indians Diabetes Data Set  }
\description{
     From National Institute of Diabetes and Digestive and Kidney Diseases.
}

\usage{
diabetes
diabetes.tr
diabetes.te
}

\format{

\code{X} is a data frame of 768 female patients with 8 attributes.

  \tabular{ll}{
    \code{no.pregnant}  \tab number of pregnancies.   \cr
    \code{glucose}      \tab plasma glucose concentration in an oral glucose tolerance test  \cr
    \code{blood.press}  \tab diastolic blood pressure (mm Hg)     \cr
    \code{triceps.thick}\tab triceps skin fold thickness (mm)   \cr
    \code{insulin}      \tab  2-Hour serum insulin (mu U/ml)       \cr
    \code{BMI}          \tab body mass index (weight in kg/(height in m)\^2)   \cr
    \code{pedigree}     \tab diabetes pedigree function   \cr
    \code{age}          \tab age in years

  }
  
\code{y} contains the class labels: \code{Yes} or {No}, for diabetic according to WHO criteria.

The training set \code{diabetes.tr} contains a randomly selected set of 600 subjects, 
and \code{diabetes.te} contains the remaining 168 subjects. \code{diabetes} contains 
all 768 objects.

}

\details{
      Several constraints were placed on the selection of these instances from
      a larger database.  In particular, all patients here are females at
      least 21 years old of Pima Indian heritage.
}

\source{
Chih-Chung Chang and Chih-Jen Lin, LIBSVM : a library for support vector machines, 2001.
Software available at \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}.

}

\references{
Smith, J.W., Everhart, J.E., Dickson, W.C., Knowler, W.C., & Johannes, R.S. (1988).
Using the ADAP learning algorithm to forecast the onset of diabetes mellitus.
In Proceedings of the \emph{Symposium on Computer Applications and Medical Care} (pp. 261--265).
IEEE Computer Society Press.
}

\examples{
attach(diabetes)
summary(X)
summary(y)
}

\keyword{datasets}

