#' Supermarket data 1996
#'
#' This
#' @name supermarket1996
#' @docType data
#' @format This dataframe contains the following columns
#' \describe{
#'   \item{STORE}{Identifier of the store}
#'   \item{CITY}{The city of the store}
#'   \item{ZIP}{The zip code of the store}
#'   \item{GROCERY_sum}{}
#'   \item{GROCCOUP_sum}{}
#'   \item{AGE9}{}
#'   \item{AGE60}{}
#'   \item{ETHNIC}{}
#'   \item{EDUC}{}
#'   \item{NOCAR}{}
#'   \item{INCOME}{}
#'   \item{INCSIGMA}{}
#'   \item{HSIZEAVG}{}
#'   \item{HSIZE1}{}
#'   \item{HSIZE2}{}
#'   \item{HSIZE34}{}
#'   \item{HSIZE567}{}
#'   \item{HH3PLUS}{}
#'   \item{HH4PLUS}{}
#'   \item{HHSINGLE}{}
#'   \item{HHLARGE}{}
#'   \item{WORKWOM}{}
#'   \item{SINHOUSE}{}
#'   \item{DENSITY}{}
#'   \item{HVAL150}{}
#'   \item{HVAL200}{}
#'   \item{HVALMEAN}{}
#'   \item{SINGLE}{}
#'   \item{RETIRED}{}
#'   \item{UNEMP}{}
#'   \item{WRKCH5}{}
#'   \item{WRKCH17}{}
#'   \item{NWRKCH5}{}
#'   \item{NWRKCH17}{}
#'   \item{WRKCH}{}
#'   \item{NWRKCH}{}
#'   \item{WRKWCH}{}
#'   \item{WRKWNCH}{}
#'   \item{TELEPHN}{}
#'   \item{MORTGAGE}{}
#'   \item{NWHITE}{}
#'   \item{POVERTY}{}
#'   \item{SHPCONS}{}
#'   \item{SHPHURR}{}
#'   \item{SHPAVID}{}
#'   \item{SHPKSTR}{}
#'   \item{SHPUNFT}{}
#'   \item{SHPBIRD}{}
#'   \item{SHOPINDX}{}
#'   \item{SHPINDX}{}
#' }
#' @keywords datasets
#' @examples
#' head(supermarket1996, 3)
NULL

#' Australian Credit Approval Dataset
#'
#' This file concerns credit card applications of 690 households.
#'
#' All attribute names and values have been changed to meaningless symbols to
#' protect confidentiality of the data.
#'
#' This dataset is interesting because there is a good mix of attributes --
#' continuous, nominal with small numbers of values, and nominal with larger
#' numbers of values.  There are also a few missing values.
#'
#' @name AusCredit
#' @aliases AusCredit AusCredit.te AusCredit.tr
#' @docType data
#' @format This data set has been split into two components for the convenience
#' of the model training.
#'
#' \code{data.frame}-object \code{X} consists of with 6 numerical and 8
#' categorical attributes. The labels have been changed for the convenience of
#' the statistical algorithms. For example, attribute 4 originally had 3 labels
#' p,g,gg and these have been changed to labels 1,2,3.
#'
#' Factor \code{y} indicates whether the application has been \code{Accepted}
#' or \code{Rejected}
#'
#' The training set \code{AusCredit.tr} contains a randomly selected set of 400
#' subjects, and \code{AusCredit.te} contains the remaining 290 subjects.
#' \code{AusCredit} contains all 690 objects.
#' @source Chih-Chung Chang and Chih-Jen Lin, LIBSVM : a library for support
#' vector machines, 2001.  Software available at
#' \url{https://www.csie.ntu.edu.tw/~cjlin/libsvm/}.
#' @keywords datasets
#' @examples
#'
#' attach(AusCredit)
#' summary(X)
#' summary(y)
#' detach(AusCredit)
#'
NULL

#' Pima Indians Diabetes Data Set
#'
#' From National Institute of Diabetes and Digestive and Kidney Diseases.
#'
#' Several constraints were placed on the selection of these instances from a
#' larger database.  In particular, all patients here are females at least 21
#' years old of Pima Indian heritage.
#'
#' @name diabetes
#' @aliases diabetes diabetes.tr diabetes.te
#' @docType data
#' @format
#'
#' \code{X} is a data frame of 768 female patients with 8 attributes.
#'
#' \tabular{ll}{
#'  \code{no.pregnant} \tab number of pregnancies. \cr
#'  \code{glucose} \tab plasma glucose concentration in an oral glucose
#'                      tolerance test \cr
#'  \code{blood.press} \tab diastolic blood pressure (mm Hg) \cr
#'  \code{triceps.thick} \tab triceps skin fold thickness (mm) \cr
#'  \code{insulin} \tab 2-Hour serum insulin (mu U/ml) \cr
#'  \code{BMI} \tab body mass index (weight in kg/(height in m)**2) \cr
#'  \code{pedigree} \tab diabetes pedigree function \cr
#'  \code{age} \tab age in years
#' }
#'
#' \code{y} contains the class labels: \code{Yes} or No, for diabetic according
#' to WHO criteria.
#'
#' The training set \code{diabetes.tr} contains a randomly selected set of 600
#' subjects, and \code{diabetes.te} contains the remaining 168 subjects.
#' \code{diabetes} contains all 768 objects.
#' @references Smith, J.W., Everhart, J.E., Dickson, W.C., Knowler, W.C., &
#' Johannes, R.S. (1988). Using the ADAP learning algorithm to forecast the
#' onset of diabetes mellitus. In Proceedings of the \emph{Symposium on
#' Computer Applications and Medical Care} (pp. 261--265). IEEE Computer
#' Society Press.
#' @source Chih-Chung Chang and Chih-Jen Lin, LIBSVM : a library for support
#' vector machines, 2001. Software available at
#' \url{https://www.csie.ntu.edu.tw/~cjlin/libsvm/}.
#' @keywords datasets
#' @examples
#'
#' attach(diabetes)
#' summary(X)
#' summary(y)
#'
NULL

#' Congressional Voting Records Data Set
#'
#' 1984 United Stated Congressional Voting Records; Classify as Republican or
#' Democrat.
#'
#' This data set includes votes for each of the U.S. House of Representatives
#' Congressmen on the 16 key votes identified by the CQA. The CQA lists nine
#' different types of votes: voted for, paired for, and announced for (these
#' three simplified to yea), voted against, paired against, and announced
#' against (these three simplified to nay), voted present, voted present to
#' avoid conflict of interest, and did not vote or otherwise make a position
#' known (these three simplified to an unknown disposition).
#'
#' @name voting
#' @aliases voting voting.tr voting.te
#' @docType data
#' @format
#'
#' \code{X} is a data frame with 434 congress members and 16 attributes: 16 key
#' votes identified by the Congressional Quarterly Almanac (CQA). All
#' attributes are binary values, with \code{1=} yes and \code{0=} no.
#'
#' \tabular{ll}{
#'  \code{X1} \tab handicapped-infants \cr
#'  \code{X2} \tab water-project-cost-sharing \cr
#'  \code{X3} \tab adoption-of-the-budget-resolution \cr
#'  \code{X4} \tab physician-fee-freeze \cr
#'  \code{X5} \tab el-salvador-aid \cr
#'  \code{X6} \tab religious-groups-in-schools \cr
#'  \code{X7} \tab anti-satellite-test-ban \cr
#'  \code{X8} \tab aid-to-nicaraguan-contras \cr
#'  \code{X9} \tab mx-missile \cr
#'  \code{X10} \tab immigration \cr
#'  \code{X11} \tab synfuels-corporation-cutback \cr
#'  \code{X12} \tab education-spending \cr
#'  \code{X13} \tab superfund-right-to-sue \cr
#'  \code{X14} \tab crime \cr
#'  \code{X15} \tab duty-free-exports \cr
#'  \code{X16} \tab export-administration-act-south-africa \cr
#'  }
#'
#' \code{y} consists factors which denotes whether the congress member is a
#' \code{Republican} or a \code{Democrat}.
#'
#' The training set \code{voting.tr} contains a randomly selected set of 300
#' subjects, and \code{voting.te} contains the remaining 134 subjects.
#' \code{voting} contains all 434 objects.
#' @source Chih-Chung Chang and Chih-Jen Lin, LIBSVM : a library for support
#' vector machines, 2001. Software available at
#' \url{https://www.csie.ntu.edu.tw/~cjlin/libsvm/}.
#' @keywords datasets
#' @examples
#'
#' attach(voting)
#' summary(X)
#' summary(y)
#'
NULL
