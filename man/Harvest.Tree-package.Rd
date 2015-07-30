\name{Harvest.Tree}
\alias{Harvest.Tree}
\alias{Harvest.Tree}
\docType{package}
\title{
Harvest the classification tree
}
\description{
Aimed at applying the Harvest classification tree algorithm, modified algorithm of classic classification tree.The harvested tree has advantage of deleting redundant rules in trees, leading to a simplify and more efficient tree model.It was firstly used in drug discovery field, but it also performs well in other kinds of data, especially when the region of a class is disconnected. This package also improves the basic harvest classification tree algorithm by extending the field of data of algorithm to both continuous and categorical variables.

To learn more about the harvest classification tree algorithm, you can go to http://www.stat.ubc.ca/Research/TechReports/techreports/220.pdf for more information.  


}
\details{
\tabular{ll}{
Package: \tab Harvest.Tree\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-07-30\cr
License: \tab GPL-2\cr
}
The main function of package called 'harvest', it can be used to analyze the data which is stored in a data frame, where first column stores the class of response data, and the second to last column stores explantory variables accordingly.The 'predict' funciton offers function to predict the unclassified data based on training model. The 'harfunc' function is the fundemental part of 'harvest', which can be used to analyze the data which has already been classified by rpart function(traditional classification tree). Please check the help file of these three functions for more information.
}
\author{
Bingyuan Liu \\
Yan Yuan \\
Qian Shi


Maintainer: Bingyuan Liu<adler1016@gmail.com>

}

