
# RAND_VAR ----------------------------------------------------------------



#' RAND_VAR class environment
#'
#' @name RAND_VAR
#'
#' @description This is the base class of random variable, inherited from
#' [BASE].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [BASE]
#' \cr
#' \cr
#' New attributes: [RAND_VAR$dist], [RAND_VAR$prm]
#' \cr
#' \cr
#' New methods: [RAND_VAR$..init..], [RAND_VAR$..str..],
#' [RAND_VAR$E], [RAND_VAR$Var], [RAND_VAR$set_prm]
#' @export
RAND_VAR <- class_RAND_VAR()

#' Distribution name
#'
#' @name RAND_VAR$dist
#'
#' @description A string, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$dist

#' List of parameters
#'
#' @name RAND_VAR$prm
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$prm

#' Initialization method
#'
#' @name RAND_VAR$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param dist Character. Distribution name.
#' @param prm List. List of parameters.
#' @return Return the object itself.
#'
#' @examples
#'
#' RAND_VAR$..init..
#'
#' # Instantiate
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$..init..

#' Expectation of the random variable
#'
#' @name RAND_VAR$E
#'
#' @description Expectation of the random variable.
#' User could override this method in derived class.
#' @return NA
#'
#' @examples
#'
#' RAND_VAR$E()
RAND_VAR$E

#' Variance of the random variable
#'
#' @name RAND_VAR$Var
#'
#' @description Variance of the random variable.
#' User could override this method in derived class.
#' @return NA
#'
#' @examples
#'
#' RAND_VAR$Var()
RAND_VAR$Var

#' Generate random values
#'
#' @name RAND_VAR$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @param ... Ignored.
#' @return NA. User needs to define their own `gen` method in derived class.
#'
#' @examples
#'
#' test <- RAND_VAR$gen(10)
RAND_VAR$gen

#' Generate random values
#'
#' @name RAND_VAR$set_prm
#'
#' @description This function updates the parameters.
#' @param prm_name List or Vector. A sequence of character parameter names.
#' @param prm_value List or Vector. A sequence of parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' test <- rand_var()
#' test$set_prm("a", 1)
#' test
RAND_VAR$set_prm

#' String representation of the object
#'
#' @name RAND_VAR$..str..
#'
#' @description This function returns a string representation of the object.
#' @return A string.
#'
#' @examples
#'
#' RAND_VAR$..str..()
#'
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$..str..()
RAND_VAR$..str..



# RAND_UNIFORM ------------------------------------------------------------

#' RAND_UNIFORM class environment
#'
#' @name RAND_UNIFORM
#'
#' @description This is the class of the uniform random variable, inherited from
#' [RAND_VAR].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [RAND_VAR]
#' \cr
#' \cr
#' New methods: [RAND_UNIFORM$..init..], [RAND_UNIFORM$gen],
#' RAND_UNIFORM$E, RAND_UNIFORM$Var
#' @export
RAND_UNIFORM <- class_RAND_UNIFORM()

#' Initialization method
#'
#' @name RAND_UNIFORM$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param a Numeric. Lower bound. Default is 0.
#' @param b Numeric. Upper bound. Default is 1.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_uniform(a = 1, b = 2)
#' test
RAND_UNIFORM$..init..

#' Generate random values
#'
#' @name RAND_UNIFORM$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @param a Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param b Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::runif()]
#'
#' @examples
#'
#' test <- rand_uniform(a = 1, b = 2)
#' test$gen(10)
#'
#' test$gen(3, a = c(1,2,3), b = c(2,3,4))
RAND_UNIFORM$gen



# RAND_NORMAL -------------------------------------------------------------

#' RAND_NORMAL class environment
#'
#' @name RAND_NORMAL
#'
#' @description This is the class of the normal random variable, inherited from
#' [RAND_VAR].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [RAND_VAR]
#' \cr
#' \cr
#' New methods: [RAND_NORMAL$..init..], [RAND_NORMAL$gen],
#' RAND_NORMAL$E, RAND_NORMAL$Var
#' @export
RAND_NORMAL <- class_RAND_NORMAL()

#' Initialization method
#'
#' @name RAND_NORMAL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param mu Numeric. Mean. Default is 0.
#' @param sigma Numeric. Standard deviation. Default is 1.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_normal(mu = 1, sigma = 2)
#' test
RAND_NORMAL$..init..

#' Generate random values
#'
#' @name RAND_NORMAL$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @param mu Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param sigma Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::rnorm()]
#'
#' @examples
#'
#' test <- rand_normal(mu = 1, sigma = 2)
#' test$gen(10)
#'
#' test$gen(3, mu = c(0,1,2), sigma = c(1,2,4))
RAND_NORMAL$gen


# RAND_LOGNORMAL ----------------------------------------------------------

#' RAND_LOGNORMAL class environment
#'
#' @name RAND_LOGNORMAL
#'
#' @description This is the class of the log-normal random variable, inherited from
#' [RAND_VAR].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [RAND_VAR]
#' \cr
#' \cr
#' New methods: [RAND_LOGNORMAL$..init..], [RAND_LOGNORMAL$gen],
#' RAND_LOGNORMAL$E, RAND_LOGNORMAL$Var
#' @export
RAND_LOGNORMAL <- class_RAND_LOGNORMAL()

#' Initialization method
#'
#' @name RAND_LOGNORMAL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param mu Numeric. Mean of the log of the random variable. Default is 0.
#' @param sigma Numeric. Standard deviation of the log the random variable.
#' Default is 1.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_lognormal(mu = 1, sigma = 2)
#' test
RAND_LOGNORMAL$..init..

#' Generate random values
#'
#' @name RAND_LOGNORMAL$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @param mu Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param sigma Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::rlnorm()]
#'
#' @examples
#'
#' test <- rand_lognormal(mu = 1, sigma = 2)
#' test$gen(10)
#'
#' test$gen(3, mu = c(0,1,2), sigma = c(1,2,3))
RAND_LOGNORMAL$gen



# RAND_UNIFORM_D ----------------------------------------------------------


#' RAND_UNIFORM_D class environment
#'
#' @name RAND_UNIFORM_D
#'
#' @description This is the class of the discrete uniform random variable, inherited from
#' [RAND_VAR].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [RAND_VAR]
#' \cr
#' \cr
#' New methods: [RAND_UNIFORM_D$..init..], [RAND_UNIFORM_D$gen],
#' RAND_UNIFORM_D$E, RAND_UNIFORM_D$Var
#' @export
RAND_UNIFORM_D <- class_RAND_UNIFORM_D()

#' Initialization method
#'
#' @name RAND_UNIFORM_D$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param a Numeric. Lower bound. Default is 0.
#' @param b Numeric. Upper bound. Default is 1.
#' @param k Integer. Number of unique discrete values. Default is 5.
#' @param even Boolean. Whether or not candidate values are evenly spaced.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_uniform_d(a = 1, b = 2, k = 3)
#' test
RAND_UNIFORM_D$..init..

#' Generate random values
#'
#' @name RAND_UNIFORM_D$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @param a Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param b Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param k Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param even Boolean. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::runif()], [sample()]
#'
#' @examples
#'
#' test <- rand_uniform_d(a = 1, b = 2, k = 2, even = TRUE)
#' test$gen(10)
#'
#' test$gen(3, a = c(1,2,3), b = c(2,3,4), k = 1, even = c(TRUE, TRUE, FALSE))
RAND_UNIFORM_D$gen
