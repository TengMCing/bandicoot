
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bandicoot

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/TengMCing/bandicoot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/TengMCing/bandicoot?branch=master)
<!-- badges: end -->

The goal of bandicoot is to provide a set of tools for building
light-weight object oriented system, which has Python-like syntax and
duner methods for simplicity, but uses static dispatch for less
overhead. This system also allows multiple inheritances and provides
Python-like method resolution order for the possibility of implementing
dynamic dispatch by users.

This system is inspired by the OOP systems implemented in
[R6](https://github.com/r-lib/R6) and
[Python](https://github.com/python/cpython).

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TengMCing/bandicoot")
```

## 1. bandicoot OOP system

``` r
library(bandicoot)
```

**1.1. Define a new class**

A class can be defined with the `new_class` function. All positional
arguments are for specifying parent classes, `BASE` is the base object
class provided by the package, you don’t need to manually specify it.
But if you would like to have advanced behaviour, you can try to
implement your own `object` class.

Class name is mandatory and should be unique.

``` r
# You don't actually need to specify BASE here. This is only for demonstration.
DEMO <- new_class(BASE, class_name = "DEMO")
DEMO
#> 
#> ── <DEMO class>
```

The object is an environment containing some useful attributes and
methods.

- `OBJECT$..type..` gives the current class name.
- `OBJECT$..class..` gives the current class name and parent class
  names.

``` r
DEMO$..type..
#> [1] "DEMO"
DEMO$..class..
#> [1] "DEMO" "BASE"
```

- `OBJECT$..dir..()` returns all names of attribute and method of the
  object.
- `OBJECT$..methods..()` returns all names of method of the object

``` r
DEMO$..dir..()
#>  [1] "..mro.."          "..bases.."        "..str.."          "..len.."         
#>  [5] "..class.."        "..new.."          "has_attr"         "del_attr"        
#>  [9] "..repr.."         "set_attr"         "..type.."         "get_attr"        
#> [13] "..dir.."          "..methods.."      "..method_env.."   "..instantiated.."
#> [17] "..init.."         "..class_tree.."   "instantiate"
DEMO$..methods..()
#>  [1] "..str.."     "..len.."     "..new.."     "has_attr"    "del_attr"   
#>  [6] "..repr.."    "set_attr"    "get_attr"    "..dir.."     "..methods.."
#> [11] "..init.."    "instantiate"
```

- `OBJECT$..str..()` returns a string representation of the object,
  which will be used by the S3 `print()` method. This method usually
  needs to be overridden in subclass to give short summary of the
  object.

``` r
DEMO$..str..()
#> [1] "<DEMO class>"
```

**1.2. Register a method for the class**

Methods can be registered by using `register_method()`. The first
argument is the object you want to bind the function to, the rest of the
positional arguments are for specifying method names and functions. The
syntax is `method_name = function`.

You can choose to write inline function or pass pre-defined function.
The associative environment of the function doesn’t matter, it will be
modified by the `register_method()` function.

``` r
pre_defined_fn <- function() 1 + 2

register_method(DEMO, inline_fn = function() 1 + 1, pre_defined_fn = pre_defined_fn)

DEMO$inline_fn()
#> [1] 2
DEMO$pre_defined_fn()
#> [1] 3
```

For method that needs to access the object itself, just simply use
`self` in your method. It is an reference to the object.

``` r
DEMO$val <- 5

register_method(DEMO, get_val = function() self$val)

DEMO$get_val()
#> [1] 5
```

**1.3. Override the `..init..()` method**

`..init..()` method is for instance initialization. To override the
`..init..()` method, you need to use the `register_method()` to register
it again.

``` r
init <- function(first_name, employee_id) {
  self$first_name <- first_name
  self$employee_id <- employee_id
}

register_method(DEMO, ..init.. = init)
```

Now the class requires two two arguments `first_name` and `employee_id`
to initialize the instance.

**1.4. Build an instance**

To new and initialize an instance, you need to use the `instantiate()`
method. The output will show it is an object.

``` r
mike <- DEMO$instantiate("Mike", 25)
mike
#> 
#> ── <DEMO object>
```

`first_name` and `employee_id` are stored in the object because of the
`..init..()` method.

``` r
mike$first_name
#> [1] "Mike"
mike$employee_id
#> [1] 25
```

**1.5. A complete workflow**

It is recommend to write your class definition in a function to make
debugging easier. The following example new a class `DEMO_2`, defines
its own `..init..()` method, defines a `get_email()` function for
retrieving the email address, defines its own `..str..()` method such
that when we print the object, it will provide us with a nicely
formatted summary.

`super()` returns the next class of the method resolution order, which
will always be the parent class in single inheritance, but not necessary
in multiple inheritance.

`use_method()` is used to run methods from other classes, which in this
case, the `..str..()` method from the parent class (`BASE`).

``` r
class_DEMO_2 <- function(env = new.env(parent = parent.frame())) {
  
  new_class(env = env, class_name = "DEMO_2")
  
  init_ <- function(first_name, employee_id) {
    self$first_name <- first_name
    self$employee_id <- employee_id
  }
  
  get_email_ <- function() {
    paste0(self$first_name, "_", self$employee_id, "@company.com")
  }
  
  str_ <- function() {
    paste(use_method(self, super()$..str..)(), 
          paste("Name:", self$first_name,
                "\nEmployee ID:", self$employee_id,
                "\nEmail:", self$get_email()), 
          sep = "\n")
  }
  
  register_method(env,
                  ..init.. = init_,
                  get_email = get_email_,
                  ..str.. = str_)
  
  return(env)
}
```

``` r
DEMO_2 <- class_DEMO_2()
mike <- DEMO_2$instantiate("Mike", 25)
mike$get_email()
#> [1] "Mike_25@company.com"
```

``` r
mike$..str..()
#> [1] "<DEMO_2 object>\nName: Mike \nEmployee ID: 25 \nEmail: Mike_25@company.com"
mike
#> 
#> ── <DEMO_2 object>
#> Name: Mike 
#> Employee ID: 25 
#> Email: Mike_25@company.com
```
