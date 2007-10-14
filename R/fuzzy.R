## Support for fuzzy relations needs the specification of negation,
## conjunction and disjunction via the functions N, T (t-norm) and S
## (t-conorm), respectively.
##
## We try to make our code "work" with arbitrary such triples by using
## functions .N., .T. and .S., respectively, and allow setting the fuzzy
## logic system via a dynamic variable.  If this turns out to be too
## cumbersome, we could also move back to hard-wiring the "usual" Zadeh
## connectives 1 minus, min and max, and provide ways to unlock the
## locked bindings as needed.

## .N. <- function(x) 1 - x
## .T. <- function(x, y) pmin(x, y)
## .S. <- function(x, y) pmax(x, y)
## .I. <- function(x, y) ifelse(x <= y, 1, y)

.N. <- function(x) fuzzy_logic()$N(x)
.T. <- function(x, y) fuzzy_logic()$T(x, y)
.S. <- function(x, y) fuzzy_logic()$S(x, y)
.I. <- function(x, y) fuzzy_logic()$I(x, y)

## Use dynamic variables for the fuzzy connectives.
## Note that there are also parametric fuzzy logic families, which we
## can set via
##   fuzzy_logic(NAME, PARAMS)

## One might think that using an environment for storing the fuzzy logic
## connectives, along the lines of
##   fuzzy_logic_db <- new.env()
## and adding
##   for(nm in names(family))
##     assign(nm, family[[nm]], envir = fuzzy_logic_db)
## and using
##   .N. <- function(...) fuzzy_logic_db$N(...)
## would be somewhat faster than
##   .N. <- function(...) fuzzy_logic()$N(...)
## but we could not find conclusive evidence for performance gains 
## either way.

fuzzy_logic <-
local({
    family <- NULL
    function(new, ...) {
        if(!missing(new))
            family <<- fuzzy_logic_families[[new]](...)
        else
            family
    }
})

fuzzy_logic_family <-
function(name, T, S, N, I = NULL, params = NULL)
{
    if(is.null(I))
        I <- function(x, y)
            stop("Implication not available.", call. = FALSE)
    structure(list(name = name, T = T, S = S, N = N, I = I,
                   params = params),
              class = "fuzzy_logic_family")
}

fuzzy_logic_family_Zadeh <-
function()
    fuzzy_logic_family(name = "Zadeh",
                       N = function(x) 1 - x,
                       T = function(x, y) pmin(x, y),
                       S = function(x, y) pmax(x, y),
                       I = function(x, y) ifelse(x <= y, 1, y))

fuzzy_logic_family_drastic <-
function()
    fuzzy_logic_family(name = "drastic",
                       N = function(x) 1 - x,
                       T = function(x, y)
                       ifelse(pmax(x, y) == 1, pmin(x, y), 0),
                       S = function(x, y)
                       ifelse(pmin(x, y) == 0, pmax(x, y), 1))

fuzzy_logic_family_product <-
function()
    fuzzy_logic_family(name = "product",
                       N = function(x) 1 - x,                        
                       T = function(x, y) x * y,
                       S = function(x, y) x + y - x * y,
                       I = function(x, y) pmin(y / x, 1))

fuzzy_logic_family_Lukasiewicz <-
function()
    fuzzy_logic_family(name = "Lukasiewicz", 
                       N = function(x) 1 - x,
                       T = function(x, y) pmax(x + y - 1, 0),
                       S = function(x, y) pmin(x + y, 1),
                       I = function(x, y) pmin(1 - x + y, 1))

fuzzy_logic_family_Fodor <-
function()
    fuzzy_logic_family(name = "Fodor",
                       N = function(x) 1 - x,
                       T = function(x, y)
                       ifelse(x + y > 1, pmin(x, y), 0),
                       S = function(x, y)
                       ifelse(x + y < 1, pmax(x, y), 1),
                       I = function(x, y)
                       ifelse(x <= y, 1, pmax(1 - x, y)))

## Frank family (Fodor & Roubens, page 20).
## One parameter family with 0 <= s <= \infty, where 0, 1 and \infty
## give Zadeh, product and Lukasiewicz, respectively.
fuzzy_logic_family_Frank <-
function(s)
{
    if(s < 0)
        stop("Invalid parameter.")
    if(s == 0) fuzzy_logic_family_Zadeh()
    else if(s == 1) fuzzy_logic_family_product()
    else if(s == Inf) fuzzy_logic_family_Lukasiewicz()
    else {
        T <- function(x, y)
            log(1 + (s^x - 1) * (s^y - 1) / (s - 1)) / log(s)
        fuzzy_logic_family(name = "Frank",
                           N = function(x) 1 - x,
                           T = T,
                           S = function(x, y) 1 - T(1 - x, 1 - y),
                           params = list(s = s))
    }
}

## Hamacher family (Fodor & Roubens, page 21).
## This is a three parameter family of connectives T_\alpha, S_\beta,
## N_\gamma with \alpha >= 0, \beta, \gamma >= -1, which is a de Morgan
## triple iff \gamma > -1 and \alpha = (1 + \beta) / (1 + \gamma).
## Be nice and provide some default values ...
fuzzy_logic_family_Hamacher <-
function(alpha = NULL, beta = 0, gamma = 0)
{
    if(is.null(alpha))
        alpha <- (1 + beta) / (1 + gamma)
    else if((alpha < 0) || (beta < -1) || (gamma < -1))
        stop("Invalid parameter.")
    fuzzy_logic_family(name = "Hamacher",
                       N = function(x) (1 - x) / (1 + gamma * x),
                       T = function(x, y)
                       ifelse(x * y == 0,
                              0,
                              x * y /
                              (alpha + (1 - alpha) * (x + y - x * y))),
                       S = function(x, y)
                       (x + y + (beta - 1) * x * y) /
                        (1 + beta * x * y),
                       params =
                       list(alpha = alpha, beta = beta, gamma = gamma))
}

## The following "families" are really families of t-norms, which we
## leverage into fuzzy logic families using standard negation and de
## Morgan triplet associated t-conorm.

## Schweizer-Sklar.
fuzzy_logic_family_Schweizer_Sklar <-
function(p)
{
    if(p == -Inf) fuzzy_logic_family_Zadeh()
    else if(p == 0) fuzzy_logic_family_product()
    else if(p == Inf) fuzzy_logic_family_drastic()
    else {
        T <- if(p < 0)
            function(x, y) (x^p + y^p - 1) ^ (1/p)
        else
            function(x, y) pmax(0, (x^p + y^p - 1) ^ (1/p))
        fuzzy_logic_family(name = "Schweizer-Sklar",
                       N = function(x) 1 - x,
                       T = T,
                       S = function(x, y) 1 - T(1 - x, 1 - y)
                       )
    }
}

## Yager t-norm.
fuzzy_logic_family_Yager <-
function(p)
{
    if(p < 0)
        stop("Invalid parameter.")
    if(p == 0) fuzzy_logic_family_drastic()
    else if(p == Inf) fuzzy_logic_family_Zadeh()
    else {
        fuzzy_logic_family(name = "Yager",
                           N = function(x) 1 - x,
                           T = function(x, y)
                           pmax(0, 1 - ((1 - x)^p + (1 - y)^p)^(1/p)),
                           S = function(x, y)
                           pmin(1, (x^p + y^p) ^ (1/p))
                           )
    }
}

## Dombi t-norm.
fuzzy_logic_family_Dombi <-
function(p)
{
    if(p < 0)
        stop("Invalid parameter.")
    if(p == 0) fuzzy_logic_family_drastic()
    else if(p == Inf) fuzzy_logic_family_Zadeh()
    else {
        T <- function(x, y)
            ifelse(x * y == 0,
                   0,
                   1 / (1 + ((1 / x - 1)^p + (1 / y - 1)^p)^(1 / p)))
        fuzzy_logic_family(name = "Dombi",
                           N = function(x) 1 - x,
                           T = T,
                           S = function(x, y) 1 - T(1 - x, 1 - y)
                           )
    }
}
    
## Aczel-Alsina t-norm.
fuzzy_logic_family_Aczel_Alsina <-
function(p)
{
    if(p < 0)
        stop("Invalid parameter.")
    if(p == 0) fuzzy_logic_family_drastic()
    else if(p == Inf) fuzzy_logic_family_Zadeh()
    else {
        T <- function(x, y) exp(- (abs(log(x))^p + abs(log(y))^p))
        fuzzy_logic_family(name = "Aczel-Alsina",
                           N = function(x) 1 - x,
                           T = T,
                           S = function(x, y) 1 - T(1 - x, 1 - y)
                           )
    }
}

## Sugeno-Weber t-norm.
fuzzy_logic_family_Sugeno_Weber <-
function(p)
{
    if(p < -1)
        stop("Invalid parameter.")
    if(p == -1) fuzzy_logic_family_drastic()
    else if(p == Inf) fuzzy_logic_family_product()
    else
        fuzzy_logic_family(name = "Sugeno-Weber",
                           N = function(x) 1 - x,
                           T = function(x, y)
                           pmax(0, (x + y - 1 + p * x * y) / (1 + p)),
                           S = function(x, y)
                           pmin(1, x + y - p * x * y / (1 + p))
                           )
}


fuzzy_logic_families <-
    list(Zadeh = fuzzy_logic_family_Zadeh,
         drastic = fuzzy_logic_family_drastic,
         product = fuzzy_logic_family_product,
         Lukasiewicz = fuzzy_logic_family_Lukasiewicz,
         Fodor = fuzzy_logic_family_Fodor,
         Frank = fuzzy_logic_family_Frank,
         Hamacher = fuzzy_logic_family_Hamacher,
         "Schweizer-Sklar" = fuzzy_logic_family_Schweizer_Sklar,
         Yager = fuzzy_logic_family_Yager,
         Dombi = fuzzy_logic_family_Dombi,
         "Aczel-Alsina" = fuzzy_logic_family_Aczel_Alsina,
         "Sugeno-Weber" = fuzzy_logic_family_Sugeno_Weber
         )
