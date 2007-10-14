## * Element function, used for the creation of gsets by elements

e <-
function(x, memberships = 1L)
{
    .stop_if_memberships_are_invalid(memberships)
    structure(x, memberships = memberships, class = c("element", class(x)))
}

print.element <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

format.element <-
function(x, ...)
{
    x <- .remove_element_class(x)
    paste(paste(LABEL(x), collapse = " "),
          " [",
          paste(format(.get_memberships(x)), collapse = ", "),
          "]",
          sep = "")
}

LABEL.element <-
function(x)
    format(x)

