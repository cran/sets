set_options <-
local({
    options <- list(quote = FALSE)
    function(option = NULL, value = NULL) {
        if (is.null(option)) return(options)
        if (is.null(value))
            options[[option]]
        else
            options[[option]] <<- value
    }
})
