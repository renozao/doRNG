
# from pkgmaker 0.31
ns_get <- function (x, ns = NULL, ...){
    if (is.null(ns)) {
        ns <- gsub("^([^:]+)::.*", "\\1", x)
        x <- gsub(".*::([^:]+)$", "\\1", x)
    }
    if (!isNamespace(ns)) {
        ns <- tryCatch(asNamespace(ns), error = function(e) NULL)
        if (is.null(ns)) 
            return()
    }
    get0(x, envir = ns, ...)
}