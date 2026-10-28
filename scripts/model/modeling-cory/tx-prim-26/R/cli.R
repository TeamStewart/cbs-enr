#' Build a CLI argument parser from a roxygen-documented function
#'
#' Parses an R source file with \code{roxygen2::parse_file()} to extract
#' documentation and function formals, and returns a closure that accepts
#' \code{commandArgs(trailingOnly = TRUE)} and dispatches to the wrapped
#' function with type-coerced arguments.
#'
#' Logical parameters become boolean flags: \code{--flag} / \code{--no-flag}.
#' Character-vector parameters accept comma-separated values.
#' A \code{-h} / \code{--help} flag prints formatted documentation and exits.
#'
#' @param file Path to an R source file containing the function.
#' @param fn_name Name of the function to wrap. If \code{NULL}, uses the first
#'   documented function found in \code{file}.
#' @returns A function \code{f(args)} that parses \code{args} (defaulting to
#'   \code{commandArgs(trailingOnly = TRUE)}) and calls the wrapped function.
make_arg_parser <- function(file, fn_name = NULL) {
    caller_env <- parent.frame()
    blocks <- roxygen2::parse_file(file)

    get_block_fn_name <- function(b) {
        if (!is.null(b$object) && !is.null(b$object$alias)) return(b$object$alias)
        nm_tags <- Filter(function(t) t$tag == "name", b$tags)
        if (length(nm_tags)) nm_tags[[1L]]$val else NULL
    }

    if (is.null(fn_name)) {
        block <- blocks[[1L]]
        fn_name <- get_block_fn_name(block)
    } else {
        idx <- which(vapply(blocks, function(b) identical(get_block_fn_name(b), fn_name), FALSE))
        if (!length(idx)) stop("Function '", fn_name, "' not found in '", file, "'")
        block <- blocks[[idx[[1L]]]]
    }

    fn <- block$object$value
    if (!is.function(fn)) stop("'", fn_name, "' is not a function")
    params <- formals(fn)
    get_tags <- function(nm) Filter(function(t) t$tag == nm, block$tags)

    title_tags <- get_tags("title")
    title <- if (length(title_tags)) trimws(title_tags[[1L]]$val) else fn_name

    desc_tags <- get_tags("description")
    description <- if (length(desc_tags)) trimws(desc_tags[[1L]]$val) else ""

    param_tags <- get_tags("param")
    param_docs <- structure(
        vapply(param_tags, function(t) trimws(t$val$description), ""),
        names = vapply(param_tags, function(t) t$val$name, "")
    )

    function(args = commandArgs(trailingOnly = TRUE)) {
        if ("-h" %in% args || "--help" %in% args) {
            cat(cli_help_text(fn_name, title, description, params, param_docs, caller_env), "\n", sep = "")
            return(invisible(NULL))
        }
        # Look up the live function in the caller's environment so it runs with
        # the right enclosing scope (access to globals, sourced helpers, etc.)
        fn_live <- get(fn_name, envir = caller_env, inherits = TRUE)
        call_args <- parse_cli_args(args, params, caller_env)
        do.call(fn_live, call_args)
    }
}


# Helpers ---------------------------------------------------------------------

# Check whether a formal argument has no default (i.e. is required on the CLI)
is_missing_formal <- function(x) is.symbol(x) && identical(x, quote(expr = ))

# Try to evaluate a formal default in env; returns NULL on failure or if required
try_eval_default <- function(x, env = globalenv()) {
    if (is_missing_formal(x)) return(NULL)
    tryCatch(eval(x, envir = env), error = function(e) NULL)
}

# Return a display string for a parameter default (or NULL if required).
# Literal defaults (numbers, strings, logicals, NULL) are shown as their
# values; expressions (calls, symbols) are shown in raw deparsed form.
default_repr <- function(p) {
    if (is_missing_formal(p)) return(NULL)
    if (is.numeric(p) || is.character(p) || is.logical(p) || is.null(p)) {
        val <- eval(p)
        if (length(val) > 3L) {
            paste0("c(", paste(head(val, 3L), collapse = ", "), ", ...)")
        } else {
            paste(deparse(val), collapse = " ")
        }
    } else {
        paste(deparse(p), collapse = " ")
    }
}

# Coerce a CLI string value to the expected R type based on a default value
coerce_cli_value <- function(value, default_val) {
    if (is.null(default_val)) return(value)
    if (is.logical(default_val)) return(as.logical(value))
    if (is.integer(default_val)) return(as.integer(value))
    if (is.double(default_val)) return(as.double(value))
    # if (inherits(default_val, "Date")) return(as.Date(value))
    if (inherits(default_val, "POSIXt")) return(as.POSIXct(value))
    if (is.character(default_val) && length(default_val) > 1L) {
        return(trimws(strsplit(value, ",", fixed = TRUE)[[1L]]))
    }
    value
}

# Parse a character vector of CLI tokens against a function's formals pairlist
parse_cli_args <- function(args, params, env = globalenv()) {
    result <- list()
    i <- 1L

    while (i <= length(args)) {
        tok <- args[[i]]
        if (!startsWith(tok, "-")) stop("Unexpected positional argument: ", tok)

        eq_pos <- regexpr("=", tok, fixed = TRUE)[[1L]]

        if (eq_pos > 0L) {
            # --param=value form
            flag <- substr(tok, 3L, eq_pos - 1L)
            value <- substr(tok, eq_pos + 1L, nchar(tok))
            is_negated <- FALSE
            i <- i + 1L
        } else {
            raw_flag <- sub("^--?", "", tok)
            is_negated <- startsWith(raw_flag, "no-")
            flag <- if (is_negated) sub("^no-", "", raw_flag) else raw_flag
            r_name <- gsub("-", "_", flag, fixed = TRUE)

            if (r_name %in% names(params)) {
                dflt_val <- try_eval_default(params[[r_name]], env)
                if (is.logical(dflt_val)) {
                    result[[r_name]] <- !is_negated
                    i <- i + 1L
                    next
                }
            }

            if (is_negated) {
                stop("--no- prefix only valid for logical parameters: ", tok)
            }
            if (i >= length(args)) stop("Missing value after: ", tok)
            value <- args[[i + 1L]]
            i <- i + 2L
        }

        r_name <- gsub("-", "_", flag, fixed = TRUE)
        if (!(r_name %in% names(params))) stop("Unknown argument: --", flag)

        dflt_val <- try_eval_default(params[[r_name]], env)
        result[[r_name]] <- coerce_cli_value(value, dflt_val)
    }

    # Ensure all required (no-default) params were supplied
    for (nm in names(params)) {
        if (!(nm %in% names(result)) && is_missing_formal(params[[nm]])) {
            stop("Missing required argument: --", nm)
        }
    }

    result
}

# Build a formatted help string for a CLI-wrapped function
cli_help_text <- function(fn_name, title, description, params, param_docs, env = globalenv()) {
    col <- 28L
    hr <- strrep("\u2500", 60L)

    lines <- c(hr, cli::style_bold(title), hr, "")

    if (nzchar(description)) {
        lines <- c(lines, strwrap(description, width = 72L), "")
    }

    lines <- c(
        lines,
        cli::style_bold("Usage:"),
        paste0("  Rscript <script> [options]"),
        "",
        cli::style_bold("Options:")
    )

    for (nm in names(params)) {
        p <- params[[nm]]
        dflt_val <- try_eval_default(p, env)
        is_logical <- is.logical(dflt_val)
        is_req <- is_missing_formal(p)

        cli_nm <- gsub("_", "-", nm, fixed = TRUE)
        flag_str <- if (is_logical) {
            paste0("--[no-]", cli_nm)
        } else if (is_req) {
            paste0("--", cli_nm, " <", cli_nm, ">")
        } else {
            paste0("--", cli_nm, " <value>")
        }

        doc <- if (!is.na(param_docs[[nm]]) && nzchar(param_docs[[nm]])) param_docs[[nm]] else ""
        repr <- default_repr(p)
        dflt_str <- if (is_req) {
            cli::style_italic("(required)")
        } else {
            cli::style_italic(paste0("[default: ", repr, "]"))
        }

        desc_full <- paste(Filter(nzchar, c(doc, dflt_str)), collapse = "  ")
        lhs <- paste0("  ", flag_str)
        if (nchar(lhs) >= col) {
            lines <- c(lines, lhs, paste0(strrep(" ", col), desc_full))
        } else {
            lines <- c(lines, paste0(formatC(lhs, width = col, flag = "-"), desc_full))
        }
    }

    help_lhs <- "  -h, --help"
    lines <- c(
        lines,
        paste0(formatC(help_lhs, width = col, flag = "-"), "Show this help message"),
        ""
    )

    paste(lines, collapse = "\n")
}
