library(stringr)

remove_extras = TRUE

dir_out = Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")
path = Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")

html = readLines(file.path(dir_out, path), warn = FALSE)

# first: remove hashes
lib_paths = Sys.glob(c(
    file.path(dir_out, "reports/libs/*/*.*"),
    file.path(dir_out, "reports/libs/*/*/*.*")
))
lib_hash_rgx = fixed(paste0("-", na.omit(str_extract(lib_paths, "[0-9a-f]{32}"))))

for (lib_path in lib_paths) {
    lib_path_new = lib_path
    for (rgx in lib_hash_rgx) {
        lib_path_new = str_remove(lib_path_new, rgx)
    }
    file.rename(lib_path, lib_path_new)
}

for (rgx in lib_hash_rgx) {
    html = str_remove(html, rgx)
}

lib_line = str_detect(html, "(libs|assets)/")
html[lib_line] = str_replace(html[lib_line], fixed("libs/"), "../reports/libs/")
html[lib_line] = str_replace(html[lib_line], fixed("assets/"), "../reports/assets/")

# remove extras
if (isTRUE(remove_extras)) {
    extras = c(
        "libs/bootstrap/bootstrap-dark.min.css",
        "libs/quarto-html/quarto-syntax-highlighting-dark.css",
        "libs/quarto-html/quarto-syntax-highlighting.css",
        "libs/quarto-html/popper.min.js",
        "libs/quarto-html/tippy.umd.min.js",
        "libs/quarto-html/tippy.css",
        "libs/quarto-html/axe/axe-check.js",
        "libs/quarto-dashboard/pdfmake.min.js"
    )
    file.remove(file.path(dir_out, "reports", extras))

    html = html[rowSums(sapply(fixed(basename(extras)), \(x) str_detect(html, x))) == 0]

    path_js = file.path(dir_out, "reports/libs/quarto-html/quarto.js")
    quarto_js = readLines(path_js, warn = FALSE)
    quarto_js[str_detect(quarto_js, "axe-check\\.js")] = "const axe = {init: () => {}};"
    writeLines(quarto_js, path_js)
}

# cache hack :(
cache_inval = c(
    "reports/libs/quarto-dashboard/web-components.js",
    "reports/libs/quarto-html/quarto.js",
    "reports/libs/quarto-ojs/quarto-ojs-runtime.js",
    "reports/libs/quarto-ojs/quarto-ojs.css",
    "reports/libs/quarto-html/tabsets/tabsets.js",
    "reports/libs/bootstrap/bootstrap-icons.css"
)
cache_new = str_replace(cache_inval, "\\.(js|css)$", "-new.\\1")
file.rename(file.path(dir_out, cache_inval), file.path(dir_out, cache_new))
lib_cache = rowSums(sapply(fixed(basename(cache_inval)), \(x) str_detect(html, x))) > 0
html[lib_cache] = str_replace(html[lib_cache], "\\.(js|css)\"", "-new.\\1\"")

path_js = file.path(dir_out, "reports/libs/quarto-html/quarto-new.js")
quarto_js = readLines(path_js, warn = FALSE)
i_tab_js = str_detect(quarto_js, "tabsets\\.js")
quarto_js[i_tab_js] = str_replace(quarto_js[i_tab_js], "tabsets\\.js", "tabsets-new.js")
writeLines(quarto_js, path_js)

file.rename(
    file.path(dir_out, "reports/libs/bootstrap/bootstrap.min.css"),
    file.path(dir_out, "reports/libs/bootstrap/bootstrap-custom.min.css")
)
lib_css = str_detect(html, "bootstrap\\.min\\.css")
html[lib_css] = str_replace(html[lib_css], "bootstrap\\.min\\.css", "bootstrap-custom.min.css")

writeLines(html, file.path(dir_out, path))
