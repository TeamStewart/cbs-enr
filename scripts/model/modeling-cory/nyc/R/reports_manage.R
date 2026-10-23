here::i_am("R/reports_manage.R")
suppressPackageStartupMessages({
    library(here)
    library(cli)
    devtools::load_all("../enightmodels", quiet=TRUE)
})

# load static files
if (FALSE) {
    base = here("reports/")
    paths = Sys.glob(c(
        here("reports/assets/cbs_white.svg"),
        here("reports/utils.js"),
        here("reports/libs/*/*.*"),
        here("reports/libs/*/*.*.*"),
        here("reports/libs/*/*/*.*")
    ))
    # paths = paths[basename(paths) %in% c("tabsets.js", "quarto-ojs-runtime.js", "web-components.js", "quarto.js")]
    # paths = paths[str_detect(paths, "-new")]
    for (path in paths) {
        path_s3 = paste0("reports/", str_sub(path, nchar(base) + 1L))
        res = write_file_bucket(path, path_s3)
        if (!isTRUE(res)) cli_alert_warning("Failed to upload {path_s3}.")
    }

    path_geo = here("reports/data-out/nyc_precs.geojson")
    path_geo_mbbqs = here("reports/data-out/nyc_mbbqs.geojson")
    to_s3 = \(x) paste0("20251104-NY-M/data-out/", basename(x))
    write_file_bucket(path_geo, to_s3(path_geo))
    write_file_bucket(path_geo_mbbqs, to_s3(path_geo_mbbqs))
}

write_file_bucket(here("reports/nyc.html"), "20251104-NY-M/nyc.html")
write_file_bucket(
    here("reports/libs/bootstrap/bootstrap-custom.min.css"),
    "reports/libs/bootstrap/bootstrap-custom.min.css",
    headers = list(`Cache-Control` = "no-cache, max-age=0")
)

write_file_bucket("2025/nyc/reports/utils_nyc.js", "20251104-NY-M/utils_nyc.js")
write_file_bucket("2025/nyc/reports/nyc.html", "20251104-NY-M/nyc.html")
# remove_file_bucket("reports/libs/bootstrap/bootstrap.min.css")