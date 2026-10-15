plot_county <- function(d, qty, mapgl = FALSE) {
    library(sf)

    d$.qty = rlang::eval_tidy(rlang::enquo(qty), d)
    lbl = rlang::as_label(rlang::enquo(qty))
    shp = if (!file.exists(path <- "2026/tx-prim-26/data/county_map.rds")) {
        shp = tigris::counties("TX", cb = TRUE, resolution="20m") |>
            dplyr::transmute(county = toupper(NAME), geometry = geometry)
        saveRDS(shp, path, compress = FALSE)
        shp
    } else {
        readRDS(path)
    }

    shp = dplyr::left_join(shp, d, by="county")
    pal = colorRampPalette(wacolors::wacolors$vantage)
    if (mapgl) {
        mapgl::maplibre_view(
            shp,
            column = ".qty",
            palette = pal,
            legend = FALSE
        )
    } else {
        plot(shp[".qty"], lwd = 0.5, pal = pal, main = lbl)
    }
}

get_s3_paths <- function(refresh = FALSE) {
    if (!file.exists(path <- "2026/tx-prim-26/data/s3_paths.txt") || isTRUE(refresh)) {
        paths = list_bucket("Precincts/20260303-TX/TX_") |>
            filter(type == "parquet") |>
            pull(path)
        writeLines(paths, path)
        paths
    } else {
        readLines(path)
    }
}

glm_minnorm <- function(form, family, data) {
    mf = model.frame(form, data)
    X = model.matrix(form, mf)
    y = model.response(mf)

    fit = glmnet::glmnet(X, y, family = family, lambda = 10)
    attr(fit, "terms") = attr(mf, "terms")
    class(fit) = c("glm_minnorm", class(fit))
    fit
}
predict.glm_minnorm <- function(
    object,
    newdata,
    type = c("link", "response", "coefficients", "nonzero", "class"),
    ...
) {
    tt = delete.response(terms(object))
    if (type == "coefficients") {
        return(glmnet::predict.glmnet(object, X, type = type, ...))
    }
    m = if (missing(newdata)) {
        stop("`newdata` is required for glm_minnorm objects")
    } else {
        model.frame(tt, newdata)
    }
    if (!is.null(classes <- attr(tt, "dataClasses"))) {
        .checkMFClasses(classes, m)
    }
    X = model.matrix(tt, m)
    preds = glmnet::predict.glmnet(object, X, type = "link", ...)
    if (type == "response") {
        preds = family(object)$linkinv(preds)
    }
    preds
}

map_until <- function(x, f) {
    fn = rlang::as_function(f)
    out = NULL
    i = 1L
    while (is.null(out) && i <= length(x)) {
        out = tryCatch(fn(x[[i]]), error = \(e) NULL)
        i = i + 1L
    }
    if (is.null(out)) {
        fn(x[[1]])
    }
    out = list(out)
    names(out) = names(x)[i - 1L]
    out
}



# fmt: skip
tgt_mit <- c("BASTROP", "BELL", "BEXAR", "CLAY", "COMAL", "DALLAS", "DENTON",
    "EL PASO", "FORT BEND", "GALVESTON", "GREGG", "HARRIS", "HIDALGO",
    "HOOD", "HUNT", "JEFFERSON", "KAUFMAN", "KENDALL", "LAMAR", "LUBBOCK",
    "MIDLAND", "MONTAGUE", "NUECES", "PARKER", "ROBERTSON", "ROCKWALL",
    "RUSK", "TARRANT", "TRAVIS", "VICTORIA", "WOOD", "WILLIAMSON")

# fmt: skip
fake_times <- structure(c(1772568000, 1772568600, 1772569200, 1772569800, 1772570400,
    1772571000, 1772571600, 1772572200, 1772572800, 1772573400, 1772574000,
    1772574600, 1772575200, 1772575800, 1772576400, 1772577000, 1772577600,
    1772578200, 1772578800, 1772579400, 1772580000, 1772580600, 1772581200,
    1772581800, 1772582400, 1772583000, 1772583600, 1772584200, 1772584800,
    1772585400, 1772586000), class = c("POSIXct", "POSIXt"), tzone = "UTC")
fake_path = "2026/tx-prim-26/data/prec_fake.parquet"

fake_data_cty <- function(now) {
    idx_obs = which.min(fake_times <= now)

    nanoparquet::read_parquet(fake_path) |>
        mutate(geo = substring(geo, 1, 6)) |>
        summarize(
            votes = sum(votes[rep_time <= idx_obs]),
            .by = c(all_of(RACE_ID), candidate, party, method, geo, county)
        ) |>
        mutate(
            time = now,
            stage = if_else(any(votes > 0), 2L, 0L),
            .by = geo
        ) |>
        coerce_elec_data() |>
        as_tibble()
}

fake_data_prec <- function(now) {
    idx_obs = which.min(fake_times <= now)

    nanoparquet::read_parquet(fake_path) |>
        filter(rep_time_mit <= length(fake_times)) |>
        collapse::group_by_vars(c(RACE_ID, "candidate", "party", "method", "geo", "county")) |>
        collapse::fsummarize(
            votes = sum(votes[rep_time_mit <= idx_obs])
        ) |>
        collapse::group_by_vars("geo") |>
        collapse::fmutate(
            time = now,
            stage = ifelse(any(votes > 0), 2L, 0L)
        ) |>
        collapse::fungroup() |>
        coerce_elec_data() |>
        as_tibble()
}
