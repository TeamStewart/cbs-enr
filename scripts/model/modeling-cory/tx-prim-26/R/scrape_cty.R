# Download -------------------------------------------------------------------

suppressPackageStartupMessages({
    library(dplyr)
})

#' Download TX election JSON from a URL, bypassing Cloudflare
#' @param url URL to a County.json endpoint
#' @returns raw vector (first byte; full content cached internally for parsing)
download_tx_json <- function(url) {
    resp <- httr2::request(url) |>
        httr2::req_headers(
            `User-Agent` = paste0(
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                "AppleWebKit/537.36 (KHTML, like Gecko) ",
                "Chrome/122.0.0.0 Safari/537.36"
            ),
            Accept = "application/json, text/plain, */*",
            `Accept-Language` = "en-US,en;q=0.9",
            `Accept-Encoding` = "gzip, deflate",
            # Referer = "https://goelect.txelections.civixapps.com/",
            # Origin = "https://goelect.txelections.civixapps.com"
        ) |>
        httr2::req_retry(max_tries = 3, backoff = ~2) |>
        httr2::req_perform()

    httr2::resp_body_raw(resp)
}


# Parse ----------------------------------------------------------------------

#' Parse TX county election JSON into tidy election data
#' @param raw_json raw vector or character string of JSON
#' @param election election date (Date or string "YYYY-MM-DD")
#' @param contests character vector of contest types to include, or NULL for all
#' @returns tibble matching enightmodels election data schema
parse_tx_county <- function(
    raw_json,
    election,
    contests = CONTESTS
) {
    election <- as.Date(election)
    timestamp <- Sys.time()

    raw <- yyjsonr::read_json_raw(raw_json)
    if (identical(names(raw), "upload")) {
        raw <- yyjsonr::read_json_raw(base64enc::base64decode(raw$upload))
    }
    known_cands = list()
    known_races = list()

    map(raw, function(county) {
        if (county$N == "LASALLE") {
            county$N = "LA SALLE"
        }
        geo <- cty_to_fips[county$N]
        if (is.na(geo)) {
            stop("Unknown county name: ", county$N)
        }

        map(county$Races, function(race) {
            parsed <- if (!is.null(known_races[[race$N]])) {
                known_races[[race$N]]
            } else {
                known_races[[race$N]] <<- parse_race_name(race$N)
            }
            if (is.null(parsed) || is.na(parsed$contest)) {
                return(NULL)
            }

            parties = vctrs::vec_recode_values(race$C$P, from=c("DEM", "REP"), to=c("dem", "rep"), default = "oth")
            if (month(election) != 11L) {
                if (all(parties == "dem")) {
                    parsed$contest = paste0(parsed$contest, "_dem")
                } else if (all(parties == "rep")) {
                    parsed$contest = paste0(parsed$contest, "_rep")
                }
            }
            if (!is.null(contests) && !(parsed$contest %in% contests)) {
                return(NULL)
            }

            cands = map_chr(race$C$N, function(nm) {
                if (!is.null(known_cands[[nm]])) {
                    known_cands[[nm]]
                } else {
                    known_cands[[nm]] <<- parse_candidate_name(nm, parsed$contest)
                }
            })
            adv = as.numeric(race$C$EV)
            eday = as.numeric(race$C$V) - adv
            nr = length(cands) * 2L
            tibble::new_tibble(list(
                    state = rep("TX", nr),
                    election = rep(election, nr),
                    jurisdiction = rep(parsed$jurisdiction, nr),
                    contest = rep(parsed$contest, nr),
                    district = rep(parsed$district, nr),
                    candidate = rep(cands, 2),
                    party = rep(parties, 2),
                    time = rep(timestamp, nr),
                    stage = rep(c(any(adv > 0)*2L, any(eday > 0)*2L), length(cands)), # by method
                    geo = rep(geo, nr),
                    method = rep(c("adv", "eday"), each = length(cands)),
                    votes = c(adv, eday)
            ))
        }) |>
            list_rbind()
    }) |>
        list_rbind() |>
        relocate(any_of(COLS_ELEC))
}


#' Combined download and parse
#' @inheritParams download_tx_json
#' @inheritParams parse_tx_county
#' @returns tibble matching enightmodels election data schema
scrape_tx_county <- function(
    url,
    election,
    contests = c("sen_dem", "sen_rep", "house_dem", "house_rep", "gov_dem", "gov_rep")
) {
    raw <- download_tx_json(url)
    parse_tx_county(raw, election, contests)
}


# Helpers -------------------------------------------------

parse_race_name <- function(on) {
    on <- trimws(on)

    # Skip congressional-district-level presidential races (primary artifacts)
    if (str_detect(on, "^PRESIDENT/VICE-PRESIDENT, CONGRESSIONAL")) {
        return(NULL)
    }

    if (str_detect(on, "^PRESIDENT/VICE-PRESIDENT")) {
        list(contest = "pres", jurisdiction = "US", district = NA_integer_)
    } else if (str_detect(on, "^U\\.?\\s*S\\.?\\s+SENATOR")) {
        list(contest = "sen", jurisdiction = "TX", district = NA_integer_)
    } else if (str_detect(on, "^U\\.?\\s*S\\.?\\s+REPRESENTATIVE DISTRICT\\s+(\\d+)")) {
        d <- as.integer(str_match(on, "DISTRICT\\s+(\\d+)")[, 2])
        list(contest = "house", jurisdiction = "TX", district = d)
    } else if (str_detect(on, "^GOVERNOR")) {
        list(contest = "gov", jurisdiction = "TX", district = NA_integer_)
    } else if (str_detect(on, "^STATE SENATOR,?\\s+DISTRICT\\s+(\\d+)")) {
        d <- as.integer(str_match(on, "DISTRICT\\s+(\\d+)")[, 2])
        list(contest = "stsen", jurisdiction = "TX", district = d)
    } else if (str_detect(on, "^STATE REPRESENTATIVE\\s+DISTRICT\\s+(\\d+)")) {
        d <- as.integer(str_match(on, "DISTRICT\\s+(\\d+)")[, 2])
        list(contest = "strep", jurisdiction = "TX", district = d)
    } else if (str_detect(on, "^JUSTICE,\\s+SUPREME COURT,\\s+PLACE\\s+(\\d+)")) {
        d <- as.integer(str_match(on, "PLACE\\s+(\\d+)")[, 2])
        list(contest = "scourt", jurisdiction = "TX", district = d)
    } else {
        list(contest = NA_character_)
    }
}


parse_candidate_name <- function(name, contest) {
    # Strip running mate for president
    if (contest == "president") {
        name <- str_extract(name, "^[^/]+") |> str_trim()
    }
    # strip incumbency
    name = str_remove(name, " \\(I\\)$")
    # strip nickname
    name = str_remove(name, ' ".+"')

    name <- str_to_title(name) |>
        str_remove("\\.")

    # Suffixes
    suffixes <- c("Jr?", "Sr?", "II", "III", "IV")
    suffix_pattern <- paste0("\\b(", paste(suffixes, collapse = "|"), ")$")
    suffix <- str_extract(name, regex(suffix_pattern, ignore_case = TRUE))
    if (!is.na(suffix)) {
        name <- str_remove(name, regex(paste0(",?\\s*", suffix, "$"), ignore_case = TRUE)) |>
            str_trim()
        suffix <- str_to_upper(suffix) |>
            str_replace("^II$", "II") |>
            str_replace("^III$", "III") |>
            str_replace("^IV$", "IV") |>
            str_replace("^JR?$", "Jr") |>
            str_replace("^SR?$", "Sr")
    }

    parts <- str_split(name, "\\s+")[[1]]
    if (length(parts) == 0) {
        return(NA_character_)
    }

    last <- parts[length(parts)]
    first_middle <- parts[-length(parts)]

    if (length(first_middle) == 0) {
        result <- last
    } else {
        result <- paste0(last, ", ", paste(first_middle, collapse = " "))
    }

    if (!is.na(suffix)) {
        result <- paste0(result, ", ", suffix)
    }

    result
}

cty_to_fips = c(ANDERSON = "48001", ANDREWS = "48003", ANGELINA = "48005",
    ARANSAS = "48007", ARCHER = "48009", ARMSTRONG = "48011", ATASCOSA = "48013",
    AUSTIN = "48015", BAILEY = "48017", BANDERA = "48019", BASTROP = "48021",
    BAYLOR = "48023", BEE = "48025", BELL = "48027", BEXAR = "48029",
    BLANCO = "48031", BORDEN = "48033", BOSQUE = "48035", BOWIE = "48037",
    BRAZORIA = "48039", BRAZOS = "48041", BREWSTER = "48043", BRISCOE = "48045",
    BROOKS = "48047", BROWN = "48049", BURLESON = "48051", BURNET = "48053",
    CALDWELL = "48055", CALHOUN = "48057", CALLAHAN = "48059", CAMERON = "48061",
    CAMP = "48063", CARSON = "48065", CASS = "48067", CASTRO = "48069",
    CHAMBERS = "48071", CHEROKEE = "48073", CHILDRESS = "48075",
    CLAY = "48077", COCHRAN = "48079", COKE = "48081", COLEMAN = "48083",
    COLLIN = "48085", COLLINGSWORTH = "48087", COLORADO = "48089",
    COMAL = "48091", COMANCHE = "48093", CONCHO = "48095", COOKE = "48097",
    CORYELL = "48099", COTTLE = "48101", CRANE = "48103", CROCKETT = "48105",
    CROSBY = "48107", CULBERSON = "48109", DALLAM = "48111", DALLAS = "48113",
    DAWSON = "48115", `DEAF SMITH` = "48117", DELTA = "48119", DENTON = "48121",
    DEWITT = "48123", DICKENS = "48125", DIMMIT = "48127", DONLEY = "48129",
    DUVAL = "48131", EASTLAND = "48133", ECTOR = "48135", EDWARDS = "48137",
    ELLIS = "48139", `EL PASO` = "48141", ERATH = "48143", FALLS = "48145",
    FANNIN = "48147", FAYETTE = "48149", FISHER = "48151", FLOYD = "48153",
    FOARD = "48155", `FORT BEND` = "48157", FRANKLIN = "48159", FREESTONE = "48161",
    FRIO = "48163", GAINES = "48165", GALVESTON = "48167", GARZA = "48169",
    GILLESPIE = "48171", GLASSCOCK = "48173", GOLIAD = "48175", GONZALES = "48177",
    GRAY = "48179", GRAYSON = "48181", GREGG = "48183", GRIMES = "48185",
    GUADALUPE = "48187", HALE = "48189", HALL = "48191", HAMILTON = "48193",
    HANSFORD = "48195", HARDEMAN = "48197", HARDIN = "48199", HARRIS = "48201",
    HARRISON = "48203", HARTLEY = "48205", HASKELL = "48207", HAYS = "48209",
    HEMPHILL = "48211", HENDERSON = "48213", HIDALGO = "48215", HILL = "48217",
    HOCKLEY = "48219", HOOD = "48221", HOPKINS = "48223", HOUSTON = "48225",
    HOWARD = "48227", HUDSPETH = "48229", HUNT = "48231", HUTCHINSON = "48233",
    IRION = "48235", JACK = "48237", JACKSON = "48239", JASPER = "48241",
    `JEFF DAVIS` = "48243", JEFFERSON = "48245", `JIM HOGG` = "48247",
    `JIM WELLS` = "48249", JOHNSON = "48251", JONES = "48253", KARNES = "48255",
    KAUFMAN = "48257", KENDALL = "48259", KENEDY = "48261", KENT = "48263",
    KERR = "48265", KIMBLE = "48267", KING = "48269", KINNEY = "48271",
    KLEBERG = "48273", KNOX = "48275", LAMAR = "48277", LAMB = "48279",
    LAMPASAS = "48281", `LA SALLE` = "48283", LAVACA = "48285", LEE = "48287",
    LEON = "48289", LIBERTY = "48291", LIMESTONE = "48293", LIPSCOMB = "48295",
    `LIVE OAK` = "48297", LLANO = "48299", LOVING = "48301", LUBBOCK = "48303",
    LYNN = "48305", MCCULLOCH = "48307", MCLENNAN = "48309", MCMULLEN = "48311",
    MADISON = "48313", MARION = "48315", MARTIN = "48317", MASON = "48319",
    MATAGORDA = "48321", MAVERICK = "48323", MEDINA = "48325", MENARD = "48327",
    MIDLAND = "48329", MILAM = "48331", MILLS = "48333", MITCHELL = "48335",
    MONTAGUE = "48337", MONTGOMERY = "48339", MOORE = "48341", MORRIS = "48343",
    MOTLEY = "48345", NACOGDOCHES = "48347", NAVARRO = "48349", NEWTON = "48351",
    NOLAN = "48353", NUECES = "48355", OCHILTREE = "48357", OLDHAM = "48359",
    ORANGE = "48361", `PALO PINTO` = "48363", PANOLA = "48365", PARKER = "48367",
    PARMER = "48369", PECOS = "48371", POLK = "48373", POTTER = "48375",
    PRESIDIO = "48377", RAINS = "48379", RANDALL = "48381", REAGAN = "48383",
    REAL = "48385", `RED RIVER` = "48387", REEVES = "48389", REFUGIO = "48391",
    ROBERTS = "48393", ROBERTSON = "48395", ROCKWALL = "48397", RUNNELS = "48399",
    RUSK = "48401", SABINE = "48403", `SAN AUGUSTINE` = "48405",
    `SAN JACINTO` = "48407", `SAN PATRICIO` = "48409", `SAN SABA` = "48411",
    SCHLEICHER = "48413", SCURRY = "48415", SHACKELFORD = "48417",
    SHELBY = "48419", SHERMAN = "48421", SMITH = "48423", SOMERVELL = "48425",
    STARR = "48427", STEPHENS = "48429", STERLING = "48431", STONEWALL = "48433",
    SUTTON = "48435", SWISHER = "48437", TARRANT = "48439", TAYLOR = "48441",
    TERRELL = "48443", TERRY = "48445", THROCKMORTON = "48447", TITUS = "48449",
    `TOM GREEN` = "48451", TRAVIS = "48453", TRINITY = "48455", TYLER = "48457",
    UPSHUR = "48459", UPTON = "48461", UVALDE = "48463", `VAL VERDE` = "48465",
    `VAN ZANDT` = "48467", VICTORIA = "48469", WALKER = "48471",
    WALLER = "48473", WARD = "48475", WASHINGTON = "48477", WEBB = "48479",
    WHARTON = "48481", WHEELER = "48483", WICHITA = "48485", WILBARGER = "48487",
    WILLACY = "48489", WILLIAMSON = "48491", WILSON = "48493", WINKLER = "48495",
    WISE = "48497", WOOD = "48499", YOAKUM = "48501", YOUNG = "48503",
    ZAPATA = "48505", ZAVALA = "48507")


fips_to_cty = c(`001` = "ANDERSON", `003` = "ANDREWS", `005` = "ANGELINA",
    `007` = "ARANSAS", `009` = "ARCHER", `011` = "ARMSTRONG", `013` = "ATASCOSA",
    `015` = "AUSTIN", `017` = "BAILEY", `019` = "BANDERA", `021` = "BASTROP",
    `023` = "BAYLOR", `025` = "BEE", `027` = "BELL", `029` = "BEXAR",
    `031` = "BLANCO", `033` = "BORDEN", `035` = "BOSQUE", `037` = "BOWIE",
    `039` = "BRAZORIA", `041` = "BRAZOS", `043` = "BREWSTER", `045` = "BRISCOE",
    `047` = "BROOKS", `049` = "BROWN", `051` = "BURLESON", `053` = "BURNET",
    `055` = "CALDWELL", `057` = "CALHOUN", `059` = "CALLAHAN", `061` = "CAMERON",
    `063` = "CAMP", `065` = "CARSON", `067` = "CASS", `069` = "CASTRO",
    `071` = "CHAMBERS", `073` = "CHEROKEE", `075` = "CHILDRESS",
    `077` = "CLAY", `079` = "COCHRAN", `081` = "COKE", `083` = "COLEMAN",
    `085` = "COLLIN", `087` = "COLLINGSWORTH", `089` = "COLORADO",
    `091` = "COMAL", `093` = "COMANCHE", `095` = "CONCHO", `097` = "COOKE",
    `099` = "CORYELL", `101` = "COTTLE", `103` = "CRANE", `105` = "CROCKETT",
    `107` = "CROSBY", `109` = "CULBERSON", `111` = "DALLAM", `113` = "DALLAS",
    `115` = "DAWSON", `117` = "DEAF SMITH", `119` = "DELTA", `121` = "DENTON",
    `123` = "DEWITT", `125` = "DICKENS", `127` = "DIMMIT", `129` = "DONLEY",
    `131` = "DUVAL", `133` = "EASTLAND", `135` = "ECTOR", `137` = "EDWARDS",
    `139` = "ELLIS", `141` = "EL PASO", `143` = "ERATH", `145` = "FALLS",
    `147` = "FANNIN", `149` = "FAYETTE", `151` = "FISHER", `153` = "FLOYD",
    `155` = "FOARD", `157` = "FORT BEND", `159` = "FRANKLIN", `161` = "FREESTONE",
    `163` = "FRIO", `165` = "GAINES", `167` = "GALVESTON", `169` = "GARZA",
    `171` = "GILLESPIE", `173` = "GLASSCOCK", `175` = "GOLIAD", `177` = "GONZALES",
    `179` = "GRAY", `181` = "GRAYSON", `183` = "GREGG", `185` = "GRIMES",
    `187` = "GUADALUPE", `189` = "HALE", `191` = "HALL", `193` = "HAMILTON",
    `195` = "HANSFORD", `197` = "HARDEMAN", `199` = "HARDIN", `201` = "HARRIS",
    `203` = "HARRISON", `205` = "HARTLEY", `207` = "HASKELL", `209` = "HAYS",
    `211` = "HEMPHILL", `213` = "HENDERSON", `215` = "HIDALGO", `217` = "HILL",
    `219` = "HOCKLEY", `221` = "HOOD", `223` = "HOPKINS", `225` = "HOUSTON",
    `227` = "HOWARD", `229` = "HUDSPETH", `231` = "HUNT", `233` = "HUTCHINSON",
    `235` = "IRION", `237` = "JACK", `239` = "JACKSON", `241` = "JASPER",
    `243` = "JEFF DAVIS", `245` = "JEFFERSON", `247` = "JIM HOGG",
    `249` = "JIM WELLS", `251` = "JOHNSON", `253` = "JONES", `255` = "KARNES",
    `257` = "KAUFMAN", `259` = "KENDALL", `261` = "KENEDY", `263` = "KENT",
    `265` = "KERR", `267` = "KIMBLE", `269` = "KING", `271` = "KINNEY",
    `273` = "KLEBERG", `275` = "KNOX", `277` = "LAMAR", `279` = "LAMB",
    `281` = "LAMPASAS", `283` = "LA SALLE", `285` = "LAVACA", `287` = "LEE",
    `289` = "LEON", `291` = "LIBERTY", `293` = "LIMESTONE", `295` = "LIPSCOMB",
    `297` = "LIVE OAK", `299` = "LLANO", `301` = "LOVING", `303` = "LUBBOCK",
    `305` = "LYNN", `307` = "MCCULLOCH", `309` = "MCLENNAN", `311` = "MCMULLEN",
    `313` = "MADISON", `315` = "MARION", `317` = "MARTIN", `319` = "MASON",
    `321` = "MATAGORDA", `323` = "MAVERICK", `325` = "MEDINA", `327` = "MENARD",
    `329` = "MIDLAND", `331` = "MILAM", `333` = "MILLS", `335` = "MITCHELL",
    `337` = "MONTAGUE", `339` = "MONTGOMERY", `341` = "MOORE", `343` = "MORRIS",
    `345` = "MOTLEY", `347` = "NACOGDOCHES", `349` = "NAVARRO", `351` = "NEWTON",
    `353` = "NOLAN", `355` = "NUECES", `357` = "OCHILTREE", `359` = "OLDHAM",
    `361` = "ORANGE", `363` = "PALO PINTO", `365` = "PANOLA", `367` = "PARKER",
    `369` = "PARMER", `371` = "PECOS", `373` = "POLK", `375` = "POTTER",
    `377` = "PRESIDIO", `379` = "RAINS", `381` = "RANDALL", `383` = "REAGAN",
    `385` = "REAL", `387` = "RED RIVER", `389` = "REEVES", `391` = "REFUGIO",
    `393` = "ROBERTS", `395` = "ROBERTSON", `397` = "ROCKWALL", `399` = "RUNNELS",
    `401` = "RUSK", `403` = "SABINE", `405` = "SAN AUGUSTINE", `407` = "SAN JACINTO",
    `409` = "SAN PATRICIO", `411` = "SAN SABA", `413` = "SCHLEICHER",
    `415` = "SCURRY", `417` = "SHACKELFORD", `419` = "SHELBY", `421` = "SHERMAN",
    `423` = "SMITH", `425` = "SOMERVELL", `427` = "STARR", `429` = "STEPHENS",
    `431` = "STERLING", `433` = "STONEWALL", `435` = "SUTTON", `437` = "SWISHER",
    `439` = "TARRANT", `441` = "TAYLOR", `443` = "TERRELL", `445` = "TERRY",
    `447` = "THROCKMORTON", `449` = "TITUS", `451` = "TOM GREEN",
    `453` = "TRAVIS", `455` = "TRINITY", `457` = "TYLER", `459` = "UPSHUR",
    `461` = "UPTON", `463` = "UVALDE", `465` = "VAL VERDE", `467` = "VAN ZANDT",
    `469` = "VICTORIA", `471` = "WALKER", `473` = "WALLER", `475` = "WARD",
    `477` = "WASHINGTON", `479` = "WEBB", `481` = "WHARTON", `483` = "WHEELER",
    `485` = "WICHITA", `487` = "WILBARGER", `489` = "WILLACY", `491` = "WILLIAMSON",
    `493` = "WILSON", `495` = "WINKLER", `497` = "WISE", `499` = "WOOD",
    `501` = "YOAKUM", `503` = "YOUNG", `505` = "ZAPATA", `507` = "ZAVALA"
    )

cbs_to_cty = c(`001` = "ANDERSON", `002` = "ANDREWS", `003` = "ANGELINA",
    `004` = "ARANSAS", `005` = "ARCHER", `006` = "ARMSTRONG", `007` = "ATASCOSA",
    `008` = "AUSTIN", `009` = "BAILEY", `010` = "BANDERA", `011` = "BASTROP",
    `012` = "BAYLOR", `013` = "BEE", `014` = "BELL", `015` = "BEXAR",
    `016` = "BLANCO", `017` = "BORDEN", `018` = "BOSQUE", `019` = "BOWIE",
    `020` = "BRAZORIA", `021` = "BRAZOS", `022` = "BREWSTER", `023` = "BRISCOE",
    `024` = "BROOKS", `025` = "BROWN", `026` = "BURLESON", `027` = "BURNET",
    `028` = "CALDWELL", `029` = "CALHOUN", `030` = "CALLAHAN", `031` = "CAMERON",
    `032` = "CAMP", `033` = "CARSON", `034` = "CASS", `035` = "CASTRO",
    `036` = "CHAMBERS", `037` = "CHEROKEE", `038` = "CHILDRESS",
    `039` = "CLAY", `040` = "COCHRAN", `041` = "COKE", `042` = "COLEMAN",
    `043` = "COLLIN", `044` = "COLLINGSWORTH", `045` = "COLORADO",
    `046` = "COMAL", `047` = "COMANCHE", `048` = "CONCHO", `049` = "COOKE",
    `050` = "CORYELL", `051` = "COTTLE", `052` = "CRANE", `053` = "CROCKETT",
    `054` = "CROSBY", `055` = "CULBERSON", `056` = "DALLAM", `057` = "DALLAS",
    `058` = "DAWSON", `059` = "DEAF SMITH", `060` = "DELTA", `061` = "DENTON",
    `062` = "DEWITT", `063` = "DICKENS", `064` = "DIMMIT", `065` = "DONLEY",
    `066` = "DUVAL", `067` = "EASTLAND", `068` = "ECTOR", `069` = "EDWARDS",
    `070` = "ELLIS", `071` = "EL PASO", `072` = "ERATH", `073` = "FALLS",
    `074` = "FANNIN", `075` = "FAYETTE", `076` = "FISHER", `077` = "FLOYD",
    `078` = "FOARD", `079` = "FORT BEND", `080` = "FRANKLIN", `081` = "FREESTONE",
    `082` = "FRIO", `083` = "GAINES", `084` = "GALVESTON", `085` = "GARZA",
    `086` = "GILLESPIE", `087` = "GLASSCOCK", `088` = "GOLIAD", `089` = "GONZALES",
    `090` = "GRAY", `091` = "GRAYSON", `092` = "GREGG", `093` = "GRIMES",
    `094` = "GUADALUPE", `095` = "HALE", `096` = "HALL", `097` = "HAMILTON",
    `098` = "HANSFORD", `099` = "HARDEMAN", `100` = "HARDIN", `101` = "HARRIS",
    `102` = "HARRISON", `103` = "HARTLEY", `104` = "HASKELL", `105` = "HAYS",
    `106` = "HEMPHILL", `107` = "HENDERSON", `108` = "HIDALGO", `109` = "HILL",
    `110` = "HOCKLEY", `111` = "HOOD", `112` = "HOPKINS", `113` = "HOUSTON",
    `114` = "HOWARD", `115` = "HUDSPETH", `116` = "HUNT", `117` = "HUTCHINSON",
    `118` = "IRION", `119` = "JACK", `120` = "JACKSON", `121` = "JASPER",
    `122` = "JEFF DAVIS", `123` = "JEFFERSON", `124` = "JIM HOGG",
    `125` = "JIM WELLS", `126` = "JOHNSON", `127` = "JONES", `128` = "KARNES",
    `129` = "KAUFMAN", `130` = "KENDALL", `131` = "KENEDY", `132` = "KENT",
    `133` = "KERR", `134` = "KIMBLE", `135` = "KING", `136` = "KINNEY",
    `137` = "KLEBERG", `138` = "KNOX", `139` = "LAMAR", `140` = "LAMB",
    `141` = "LAMPASAS", `142` = "LA SALLE", `143` = "LAVACA", `144` = "LEE",
    `145` = "LEON", `146` = "LIBERTY", `147` = "LIMESTONE", `148` = "LIPSCOMB",
    `149` = "LIVE OAK", `150` = "LLANO", `151` = "LOVING", `152` = "LUBBOCK",
    `153` = "LYNN", `154` = "MCCULLOCH", `155` = "MCLENNAN", `156` = "MCMULLEN",
    `157` = "MADISON", `158` = "MARION", `159` = "MARTIN", `160` = "MASON",
    `161` = "MATAGORDA", `162` = "MAVERICK", `163` = "MEDINA", `164` = "MENARD",
    `165` = "MIDLAND", `166` = "MILAM", `167` = "MILLS", `168` = "MITCHELL",
    `169` = "MONTAGUE", `170` = "MONTGOMERY", `171` = "MOORE", `172` = "MORRIS",
    `173` = "MOTLEY", `174` = "NACOGDOCHES", `175` = "NAVARRO", `176` = "NEWTON",
    `177` = "NOLAN", `178` = "NUECES", `179` = "OCHILTREE", `180` = "OLDHAM",
    `181` = "ORANGE", `182` = "PALO PINTO", `183` = "PANOLA", `184` = "PARKER",
    `185` = "PARMER", `186` = "PECOS", `187` = "POLK", `188` = "POTTER",
    `189` = "PRESIDIO", `190` = "RAINS", `191` = "RANDALL", `192` = "REAGAN",
    `193` = "REAL", `194` = "RED RIVER", `195` = "REEVES", `196` = "REFUGIO",
    `197` = "ROBERTS", `198` = "ROBERTSON", `199` = "ROCKWALL", `200` = "RUNNELS",
    `201` = "RUSK", `202` = "SABINE", `203` = "SAN AUGUSTINE", `204` = "SAN JACINTO",
    `205` = "SAN PATRICIO", `206` = "SAN SABA", `207` = "SCHLEICHER",
    `208` = "SCURRY", `209` = "SHACKELFORD", `210` = "SHELBY", `211` = "SHERMAN",
    `212` = "SMITH", `213` = "SOMERVELL", `214` = "STARR", `215` = "STEPHENS",
    `216` = "STERLING", `217` = "STONEWALL", `218` = "SUTTON", `219` = "SWISHER",
    `220` = "TARRANT", `221` = "TAYLOR", `222` = "TERRELL", `223` = "TERRY",
    `224` = "THROCKMORTON", `225` = "TITUS", `226` = "TOM GREEN",
    `227` = "TRAVIS", `228` = "TRINITY", `229` = "TYLER", `230` = "UPSHUR",
    `231` = "UPTON", `232` = "UVALDE", `233` = "VAL VERDE", `234` = "VAN ZANDT",
    `235` = "VICTORIA", `236` = "WALKER", `237` = "WALLER", `238` = "WARD",
    `239` = "WASHINGTON", `240` = "WEBB", `241` = "WHARTON", `242` = "WHEELER",
    `243` = "WICHITA", `244` = "WILBARGER", `245` = "WILLACY", `246` = "WILLIAMSON",
    `247` = "WILSON", `248` = "WINKLER", `249` = "WISE", `250` = "WOOD",
    `251` = "YOAKUM", `252` = "YOUNG", `253` = "ZAPATA", `254` = "ZAVALA"
    )
