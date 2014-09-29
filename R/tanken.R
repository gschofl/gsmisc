tanken.sql <- '
CREATE TABLE tanken(
        datum           CHAR(10),
tankstelle      VARCHAR(50),
adresse         VARCHAR(120),
kraftstoff      CHAR(3),
menge           NUMBER,
betrag          NUMBER,
kilometerstand  NUMBER
);
'

is.valid_date <- function(x) {
  grepl("\\d{2}\\.\\d{2}\\.\\d{4}", x)  
}
on_failure(is.valid_date) <- function(call, env) {
  paste0("<", deparse(call$x), "> hat kein gültiges Format. Verwende <dd.mm.yyyy>")
}

is.valid_address <- function(x) {
  grepl(".+\\d+\\w?, \\w+-\\d+ \\w+", x) 
}
on_failure(is.valid_address) <- function(call, env) {
  paste0("<", deparse(call$x), "> hat kein gültiges Format. \n
         Ein gültiges Format ist <Stadtrodaer Straße 11, D-07749 Jena>")
}

#' Eintragen und Abrufen von Daten in die tanken.db
#' 
#' @param datum Wann wurde getankt <dd.mm.yyyy>.
#' @param tankstelle Name der Tankstelle. (Aral, OMV, star, usw.)
#' @param adresse Adresse der Tankstelle. (zB. Stadtrodaer Straße 11, D-07749 Jena)
#' @param kraftstoff Eins von "E10", "E05", oder "SUP".
#' @param menge Getankte Kraftstoffmenge (in Liter).
#' @param betrag Gesamtbetrag in Euro.
#' @param kilometerstand Aktueller Gesamtkilometerstand.
#' @param initialisiern Falls \code{TRUE} wird die Datenbank initialisiert.
#' @return Trägt übergebene Daten in die tanken.db ein.
#' @export
#' @examples
#' \dontrun{
#' tanken_eingabe(initialisiern = TRUE)
#' tanken_eingabe(datum = "19.09.2014", tankstelle = "Aral",
#'                adresse = "Stadtrodaer Straße 11, 07749 Jena",
#'                kraftstoff = "E10", menge = 51.39, betrag = 76.01,
#'                kilometerstand = 433.8)
#' tanken_abruf()
#' }
tanken_eingabe <- function(datum, tankstelle,  adresse, kraftstoff = c("E10", "E05", "SUP"),
                           menge, betrag, kilometerstand, initialisiern = FALSE) {
  dbname <- getOption("gsmisc.tanken")
  if (is.null(dbname)) {
    stop("Setze den Pfad zur Datenbank mittels der globalen Option <gsmisc.tanken>.")
  }
  if (initialisiern) {
    db_create(dbName = dbname, dbSchema = tanken.sql, overwrite = FALSE)
    return(NULL)
  }
  assert_that(file.exists(dbname))
  assert_that(is.valid_date(datum))
  assert_that(is.valid_address(adresse))
  kraftstoff <- match.arg(kraftstoff, c("E10", "E05", "SUP"))
  datum <- as.character(as.POSIXct(datum, format = "%d.%m.%Y"))
  df <- data.frame(datum = datum, tankstelle = tankstelle, adresse = adresse,
                   kraftstoff = kraftstoff, menge = menge, betrag = betrag,
                   kilometerstand = kilometerstand, stringsAsFactors = FALSE)
  con <- db_connect(dbname)
  stmt <- paste0("SELECT * FROM tanken WHERE datum = '", datum,
                 "' AND kilometerstand = ", kilometerstand)
  if (nrow(db_query(con, stmt)) > 0) {
    stop("Eintrag <", paste0(df, collapse = ", "), "> existiert bereits in der Datenbank")
  }
  if (db_bulk_insert(con, "tanken", df)) {
    message("Daten erfolgreich eingetragen:\n <", paste0(db_query(con, stmt), collapse = ", "), ">")
  }
  invisible(NULL)
}

#' @rdname tanken_eingabe
#' @export
tanken_abruf <- function() {
  stopifnot(require("data.table", character.only = TRUE))
  stopifnot(require("lubridate", character.only = TRUE))
  dbname <- getOption("gsmisc.tanken")
  if (is.null(dbname)) {
    stop("Setze den Pfad zur Datenbank mittels der globalen Option <gsmisc.tanken>.")
  }
  assert_that(file.exists(dbname))
  con <- db_connect(dbname)
  rs <- data.table(db_query(con, "SELECT * FROM tanken"))
  rs$datum <- ymd(rs$datum)
  rs <- rs[order(rs$datum, decreasing = TRUE), ]
  rs
}



