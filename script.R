# =============================================================================
# PubMedSearchR — basic R script (no Shiny)
# Author: Eelke de Vries — https://github.com/eelkedevries/PubMedSearchR
#
# I wrote this script to make a reproducible PubMed search. It builds a query,
# fetches best-match results, and enriches with OpenAlex (citations & OA).
# Output: CSV and a standalone HTML with an interactive DataTable.
#
# How to write a PubMed query (short):
# - Use AND / OR / NOT for boolean logic.
# - Put multi-word phrases in double quotes, e.g., "working memory".
# - You can also use MeSH terms (Medical Subject Headings).
#   More info: https://www.ncbi.nlm.nih.gov/mesh/
#
# Default example query is below; edit the user inputs if you want.
# =============================================================================

# --------------------- user inputs (edit here) -------------------------------

search_terms     <- 'saccades AND "working memory"'  # boolean allowed, phrases in ""
start_year       <- NA_integer_  # e.g., 2000; NA = no lower bound
end_year         <- NA_integer_  # e.g., 2024; NA = no upper bound
only_full_text   <- FALSE        # TRUE = require "full text"[sb]
results_requested<- 100L         # number of articles to retrieve (best match order)
citation_format  <- "APA"        # APA, MLA, Chicago, Vancouver, Harvard, IEEE, AMA, ACS, CSE, Turabian
ncbi_api_key     <- ""           # optional; speeds up (10 req/s vs ~3 req/s)

# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(rentrez)
  library(xml2)
  library(jsonlite)
  library(htmltools)
})

options(stringsAsFactors = FALSE)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
esc <- function(x) { if (is.null(x)) return(""); x <- as.character(x); x[is.na(x)] <- ""; htmlEscape(x) }

# ---- resolve script directory (save files here)
script_dir <- (function(){
  # Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)))
  # RStudio (active document)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
  }
  # source() fallback
  p <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
  if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
  # last resort
  getwd()
})()

# ---- small helpers
safe_filename <- function(x) {
  x <- gsub("[^[:alnum:]_ -]", "_", x)  # avoid invalid chars (works with TRE)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  trimws(x, which = "both")
}

normalize_years <- function(sy, ey) {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  if (is.na(sy) && is.na(ey)) return(list(sy = NA_integer_, ey = NA_integer_))
  if (is.na(sy)) sy <- 1800L
  if (is.na(ey)) ey <- cy
  if (ey < sy) { tmp <- sy; sy <- ey; ey <- tmp }
  list(sy = as.integer(sy), ey = as.integer(ey))
}

build_query <- function(terms, yb, only_ft) {
  if (!nzchar(terms)) stop("search_terms is empty.")
  q <- sprintf("(%s)", terms)
  if (!is.na(yb$sy)) q <- paste(q, sprintf("%d:%d[PDAT]", yb$sy, yb$ey), sep = " AND ")
  if (isTRUE(only_ft)) q <- paste(q, '"full text"[sb]', sep = " AND ")
  q
}

safe_first <- function(node, xpath) { x <- xml_find_first(node, xpath); if (length(x) == 0) return(NA_character_); xml_text(x) }
collapse_all <- function(node, xpath, sep = " ") { xs <- xml_find_all(node, xpath); if (length(xs) == 0) return(NA_character_); paste(xml_text(xs), collapse = sep) }

extract_pubyear <- function(article) {
  y1 <- safe_first(article, ".//Article/ArticleDate/Year")
  y2 <- safe_first(article, ".//Journal/JournalIssue/PubDate/Year")
  y3 <- safe_first(article, ".//Journal/JournalIssue/PubDate/MedlineDate")
  y  <- if (!is.na(y1)) y1 else if (!is.na(y2)) y2 else y3
  if (!is.na(y) && grepl("\\d{4}", y)) sub(".*?(\\d{4}).*", "\\1", y) else NA_character_
}

extract_authors <- function(article) {
  auth_nodes <- xml_find_all(article, ".//AuthorList/Author")
  if (!length(auth_nodes)) return(NA_character_)
  fmt <- function(a) {
    last <- safe_first(a, ".//LastName")
    ini  <- safe_first(a, ".//Initials")
    fore <- safe_first(a, ".//ForeName")
    if (!is.na(last) && !is.na(ini)) paste0(last, " ", ini)
    else if (!is.na(last) && !is.na(fore)) paste0(last, ", ", fore)
    else if (!is.na(last)) last else NA_character_
  }
  paste(na.omit(vapply(auth_nodes, fmt, character(1))), collapse = "; ")
}

extract_keywords <- function(article) {
  kws <- xml_find_all(article, ".//KeywordList/Keyword")
  if (!length(kws)) return(NA_character_)
  paste(xml_text(kws), collapse = "; ")
}

parse_article <- function(article) {
  data.frame(
    PMID       = safe_first(article, ".//PMID"),
    Title      = safe_first(article, ".//Article/ArticleTitle"),
    Authors    = extract_authors(article),
    Journal    = safe_first(article, ".//Journal/Title"),
    PubYear    = extract_pubyear(article),
    Abstract   = collapse_all(article, ".//Abstract/AbstractText", sep = " "),
    DOI        = safe_first(article, ".//ArticleId[@IdType='doi']"),
    AuthorKeywords = extract_keywords(article),
    stringsAsFactors = FALSE
  )
}

fetch_parse_chunk <- function(id_chunk, pause_sec = 0.34) {
  raw <- entrez_fetch(db = "pubmed", id = id_chunk, rettype = "xml", parsed = FALSE)
  doc <- read_xml(raw)
  arts <- xml_find_all(doc, ".//PubmedArticle")
  res  <- lapply(arts, parse_article)
  if (pause_sec > 0) Sys.sleep(pause_sec)
  do.call(rbind, res)
}

# ---- authors & citations formatting (simple but OK for quick export)
authors_to_list <- function(auth_str) {
  if (is.na(auth_str) || auth_str == "") return(character(0))
  parts <- strsplit(auth_str, "\\s*;\\s*")[[1]]
  parts[parts != ""]
}
format_author_name <- function(x) {
  x <- trimws(x); if (!nzchar(x)) return("")
  if (grepl(",", x)) {
    parts <- strsplit(x, ",\\s*")[[1]]
    last <- parts[1]; fore <- parts[2] %||% ""
  } else {
    parts <- strsplit(x, "\\s+")[[1]]
    last <- parts[1]; fore <- paste(parts[-1], collapse=" ")
  }
  fp <- unlist(strsplit(trimws(fore), "\\s+"))
  initials <- if (length(fp)) paste(paste0(substr(fp,1,1), "."), collapse="") else ""
  if (nzchar(initials)) paste0(last, ", ", initials) else last
}
format_authors_generic <- function(auth_str, sep=", ", last_sep=" & ") {
  xs <- vapply(authors_to_list(auth_str), format_author_name, character(1))
  n <- length(xs)
  if (n == 0) return("")
  if (n == 1) return(xs[1])
  if (n <= 20) return(paste(paste(xs[1:(n-1)], collapse = sep), last_sep, xs[n], sep=""))
  paste(paste(xs[1:19], collapse = sep), " … ", xs[n])
}
fmt_citation <- function(style, authors, year, title, journal, doi) {
  y <- if (!is.na(year) && nzchar(year)) year else "n.d."
  d <- if (!is.na(doi) && nzchar(doi)) paste0(" https://doi.org/", doi) else ""
  a_generic <- format_authors_generic(authors)
  style <- toupper(trimws(style %||% "APA"))
  if (style == "APA")      paste0(a_generic, " (", y, "). ", title %||% "", ". ", journal %||% "", ".", d)
  else if (style == "MLA") paste0(a_generic, ". \"", title %||% "", ".\" ", journal %||% "", ", ", y, ".", d)
  else if (style == "CHICAGO") paste0(a_generic, ". ", y, ". \"", title %||% "", ".\" ", journal %||% "", ".", d)
  else if (style == "VANCOUVER") paste0(a_generic, ". ", title %||% "", ". ", journal %||% "", ". ", y, ".", d)
  else if (style == "HARVARD") paste0(a_generic, " ", y, ". ", title %||% "", ". ", journal %||% "", ".", d)
  else if (style == "IEEE") paste0(a_generic, ", \"", title %||% "", ",\" ", journal %||% "", ", ", y, ".", d)
  else if (style == "AMA") paste0(a_generic, ". ", title %||% "", ". ", journal %||% "", ". ", y, ".", d)
  else if (style == "ACS") paste0(a_generic, ". ", title %||% "", ". ", journal %||% "", ", ", y, ".", d)
  else if (style == "CSE") paste0(a_generic, ". ", y, ". ", title %||% "", ". ", journal %||% "", ".", d)
  else if (style == "TURABIAN") paste0(a_generic, ". ", "\"", title %||% "", ".\" ", journal %||% "", ", ", y, ".", d)
  else paste0(a_generic, " (", y, "). ", title %||% "", ". ", journal %||% "", ".", d)
}

scholar_url_for_title <- function(title, doi = NA_character_) {
  if (!is.na(title) && nzchar(title)) {
    q <- URLencode(paste0('"', title, '"'), reserved = TRUE)
    return(paste0("https://scholar.google.com/scholar?q=", q))
  }
  if (!is.na(doi) && nzchar(doi)) {
    q <- URLencode(paste0("doi:", doi), reserved = TRUE)
    return(paste0("https://scholar.google.com/scholar?q=", q))
  }
  "https://scholar.google.com/"
}

authors_links_short <- function(auth_str, keep = 6L) {
  if (is.na(auth_str) || auth_str == "") return("")
  parts <- strsplit(auth_str, "\\s*;\\s*")[[1]]; parts <- parts[parts != ""]
  make_link <- function(name) {
    q <- URLencode(paste0('author:"', name, '"'), reserved = TRUE)
    url <- paste0("https://scholar.google.com/scholar?q=", q)
    sprintf("<a href=\"%s\" target=\"_blank\" rel=\"noopener\">%s</a>", esc(url), esc(name))
  }
  if (length(parts) <= keep) return(paste(vapply(parts, make_link, character(1)), collapse = "; "))
  paste(paste(vapply(parts[1:keep], make_link, character(1)), collapse = "; "), "…")
}

oa_lookup_one <- function(doi, pmid) {
  base <- "https://api.openalex.org/works"
  url <- if (!is.na(doi) && doi != "") {
    paste0(base, "/", URLencode(paste0("https://doi.org/", doi), reserved = TRUE))
  } else if (!is.na(pmid) && pmid != "") {
    paste0(base, "/", URLencode(paste0("pmid:", pmid), reserved = TRUE))
  } else return(list())
  res <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(res) || isTRUE(res$error)) return(list())
  list(
    OA_Citations = res$cited_by_count %||% NA_integer_,
    OA_Status    = res$open_access$oa_status %||% NA_character_,
    OA_FullText  = (res$open_access$oa_url %||% res$best_oa_location$url %||% NA_character_)
  )
}

# ---- HTML export builder (standalone with DataTables)
build_export_html <- function(header_html, df) {
  keep_cols <- setdiff(colnames(df), ".haystack")
  df <- df[keep_cols]
  make_row <- function(i) paste0("<tr>", paste0("<td>", as.character(df[i, ]), "</td>", collapse = ""), "</tr>")
  rows <- paste0(vapply(seq_len(nrow(df)), make_row, character(1)), collapse = "\n")
  ths  <- paste0("<th>", keep_cols, "</th>", collapse = "")
  paste0(
    "<!DOCTYPE html>
<html lang='en'>
<head>
<meta charset='utf-8'/>
<meta name='viewport' content='width=device-width, initial-scale=1'/>
<title>PubMedSearchR results</title>
<link rel='stylesheet' href='https://cdn.datatables.net/1.13.8/css/jquery.dataTables.min.css'>
<link rel='stylesheet' href='https://cdn.datatables.net/buttons/2.4.2/css/buttons.dataTables.min.css'>
<style>
body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif; margin:20px;}
.muted{color:#555;}
.small{font-size:90%;}
/* table */
table.dataTable tbody th, table.dataTable tbody td { word-break:break-word; overflow-wrap:anywhere; }
td.col-abstract, th.col-abstract{min-width:600px; width:50vw;}
/* foldables */
details.foldable>summary{cursor:pointer; color:#444; text-decoration:underline dotted;}
details.foldable>summary::before{content:'▸ '; color:#888;}
details.foldable[open]>summary::before{content:'▾ ';}
/* toolbar row */
div.dttop { display:flex; flex-direction:row; align-items:center; gap:8px; margin-bottom:8px; }
div.dttop .dt-buttons { order:1; }
div.dttop .dataTables_filter { order:2; float:none; text-align:left; margin:0; }
.sidebar-link-wrap a, td a { word-break:break-word; overflow-wrap:anywhere; white-space:normal; }
</style>
<script src='https://code.jquery.com/jquery-3.7.1.min.js'></script>
<script src='https://cdn.datatables.net/1.13.8/js/jquery.dataTables.min.js'></script>
<script src='https://cdn.datatables.net/buttons/2.4.2/js/dataTables.buttons.min.js'></script>
<script src='https://cdn.datatables.net/buttons/2.4.2/js/buttons.html5.min.js'></script>
<script src='https://cdn.datatables.net/buttons/2.4.2/js/buttons.print.min.js'></script>
</head>
<body>
", as.character(header_html), "
<table id='results' class='display' style='width:100%'>
  <thead><tr>", ths, "</tr></thead>
  <tbody>
", rows, "
  </tbody>
</table>

<script>
$(function(){
  // Build custom buttons (expand/collapse) and set search label
  var btnExpand = {
    text: 'Expand all',
    action: function (e, dt, node, config) {
      document.querySelectorAll('details.foldable').forEach(d => d.open = true);
    }
  };
  var btnCollapse = {
    text: 'Collapse all',
    action: function (e, dt, node, config) {
      document.querySelectorAll('details.foldable').forEach(d => d.open = false);
    }
  };

  $('#results').DataTable({
    dom: '<\"dttop\"Bf>rt<\"dtbottom\"ip>',
    buttons: ['copy','csv','print', btnExpand, btnCollapse],
    pageLength: 1000,
    lengthMenu: [[25,50,100,250,500,1000,-1],[25,50,100,250,500,1000,'All']],
    autoWidth: true,
    language: { search: 'Search all fields:' },
    columnDefs: [
      { targets: 0, width: '60px' },   // Match
      { targets: 3, width: '70px' },   // Year
      { targets: 5, width: '120px' },  // PubMed ID
      { targets: 6, width: '180px' },  // DOI
      { targets: 7, width: '110px' },  // Scholar
      { targets: 8, width: '100px' },  // Cit total
      { targets: 9, width: '100px' },  // Cit/yr
      { targets: 10, width: '110px' }, // OA
      { targets: 11, width: '80px' },  // Full text
      { targets: 12, width: '100px' }  // Altmetrics
    ]
  });
});
</script>
</body></html>"
  )
}

# ================================ run ========================================

if (nzchar(ncbi_api_key)) try(rentrez::set_entrez_key(ncbi_api_key), silent = TRUE)

yb <- normalize_years(start_year, end_year)
query <- build_query(search_terms, yb, only_full_text)

message("Querying PubMed…")
srch <- entrez_search(db = "pubmed", term = query, retmax = results_requested, sort = "relevance")
ids  <- srch$ids
if (!length(ids)) stop("No records found.")

message(sprintf("Found %d IDs (retrieving details)…", length(ids)))
chunks <- split(ids, ceiling(seq_along(ids)/200L))
parts <- lapply(chunks, function(ch) {
  out <- try(fetch_parse_chunk(ch), silent = TRUE)
  if (inherits(out, "try-error")) data.frame() else out
})
db <- do.call(rbind, parts)

# rank / match number in best-match order
db$Match <- seq_len(nrow(db))

# OpenAlex enrichment
message("Enriching with OpenAlex (citations & OA)…")
enrich_rows <- vector("list", nrow(db))
for (i in seq_len(nrow(db))) {
  enrich_rows[[i]] <- oa_lookup_one(db$DOI[i], db$PMID[i])
  Sys.sleep(if (nzchar(ncbi_api_key)) 0.05 else 0.12)
}
enrich_df <- if (length(enrich_rows)) do.call(rbind, lapply(enrich_rows, as.data.frame, stringsAsFactors = FALSE)) else NULL
if (!is.null(enrich_df)) for (nm in setdiff(names(enrich_df), names(db))) db[[nm]] <- enrich_df[[nm]]

# derived metrics + links
yr_now <- as.integer(format(Sys.Date(), "%Y"))
pub_y  <- suppressWarnings(as.integer(db$PubYear))
yrs_published <- ifelse(!is.na(pub_y), pmax(1L, yr_now - pub_y + 1L), NA_integer_)
db$Citations_total    <- suppressWarnings(as.integer(db$OA_Citations))
db$Citations_per_year <- ifelse(!is.na(db$Citations_total) & !is.na(yrs_published) & yrs_published > 0,
                                round(db$Citations_total / yrs_published, 2), NA_real_)
db$Open_access   <- db$OA_Status
db$PubMed_URL    <- ifelse(!is.na(db$PMID) & db$PMID!="", paste0("https://pubmed.ncbi.nlm.nih.gov/", db$PMID, "/"), NA_character_)
db$DOI_URL       <- ifelse(!is.na(db$DOI) & db$DOI!="", paste0("https://doi.org/", db$DOI), NA_character_)
db$Altmetric_URL <- ifelse(!is.na(db$DOI) & db$DOI!="", paste0("https://www.altmetric.com/details.php?doi=", db$DOI), NA_character_)
db$Scholar_URL   <- mapply(scholar_url_for_title, db$Title, db$DOI, USE.NAMES = FALSE)

# display helpers
pmid_link_from <- function(pmid) if (!is.na(pmid) && pmid!="") sprintf("<a href='https://pubmed.ncbi.nlm.nih.gov/%s/' target='_blank' rel='noopener'>%s</a>", esc(pmid), esc(pmid)) else ""
doi_cell_from  <- function(doi)  if (!is.na(doi)  && doi !="")  sprintf("<a class='wrap' href='https://doi.org/%s' target='_blank' rel='noopener'>%s</a>", esc(doi), esc(doi)) else ""
scholar_link_from <- function(url) if (!is.na(url) && url!="") sprintf("<a href='%s' target='_blank' rel='noopener'>Scholar</a>", esc(url)) else ""
link_cell <- function(url, label="Link") if (!is.na(url) && url!="") sprintf("<a href='%s' target='_blank' rel='noopener'>%s</a>", esc(url), esc(label)) else ""

# short authors (HTML with Scholar links; CSV will keep full plain text)
db$AuthorsShortHTML <- vapply(db$Authors, authors_links_short, character(1), keep = 6L)

# formatted citation (user-selected style)
db$Citation <- mapply(fmt_citation, citation_format, db$Authors, db$PubYear, db$Title, db$Journal, db$DOI, USE.NAMES = FALSE)

# --------------------------- build HTML table --------------------------------
df_disp <- do.call(
  rbind.data.frame,
  lapply(seq_len(nrow(db)), function(i) {
    r <- db[i, , drop = FALSE]
    data.frame(
      Match = r$Match,
      Title = esc(r$Title),
      Authors = r$AuthorsShortHTML %||% "",
      Year = esc(r$PubYear),
      Journal = esc(r$Journal),
      `PubMed ID` = pmid_link_from(r$PMID),
      DOI = doi_cell_from(r$DOI),
      `Google Scholar` = scholar_link_from(r$Scholar_URL),
      `Citations total` = esc(r$Citations_total),
      `Citations per year` = esc(r$Citations_per_year),
      `Open access` = esc(r$Open_access),
      `Full text` = link_cell(r$OA_FullText, "Link"),
      Altmetrics = link_cell(r$Altmetric_URL, "Altmetric"),
      Abstract = if (!is.na(r$Abstract) && nzchar(r$Abstract))
        sprintf("<details class='foldable'><summary>Read abstract</summary><div>%s</div></details>", esc(r$Abstract)) else "",
      `Author keywords` = if (!is.na(r$AuthorKeywords) && nzchar(r$AuthorKeywords))
        sprintf("<details class='foldable'><summary>View keywords</summary><div>%s</div></details>", esc(r$AuthorKeywords)) else "",
      Citation = if (!is.na(r$Citation) && nzchar(r$Citation))
        sprintf("<details class='foldable'><summary>Citation</summary><div>%s</div></details>", esc(r$Citation)) else "",
      .haystack = esc(paste(r$Title, r$Authors, r$Journal, r$Abstract, r$AuthorKeywords, r$Citation)),
      check.names = FALSE
    )
  })
)

# header block
years_label <- if (is.na(yb$sy)) "All years" else sprintf("%d–%d", yb$sy, yb$ey)
timestamp_str <- format(Sys.time(), "%Y/%m/%d %H:%M")
header_html <- HTML(paste0(
  "<h2><strong>Literature search</strong></h2>",
  "<div class='muted'>", esc(timestamp_str), " (YYYY/MM/DD HH:MM)</div>",
  "<ul>",
  "<li><b>Query (original):</b> ", esc(query), "</li>",
  "<li><b>Query (translation):</b> ", esc(srch$QueryTranslation %||% ""), "</li>",
  "<li><b>Time range:</b> ", esc(years_label), "</li>",
  "<li><b>Only results with full text:</b> ", if (isTRUE(only_full_text)) "TRUE" else "FALSE", "</li>",
  "<li><b>Results requested:</b> ", results_requested, "</li>",
  "<li><b>Results found:</b> ", nrow(db), "</li>",
  "<li><b>Citation format:</b> ", esc(citation_format), "</li>",
  "</ul>",
  "<p class='small sidebar-link-wrap'>Open access values come from OpenAlex (e.g., gold, green, bronze, closed). ",
  "See <a href='https://en.wikipedia.org/wiki/Open_access' target='_blank' rel='noopener'>Open access</a> for an overview.</p>",
  "<p class='small'>This page attempts to find relevant papers by PubMed’s query and MeSH translation. ",
  "About MeSH: <a href='https://www.ncbi.nlm.nih.gov/mesh/' target='_blank' rel='noopener'>MeSH browser</a>.</p>",
  "<p class='small sidebar-link-wrap'>If you use this application in you research, please cite:<br/>",
  "Eelke de Vries (2025). <i>PubMedSearchR: Reproducible Literature Search in R.</i><br/>",
  "<a href='https://github.com/eelkedevries/PubMedSearchR' target='_blank' rel='noopener'>https://github.com/eelkedevries/PubMedSearchR</a></p>"
))

# ----------------------------- write files -----------------------------------

# CSV (plain values + URLs)
csv_cols <- c(
  "Match","Title","Authors","PubYear","Journal",
  "PMID","PubMed_URL",
  "DOI","DOI_URL",
  "Scholar_URL",
  "Citations_total","Citations_per_year",
  "Open_access","OA_FullText","Altmetric_URL",
  "Abstract","AuthorKeywords","Citation"
)
csv_cols <- intersect(csv_cols, names(db))
csv_df <- db[, csv_cols, drop = FALSE]
names(csv_df) <- sub("^PubYear$", "Year", names(csv_df))
names(csv_df) <- sub("^PMID$", "PubMed ID", names(csv_df))
names(csv_df) <- sub("^OA_FullText$", "Full text", names(csv_df))
names(csv_df) <- sub("^Open_access$", "Open access", names(csv_df))
names(csv_df) <- sub("^Citations_total$", "Citations total", names(csv_df))
names(csv_df) <- sub("^Citations_per_year$", "Citations per year", names(csv_df))
names(csv_df) <- sub("^Scholar_URL$", "Google Scholar", names(csv_df))
names(csv_df) <- sub("^Altmetric_URL$", "Altmetrics", names(csv_df))

years_tag <- if (is.na(yb$sy)) "AllYears" else sprintf("%d-%d", yb$sy, yb$ey)
base_name <- paste0(safe_filename(search_terms), "_", years_tag)
csv_file  <- file.path(script_dir, paste0(base_name, ".csv"))
html_file <- file.path(script_dir, paste0(base_name, ".html"))

write.csv(csv_df, file = csv_file, row.names = FALSE)
html_out <- build_export_html(header_html, df_disp)
writeLines(html_out, html_file, useBytes = TRUE)

message(sprintf("Wrote %d records to:", nrow(db)))
message(sprintf("  CSV : %s", csv_file))
message(sprintf("  HTML: %s", html_file))
