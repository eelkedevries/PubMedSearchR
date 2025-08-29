# =============================================================================
# app.R — PubMedSearchR (Shiny)
# Author: Eelke de Vries — https://github.com/eelkedevries/PubMedSearchR
#
# Shiny app for reproducible PubMed search + OpenAlex enrichment.
# - Boolean query (use double quotes for multi-word phrases).
# - Optional start/end year; empty = all years. Optional only full text.
# - Results keep PubMed “Best Match” order.
# - Folded text (Abstract, Keywords, Citation) stays searchable.
# - Logs provenance to cache/run_meta.json and lets you download sessionInfo().
#
# Packages (install once):
# install.packages(c("shiny","DT","rentrez","xml2","jsonlite","htmltools"))
# =============================================================================

library(shiny)
library(DT)
library(rentrez)
library(xml2)
library(jsonlite)
library(htmltools)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
esc <- function(x) { if (is.null(x)) return(""); x <- as.character(x); x[is.na(x)] <- ""; htmlEscape(x) }

# app meta
tool_title   <- "PubMedSearchR"
tool_version <- "0.9.3"

# ------- small XML helpers
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

# ------- query helpers
normalize_years <- function(start_year, end_year) {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  sy <- if (is.na(start_year) && is.na(end_year)) NA_integer_ else if (is.na(start_year)) 1800L else as.integer(start_year)
  ey <- if (is.na(start_year) && is.na(end_year)) NA_integer_ else if (is.na(end_year)) cy else as.integer(end_year)
  if (!is.na(sy) && !is.na(ey) && ey < sy) { tmp <- sy; sy <- ey; ey <- tmp }
  list(sy = sy, ey = ey)
}
build_query <- function(terms, yb, only_ft) {
  if (!nzchar(terms)) stop("search_terms is empty.")
  q <- sprintf("(%s)", terms)
  if (!is.na(yb$sy)) q <- paste(q, sprintf("%d:%d[PDAT]", yb$sy, yb$ey), sep = " AND ")
  if (isTRUE(only_ft)) q <- paste(q, '"full text"[sb]', sep = " AND ")
  q
}

# ------- formatting for authors & citations
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
authors_to_list <- function(auth_str) {
  if (is.na(auth_str) || auth_str == "") return(character(0))
  parts <- strsplit(auth_str, "\\s*;\\s*")[[1]]
  parts[parts != ""]
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

# ---- ALWAYS open in external browser: inline onclick + return false
make_ext_link <- function(url, label = NULL) {
  if (is.na(url) || !nzchar(url)) return("")
  if (is.null(label)) label <- url
  sprintf(
    "<a href=\"%s\" target=\"_blank\" rel=\"noopener\" onclick=\"window.open(this.href, '_blank', 'noopener,noreferrer'); return false;\">%s</a>",
    esc(url), esc(label)
  )
}

authors_links_short <- function(auth_str, keep = 6L) {
  if (is.na(auth_str) || auth_str == "") return("")
  parts <- strsplit(auth_str, "\\s*;\\s*")[[1]]; parts <- parts[parts != ""]
  make_link <- function(name) {
    q <- URLencode(paste0('author:"', name, '"'), reserved = TRUE)
    url <- paste0("https://scholar.google.com/scholar?q=", q)
    make_ext_link(url, name)
  }
  if (length(parts) <= keep) return(paste(vapply(parts, make_link, character(1)), collapse = "; "))
  paste(paste(vapply(parts[1:keep], make_link, character(1)), collapse = "; "), "…")
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

# ------- build HTML export
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
<title>PubMedSearchR export</title>
<style>
body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif; margin:20px;}
.muted{color:#555;}
table{border-collapse:collapse; width:100%;}
th,td{border:1px solid #ddd; padding:8px; vertical-align:top;}
th{background:#f6f6f6;}
td.wrap, td a {word-break:break-word; overflow-wrap:anywhere;}
td.col-abstract, th.col-abstract{min-width:600px; width:50vw;}
details.foldable>summary{cursor:pointer; color:#444; text-decoration:underline dotted;}
details.foldable>summary::before{content:'▸ '; color:#888;}
details.foldable[open]>summary::before{content:'▾ ';}
a{color:#0366d6;}
</style>
</head>
<body>
", as.character(header_html), "
<table class='results'>
<thead><tr>", ths, "</tr></thead>
<tbody>
", rows, "
</tbody>
</table>
</body></html>"
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif; }
      .muted { color:#555; }
      .small { font-size: 90%; }
      .subtitle { color:#666; font-size: 90%; margin:-10px 0 20px 4px; }
      details { margin-top: 4px; }
      .dataTable { table-layout: auto !important; }
      td.wrap, td.wrap a, .wrap a { word-break: break-word; overflow-wrap: anywhere; }
      /* Top controls: buttons + search on one row (left-aligned) */
      div.dttop { display:flex; flex-direction:row; align-items:center; gap:8px; margin-bottom:8px; }
      div.dttop .dt-buttons { order:1; }
      div.dttop .dataTables_filter { order:2; float:none; text-align:left; margin:0; }
      /* Wider Abstract column */
      td.col-abstract, th.col-abstract { min-width: 600px; width: 50vw; }
      @media (max-width: 1400px) {
        td.col-abstract, th.col-abstract { min-width: 480px; width: 45vw; }
      }
      /* Make foldable controls clearly clickable with chevrons */
      details.foldable > summary { cursor:pointer; color:#444; text-decoration: underline dotted; }
      details.foldable > summary::before { content: '▸ '; color:#888; }
      details.foldable[open] > summary::before { content: '▾ '; }
      /* Keep the folded-info snug below API key and farther from Run button */
      .api-info { margin-top:4px; margin-bottom:20px; }
      /* Ensure long links in sidebar can wrap to multiple lines */
      .sidebar-link-wrap a { word-break: break-word; overflow-wrap: anywhere; white-space: normal; display: inline-block; max-width: 100%; }
    "))
  ),
  
  # Title + subtitle (above sidebar)
  titlePanel("PubMedSearchR"),
  div(class = "subtitle",
      "PubMedSearchR is an R script and Shiny app for reproducible PubMed searches. ",
      "It supports boolean queries, date and full-text filters, shows PubMed’s MeSH translation, ",
      "and enriches results with OpenAlex citations and open-access status. ",
      "Review results in an interactive table and export to CSV/HTML."
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Query input + (grey, smaller) bullets
      textInput("terms", "Search query (PubMed)",
                value = 'saccades AND \"working memory\"',
                placeholder = 'Example: saccades AND \"working memory\"'),
      div(class = "muted small",
          tags$ul(
            tags$li("Use AND / OR / NOT for boolean."),
            tags$li(HTML('Put multi-word terms in double quotes (e.g., \"<i>working memory</i>\").')),
            tags$li(HTML('You can also use <a href=\"https://www.ncbi.nlm.nih.gov/mesh/\" target=\"_blank\" rel=\"noopener\" onclick=\"window.open(this.href, `_blank`, `noopener,noreferrer`); return false;\">MeSH terms</a>.'))
          )
      ),
      
      # Years (relabelled)
      textInput("start_year", "Start year (leave empty for no lower bound)", value = ""),
      textInput("end_year", "End year (leave empty for no upper bound)", value = ""),
      
      checkboxInput("only_ft", "Only results with full text", FALSE),
      numericInput("retmax", "Results requested", value = 100, min = 1, step = 1),
      
      selectInput("cite_fmt", "Citation format",
                  c("APA","MLA","Chicago","Vancouver","Harvard","IEEE","AMA","ACS","CSE","Turabian"), "APA"),
      
      # NCBI API key (optional) + fold-out info (immediately under the field)
      textInput("ncbi_key", "NCBI API key (optional)", value = ""),
      div(class="api-info", HTML(
        paste0(
          "<details class='foldable'><summary class='small' ",
          "style='cursor:pointer;color:#0366d6;text-decoration:underline;'>",
          "Click here for more info about the NCBI API key</summary>",
          "<ul>",
          "<li>Using PubMed is free and does not require an API key. ",
          "Without a key: up to ~3 requests/second. With a key: up to ~10 requests/second.</li>",
          "<li>Create an account: ",
          make_ext_link("https://www.ncbi.nlm.nih.gov/account/", "NCBI Account"),
          ", then get your key: ",
          make_ext_link("https://account.ncbi.nlm.nih.gov/settings/", "Account Settings"),
          ".</li>",
          "</ul></details>"
        )
      )),
      
      actionButton("run","Run search", class = "btn btn-primary"),
      tags$hr(),
      
      # Sidebar footer (citation text + repo link that wraps across lines)
      HTML(paste0(
        "<div class='sidebar-link-wrap'>",
        "<p>Developed by ",
        make_ext_link("https://github.com/eelkedevries/PubMedSearchR", "Eelke de Vries"),
        ".</p>",
        "<p>If you use this application in you research, please cite:<br/>",
        "Eelke de Vries (2025). <i>PubMedSearchR: Reproducible Literature Search in R.</i><br/>",
        make_ext_link("https://github.com/eelkedevries/PubMedSearchR",
                      "https://github.com/eelkedevries/PubMedSearchR"),
        "</p>",
        "<p>Open access values are from ",
        make_ext_link("https://openalex.org/", "OpenAlex"),
        ". For background: ",
        make_ext_link("https://en.wikipedia.org/wiki/Open_access", "Open access"),
        ".</p>",
        "</div>"
      )),
      
      tags$hr(),
      downloadButton("download_session","Download session info"),
      width = 4
    ),
    
    mainPanel(
      htmlOutput("header_html"),
      tags$div(style="display:none;", downloadButton("download_html","Download HTML")),
      DTOutput("table"), width = 8
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(df = NULL, df_csv = NULL, header = NULL, meta = NULL)
  
  cache_dir <- "cache"
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  parse_year <- function(x) {
    x <- trimws(x %||% "")
    if (!nzchar(x)) return(NA_integer_)
    y <- suppressWarnings(as.integer(x))
    if (is.na(y)) NA_integer_ else y
  }
  
  observeEvent(input$run, {
    req(nzchar(input$terms))
    
    # apply API key if provided
    try({
      key <- trimws(input$ncbi_key %||% "")
      if (nzchar(key)) rentrez::set_entrez_key(key)
    }, silent = TRUE)
    
    sy <- parse_year(input$start_year)
    ey <- parse_year(input$end_year)
    yb <- normalize_years(sy, ey)
    searchQ <- build_query(input$terms, yb, isTRUE(input$only_ft))
    retmax  <- as.integer(input$retmax %||% 100)
    
    withProgress(message = "Querying PubMed…", value = 0.15, {
      srch <- entrez_search(db = "pubmed", term = searchQ, retmax = retmax, sort = "relevance")
      ids  <- srch$ids
      if (length(ids) == 0) {
        showNotification("No records found for this query.", type = "warning")
        rv$df <- NULL; rv$df_csv <- NULL; rv$meta <- NULL
        return(NULL)
      }
      incProgress(0.25, detail = "Fetching details…")
      chunks <- split(ids, ceiling(seq_along(ids)/200L))
      parts <- lapply(chunks, function(ch) {
        out <- try(fetch_parse_chunk(ch), silent = TRUE)
        if (inherits(out, "try-error")) data.frame() else out
      })
      db <- do.call(rbind, parts)
      db$Match <- seq_len(nrow(db))
      
      incProgress(0.70, detail = "Enriching (citations, OA)…")
      enrich_rows <- vector("list", nrow(db))
      for (i in seq_len(nrow(db))) {
        enrich_rows[[i]] <- oa_lookup_one(db$DOI[i], db$PMID[i])
        Sys.sleep(0.12)
      }
      enrich_df <- if (length(enrich_rows)) do.call(rbind, lapply(enrich_rows, as.data.frame, stringsAsFactors=FALSE)) else NULL
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
      
      # anchor builders using inline onclick to suppress R internal windows
      pmid_link_from <- function(pmid) if (!is.na(pmid) && pmid!="") make_ext_link(paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"), pmid) else ""
      doi_cell_from  <- function(doi)  if (!is.na(doi)  && doi !="")  make_ext_link(paste0("https://doi.org/", doi), doi) else ""
      scholar_link_from <- function(url) if (!is.na(url) && url!="") make_ext_link(url, "Scholar") else ""
      link_cell <- function(url, label="Link") if (!is.na(url) && url!="") make_ext_link(url, label) else ""
      
      db$AuthorsShortHTML <- vapply(db$Authors, authors_links_short, character(1), keep = 6L)
      db$Citation <- mapply(fmt_citation, input$cite_fmt, db$Authors, db$PubYear, db$Title, db$Journal, db$DOI, USE.NAMES = FALSE)
      
      # display columns
      display_cols <- c(
        "Match",
        "Title","AuthorsShortHTML","PubYear","Journal",
        "PMID","DOI","Scholar_URL",
        "Citations_total","Citations_per_year","Open_access","OA_FullText","Altmetric_URL",
        "Abstract","AuthorKeywords","Citation"
      )
      display_cols <- intersect(display_cols, names(db))
      
      # assemble display rows (HTML)
      to_row <- function(i) {
        r <- db[i, , drop = FALSE]
        list(
          Match = r$Match,
          Title = esc(r$Title),
          AuthorsShortHTML = r$AuthorsShortHTML %||% "",
          PubYear = esc(r$PubYear),
          Journal = esc(r$Journal),
          PMID = pmid_link_from(r$PMID),
          DOI  = doi_cell_from(r$DOI),
          Scholar_URL = scholar_link_from(r$Scholar_URL),
          Citations_total = esc(r$Citations_total),
          Citations_per_year = esc(r$Citations_per_year),
          Open_access = esc(r$Open_access),
          OA_FullText = link_cell(r$OA_FullText, "Link"),
          Altmetric_URL = link_cell(r$Altmetric_URL, "Altmetric"),
          Abstract = if (!is.na(r$Abstract) && nzchar(r$Abstract))
            sprintf("<details class='foldable'><summary>Read abstract</summary><div>%s</div></details>", esc(r$Abstract)) else "",
          AuthorKeywords = if (!is.na(r$AuthorKeywords) && nzchar(r$AuthorKeywords))
            sprintf("<details class='foldable'><summary>View keywords</summary><div>%s</div></details>", esc(r$AuthorKeywords)) else "",
          Citation = if (!is.na(r$Citation) && nzchar(r$Citation))
            sprintf("<details class='foldable'><summary>Citation</summary><div>%s</div></details>", esc(r$Citation)) else "",
          .haystack = esc(paste(r$Title, r$Authors, r$Journal, r$Abstract, r$AuthorKeywords, r$Citation))
        )
      }
      df_list <- lapply(seq_len(nrow(db)), to_row)
      df_disp <- do.call(rbind.data.frame, lapply(df_list, as.data.frame, stringsAsFactors = FALSE))
      
      # headers
      friendly <- c(
        Match="Match",
        Title="Title",
        AuthorsShortHTML="Authors",
        PubYear="Year",
        Journal="Journal",
        PMID="PubMed<br>ID",
        DOI="DOI",
        Scholar_URL="Google<br>Scholar",
        Citations_total="Citations<br>total",
        Citations_per_year="Citations<br>per<br>year",
        Open_access="Open<br>access",
        OA_FullText="Full<br>text",
        Altmetric_URL="Altmetrics",
        Abstract="Abstract",
        AuthorKeywords="Author<br>keywords",
        Citation="Citation",
        .haystack=".haystack"
      )
      cn <- colnames(df_disp)
      colnames(df_disp) <- ifelse(cn %in% names(friendly), friendly[cn], cn)
      rv$df <- df_disp
      
      # CSV data (plain columns with URLs)
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
      rv$df_csv <- db[, csv_cols, drop = FALSE]
      
      # header (HTML)
      years_label <- if (is.na(yb$sy)) "All years" else sprintf("%d–%d", yb$sy, yb$ey)
      timestamp_str <- format(Sys.time(), "%Y/%m/%d %H:%M")
      header_html <- HTML(paste0(
        "<h3><strong>Results</strong></h3>",
        "<div class='muted'>", esc(timestamp_str), " (YYYY/MM/DD HH:MM)</div>",
        "<ul>",
        "<li><b>Query (original):</b> ", esc(searchQ), "</li>",
        "<li><b>Query (translation):</b> ", esc(srch$QueryTranslation %||% ""), "</li>",
        "<li><b>Time range:</b> ", esc(years_label), "</li>",
        "<li><b>Only results with full text:</b> ", if (isTRUE(input$only_ft)) "TRUE" else "FALSE", "</li>",
        "<li><b>Results requested:</b> ", retmax, "</li>",
        "<li><b>Results found:</b> ", nrow(db), "</li>",
        "<li><b>Citation format:</b> ", esc(input$cite_fmt), "</li>",
        "<li><b>App version:</b> ", esc(tool_version), "</li>",
        "</ul>"
      ))
      rv$header <- header_html
      
      # provenance meta
      meta <- list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        app_title = tool_title,
        app_version = tool_version,
        query_original = searchQ,
        query_translation = srch$QueryTranslation %||% "",
        start_year = yb$sy, end_year = yb$ey,
        only_full_text = isTRUE(input$only_ft),
        results_requested = retmax,
        results_found = nrow(db),
        R = R.version$version.string,
        session_info = capture.output(sessionInfo())
      )
      rv$meta <- meta
      jsonlite::write_json(meta, file.path(cache_dir, "run_meta.json"),
                           auto_unbox = TRUE, pretty = TRUE)
      
      incProgress(1)
    })
  })
  
  output$header_html <- renderUI({ req(rv$header); rv$header })
  
  output$table <- renderDT({
    req(rv$df)
    
    # Buttons: Expand/Collapse + HTML export
    btn_expand <- JS("{
      text: 'Expand all',
      action: function (e, dt, node, config) {
        document.querySelectorAll('details.foldable').forEach(d => d.open = true);
      }
    }")
    btn_collapse <- JS("{
      text: 'Collapse all',
      action: function (e, dt, node, config) {
        document.querySelectorAll('details.foldable').forEach(d => d.open = false);
      }
    }")
    btn_html <- JS("{
      text: 'HTML',
      action: function (e, dt, node, config) {
        var el = document.getElementById('download_html');
        if (el) el.click();
      }
    }")
    
    datatable(
      rv$df,
      escape = FALSE,
      rownames = FALSE,
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        dom = '<\"dttop\"Bf>rt<\"dtbottom\"ip>',
        buttons = list('copy', 'csv', 'print', btn_html, btn_expand, btn_collapse),
        pageLength = 1000,
        lengthMenu = list(c(25, 50, 100, 250, 500, 1000, -1), c('25','50','100','250','500','1000','All')),
        autoWidth = TRUE,
        language = list(search = "Search all fields:"),
        columnDefs = (function(){
          idx <- function(label_regex) {
            m <- grep(label_regex, names(rv$df), perl = TRUE)
            if (length(m)) m[1] - 1L else NA_integer_
          }
          col_idx <- list(
            haystack  = idx("^\\.haystack$"),
            match     = idx("^Match$"),
            year      = idx("^Year$"),
            pmid      = idx("^PubMed<br>ID$"),
            doi       = idx("^DOI$"),
            scholar   = idx("^Google<br>Scholar$"),
            cit_tot   = idx("^Citations<br>total$"),
            cit_py    = idx("^Citations<br>per<br>year$"),
            oa        = idx("^Open<br>access$"),
            fulltext  = idx("^Full<br>text$"),
            altm      = idx("^Altmetrics$"),
            abstract  = idx("^Abstract$"),
            keywords  = idx("^Author<br>keywords$"),
            citation  = idx("^Citation$")
          )
          defs <- list()
          ad <- function(def) if (length(def)) defs[[length(defs)+1]] <<- def
          if (!is.na(col_idx$haystack)) ad(list(visible = FALSE, targets = col_idx$haystack))
          if (!is.na(col_idx$match))    ad(list(width = "60px",  targets = col_idx$match))
          if (!is.na(col_idx$year))     ad(list(width = "70px",  targets = col_idx$year))
          if (!is.na(col_idx$pmid))     ad(list(width = "120px", targets = col_idx$pmid))
          if (!is.na(col_idx$doi))      ad(list(width = "180px", targets = col_idx$doi))
          if (!is.na(col_idx$scholar))  ad(list(width = "110px", targets = col_idx$scholar))
          if (!is.na(col_idx$cit_tot))  ad(list(width = "100px", targets = col_idx$cit_tot))
          if (!is.na(col_idx$cit_py))   ad(list(width = "100px", targets = col_idx$cit_py))
          if (!is.na(col_idx$oa))       ad(list(width = "110px", targets = col_idx$oa))
          if (!is.na(col_idx$fulltext)) ad(list(width = "80px",  targets = col_idx$fulltext))
          if (!is.na(col_idx$altm))     ad(list(width = "100px", targets = col_idx$altm))
          if (!is.na(col_idx$abstract)) ad(list(className = "col-abstract", targets = col_idx$abstract))
          if (!is.na(col_idx$keywords)) ad(list(className = "col-fold",     targets = col_idx$keywords))
          if (!is.na(col_idx$citation)) ad(list(className = "col-fold",     targets = col_idx$citation))
          defs
        })()
      )
    )
  })
  
  # HTML export (triggered by the DT “HTML” button)
  output$download_html <- downloadHandler(
    filename = function() paste0("pubmedsearch_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".html"),
    content = function(file) {
      req(rv$df, rv$header)
      html <- build_export_html(rv$header, rv$df)
      writeLines(html, file, useBytes = TRUE)
    }
  )
  
  # session info download
  output$download_session <- downloadHandler(
    filename = function() paste0("session_info_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt"),
    content  = function(file) {
      req(rv$meta)
      m <- rv$meta
      lines <- c(
        paste0(m$app_title, " v", m$app_version),
        paste0("Timestamp: ", m$timestamp),
        paste0("Query (original): ", m$query_original),
        paste0("Query (translation): ", m$query_translation),
        paste0("Start year: ", m$start_year), paste0("End year: ", m$end_year),
        paste0("Only full text: ", m$only_full_text),
        paste0("Results requested: ", m$results_requested),
        paste0("Results found: ", m$results_found),
        paste0("R: ", m$R),
        "",
        "----- sessionInfo() -----",
        m$session_info
      )
      writeLines(lines, file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
