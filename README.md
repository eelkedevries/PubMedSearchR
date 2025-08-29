# PubMedSearchR

**PubMedSearchR** is available as an **R script** and as a **Shiny app**. It performs reproducible [PubMed](https://pubmed.ncbi.nlm.nih.gov/) search and screening in R. It supports boolean queries and date/full-text filters, shows PubMed’s MeSH-based “query translation”, enriches results with [OpenAlex](https://openalex.org/) citations and open-access status, and exports to CSV/HTML. See also the [MeSH Browser](https://www.ncbi.nlm.nih.gov/mesh/).

**Background / motivation:**
I built this to improve and modernise a script I used almost 10 years ago for reproducible literature search. The goal is simple: keep the workflow transparent, easy to re-run, and suitable for sharing with co-authors or students.

**What it does (in short):**
Run PubMed searches from R and review the results in an interactive Shiny table. You can write boolean queries (please use double quotes for multi-word phrases), set start/end year (optional), and choose to include only items with full text. The app shows the automatic MeSH mapping that PubMed applies (“Query translation”), so you can see how PubMed interpreted your terms. Results keep PubMed’s **Best Match** order.

**Data returned per record:**

* **Core metadata:** Title, Authors, Journal, Publication year
* **Identifiers & links:** PMID + **PubMed URL** ([pubmed.ncbi.nlm.nih.gov](https://pubmed.ncbi.nlm.nih.gov/)), DOI + **DOI URL** ([doi.org](https://doi.org/)), **Google Scholar** link ([scholar.google.com](https://scholar.google.com/)), **Altmetric** link ([altmetric.com](https://www.altmetric.com/)), and best open-access full-text URL (when available via OpenAlex)
* **Content:** Abstract (fold/unfold), Author keywords
* **Metrics:** **Citations total** and **Citations per year** (from **OpenAlex**)
* **Access:** **Open access** status (gold/green/bronze/closed, from OpenAlex)
* **Formatted citation:** APA (default) or MLA / Chicago / Vancouver / Harvard / IEEE / AMA / ACS / CSE / Turabian

**Interface & export:**

* Shiny table with sorting, column filters, global search (also searches folded Abstract/Keywords/Citation), expand/collapse-all buttons, multi-line headers, and by default 1000 rows visible
* CSV export (with explicit URL columns so links work in Excel/Sheets) and a self-contained interactive HTML output

**Useful links / APIs:**

* PubMed: [https://pubmed.ncbi.nlm.nih.gov/](https://pubmed.ncbi.nlm.nih.gov/)
* MeSH Browser (term look-up): [https://www.ncbi.nlm.nih.gov/mesh/](https://www.ncbi.nlm.nih.gov/mesh/)
* OpenAlex (citations and open access): [https://openalex.org/](https://openalex.org/)
* Google Scholar (title/DOI search): [https://scholar.google.com/](https://scholar.google.com/)
* DOI resolver: [https://doi.org/](https://doi.org/)
* Altmetric details: [https://www.altmetric.com/](https://www.altmetric.com/)
* NCBI E-utilities (used via **rentrez**): [https://www.ncbi.nlm.nih.gov/books/NBK25501/](https://www.ncbi.nlm.nih.gov/books/NBK25501/)

*Notes:* PubMed sometimes maps your words to MeSH terms to improve recall; you can see this in “Query translation”. Citation counts and OA labels come from OpenAlex, so they may not always match publisher pages.
