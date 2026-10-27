library(jsonlite)
library(base64enc)

# ========================================
# INPUT FILES
# ========================================
json_file <- "Extraction_Texte1_HS.json"
csv_tv_v_file <- "Texte1_Extraction_clean.csv"
csv_segments_file <- "segments_with_context.csv"
output_file <- sub("\\.json$", "_viewer_v10_final.html", json_file)

cat("\n==============================================\n")
cat("TERMINOLOGY VIEWER - V10 FINAL\n")
cat("==============================================\n\n")

# ========================================
# LOAD DATA
# ========================================
cat("Loading JSON data...\n")
json_data <- fromJSON(json_file, simplifyVector = FALSE)
full_text <- json_data$data$text
annotations <- json_data$annotations[[1]]$result

cat("Loading TV-Variant links CSV...\n")
tv_v_links <- read.csv(csv_tv_v_file, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
cat(sprintf("  - %d TV-Variant links found\n", nrow(tv_v_links)))

cat("Loading segments with context CSV...\n")
segments_csv <- read.csv(csv_segments_file, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
cat(sprintf("  - %d rows with segments\n\n", nrow(segments_csv)))

# Color mapping
color_map <- list(
  "A" = "#e6a21b", "B" = "#fede4d", "C" = "#35aa32", "D" = "#87e285",
  "E" = "#0e89ea", "F" = "#72a9c5", "G" = "#8931f3", "H" = "#ac91cc", "I" = "#e11b86"
)

# Split into segments
segments <- strsplit(full_text, "\n\n+")[[1]]
segment_positions <- c(0)
segment_ends <- c()
for (i in seq_along(segments)) {
  if (i > 1) {
    segment_positions[i] <- segment_positions[i-1] + nchar(segments[i-1]) + 2
  }
  segment_ends[i] <- segment_positions[i] + nchar(segments[i])
}

# Get segment index from position
get_segment_index <- function(pos) {
  for (i in seq_along(segment_positions)) {
    if (pos >= segment_positions[i] && pos < segment_ends[i]) {
      return(i)
    }
  }
  return(length(segments))
}

# ========================================
# COLLECT ANNOTATIONS FROM JSON
# ========================================
cat("Processing annotations from JSON...\n")

all_annotations <- list()
all_terms <- list()
all_errors <- list()

for (ann in annotations) {
  if (!is.null(ann$value$taxonomy) && length(ann$value$taxonomy) > 0) {
    label <- ann$value$taxonomy[[1]][[1]]
    
    ann_start <- ann$value$start
    ann_end <- ann$value$end
    segment_idx <- get_segment_index(ann_start)
    
    if (label %in% c("TV", "V")) {
      term_data <- list(
        start = ann_start,
        end = ann_end,
        text = ann$value$text,
        type = label,
        category = label,
        code = label,
        segment = segment_idx,
        ann_id = ann$id,
        parent_id = if(!is.null(ann$parentID)) ann$parentID else NA
      )
      
      all_annotations[[length(all_annotations) + 1]] <- term_data
      all_terms[[length(all_terms) + 1]] <- term_data
      
    } else if (!is.null(ann$parentID)) {
      error_level <- ""
      if (length(ann$value$taxonomy) > 1 && length(ann$value$taxonomy[[2]]) > 0) {
        error_level <- ann$value$taxonomy[[2]][[1]]
      }
      
      comment <- ""
      if (!is.null(ann$meta) && !is.null(ann$meta$text) && length(ann$meta$text) > 0) {
        comment <- ann$meta$text[[1]]
      }
      
      error_data <- list(
        start = ann_start,
        end = ann_end,
        text = ann$value$text,
        type = "error",
        category = substr(label, 1, 1),
        code = label,
        segment = segment_idx,
        ann_id = ann$id,
        parent_id = ann$parentID,
        error_level = error_level,
        comment = comment
      )
      
      all_annotations[[length(all_annotations) + 1]] <- error_data
      all_errors[[length(all_errors) + 1]] <- error_data
    }
  }
}

cat(sprintf("  - Terms: %d\n", length(all_terms)))
cat(sprintf("  - Errors: %d\n\n", length(all_errors)))

# ========================================
# BUILD TERM GROUPS
# ========================================
cat("Building term groups...\n")

term_groups <- list()

for (term in all_terms) {
  if (is.na(term$parent_id) || term$parent_id == "") {
    term_groups[[term$ann_id]] <- list(
      id = term$ann_id,
      type = term$type,
      text = term$text,
      start = term$start,
      end = term$end,
      segment = term$segment,
      parts = list(term)
    )
  }
}

for (term in all_terms) {
  if (!is.na(term$parent_id) && term$parent_id != "") {
    if (!is.null(term_groups[[term$parent_id]])) {
      group <- term_groups[[term$parent_id]]
      group$parts[[length(group$parts) + 1]] <- term
      term_groups[[term$parent_id]] <- group
    }
  }
}

cat(sprintf("  - Term groups: %d\n\n", length(term_groups)))

# ========================================
# BUILD HIERARCHY FROM CSV DATA
# ========================================
cat("Building hierarchy from CSV data...\n")

csv_base_terms <- unique(tolower(trimws(tv_v_links$base_term)))
csv_variants <- unique(tolower(trimws(tv_v_links$variant)))
csv_all_terms <- unique(c(csv_base_terms, csv_variants))

# Group data by term_id from segments_csv
errors_by_term_id <- list()
for (i in 1:nrow(segments_csv)) {
  row <- segments_csv[i, ]
  term_id <- row$term_id
  
  if (is.null(errors_by_term_id[[term_id]])) {
    errors_by_term_id[[term_id]] <- list(
      term_id = term_id,
      term_type = row$term_type,
      english_term = row$english_term,
      context_en = ifelse(is.na(row$english_segment) || row$english_segment == "", "", row$english_segment),
      context_fr = ifelse(is.na(row$french_segment) || row$french_segment == "", "", row$french_segment),
      errors = list()
    )
  }
  
  error_data <- list(
    error_id = row$error_id,
    french_translation = row$french_translation,
    error_category = row$error_category,
    category_letter = substr(row$error_category, 1, 1),
    error_level = ifelse(is.na(row$error_level), "", as.character(row$error_level)),
    comment = ifelse(is.na(row$comment), "", row$comment)
  )
  
  errors_by_term_id[[term_id]]$errors[[length(errors_by_term_id[[term_id]]$errors) + 1]] <- error_data
}

# Group by english_term
terms_by_english <- list()
for (term_id in names(errors_by_term_id)) {
  occ <- errors_by_term_id[[term_id]]
  eng_lower <- tolower(trimws(occ$english_term))
  
  if (is.null(terms_by_english[[eng_lower]])) {
    terms_by_english[[eng_lower]] <- list(
      english_term = occ$english_term,
      term_type = occ$term_type,
      occurrences = list()
    )
  }
  
  terms_by_english[[eng_lower]]$occurrences[[length(terms_by_english[[eng_lower]]$occurrences) + 1]] <- occ
}

# Build TV-Variant hierarchy
unique_base_terms <- unique(tv_v_links$base_term)
tv_hierarchy <- list()

for (bt in unique_base_terms) {
  bt_lower <- tolower(trimws(bt))
  bt_data <- terms_by_english[[bt_lower]]
  bt_occurrences <- if (!is.null(bt_data)) bt_data$occurrences else list()
  
  bt_rows <- tv_v_links[tv_v_links$base_term == bt, ]
  variants_list <- list()
  
  for (i in 1:nrow(bt_rows)) {
    variant_text <- bt_rows$variant[i]
    variant_lower <- tolower(trimws(variant_text))
    variation_type <- bt_rows$variation_type[i]
    
    v_data <- terms_by_english[[variant_lower]]
    v_occurrences <- if (!is.null(v_data)) v_data$occurrences else list()
    
    variants_list[[length(variants_list) + 1]] <- list(
      text = variant_text,
      variation_type = variation_type,
      occurrences = v_occurrences,
      has_errors = length(v_occurrences) > 0
    )
  }
  
  tv_hierarchy[[bt]] <- list(
    base_term = bt,
    base_occurrences = bt_occurrences,
    variants = variants_list,
    has_base_errors = length(bt_occurrences) > 0,
    has_any_variant_errors = any(sapply(variants_list, function(v) v$has_errors))
  )
}

# Filter: only keep TV with errors
tv_hierarchy_filtered <- list()
for (bt_name in names(tv_hierarchy)) {
  h <- tv_hierarchy[[bt_name]]
  if (h$has_base_errors || h$has_any_variant_errors) {
    variants_with_errors <- Filter(function(v) v$has_errors, h$variants)
    if (h$has_base_errors || length(variants_with_errors) > 0) {
      h$variants <- variants_with_errors
      tv_hierarchy_filtered[[bt_name]] <- h
    }
  }
}
tv_hierarchy_filtered <- tv_hierarchy_filtered[order(names(tv_hierarchy_filtered))]

# Find orphan terms
orphan_terms <- list()
for (eng_lower in names(terms_by_english)) {
  if (!(eng_lower %in% csv_all_terms)) {
    orphan_terms[[eng_lower]] <- terms_by_english[[eng_lower]]
  }
}
orphan_terms <- orphan_terms[order(names(orphan_terms))]

cat(sprintf("  - Base terms with errors: %d\n", length(tv_hierarchy_filtered)))
cat(sprintf("  - Orphan terms: %d\n\n", length(orphan_terms)))

# ========================================
# BUILD TEXT VIEW WITH ANNOTATIONS
# ========================================
cat("Building text view...\n")

# Sort all annotations by start position
all_annotations_sorted <- all_annotations[order(sapply(all_annotations, function(a) a$start))]

# Build annotated text HTML organized by segments
annotated_text_html <- ""

for (seg_idx in seq_along(segments)) {
  segment_text <- segments[[seg_idx]]
  seg_start <- segment_positions[seg_idx]
  seg_end <- segment_ends[seg_idx]
  
  # Determine if segment is EN or FR
  seg_type <- if (seg_idx %% 2 == 1) "EN" else "FR"
  
  # Find annotations in this segment
  seg_annotations <- list()
  for (ann in all_annotations_sorted) {
    if (ann$start >= seg_start && ann$start < seg_end) {
      seg_annotations[[length(seg_annotations) + 1]] <- ann
    }
  }
  
  # Build segment HTML with annotations
  segment_html <- ""
  last_pos <- 1
  
  for (ann in seg_annotations) {
    # Add text before annotation
    ann_start_in_seg <- ann$start - seg_start + 1
    ann_end_in_seg <- ann$end - seg_start + 1
    
    if (ann_start_in_seg > last_pos) {
      before_text <- substr(segment_text, last_pos, ann_start_in_seg - 1)
      segment_html <- paste0(segment_html, htmltools::htmlEscape(before_text))
    }
    
    # Add annotated span
    ann_color <- "#999"
    if (ann$category %in% names(color_map)) {
      ann_color <- color_map[[ann$category]]
    } else if (ann$type == "TV") {
      ann_color <- "#2196F3"
    } else if (ann$type == "V") {
      ann_color <- "#9c27b0"
    }
    
    ann_class <- if (ann$type %in% c("TV", "V")) "term-annotation" else "error-annotation"
    ann_label <- if (ann$type %in% c("TV", "V")) ann$type else ann$code
    
    # Build tooltip text
    tooltip_text <- paste0(ann_label, ": ", ann$text)
    if (ann$type == "error" && !is.null(ann$comment) && ann$comment != "") {
      tooltip_text <- paste0(tooltip_text, " | ", ann$comment)
    }
    
    # Get category for filtering
    filter_cat <- if (ann$type %in% c("TV", "V")) ann$type else ann$category
    
    segment_html <- paste0(
      segment_html,
      '<span class="', ann_class, '" id="ann-', ann$ann_id, '" ',
      'style="background-color: ', ann_color, '40; border-bottom: 2px solid ', ann_color, ';" ',
      'data-ann-id="', ann$ann_id, '" ',
      'data-label="', htmltools::htmlEscape(ann_label), '" ',
      'data-category="', filter_cat, '" ',
      'data-text="', htmltools::htmlEscape(tolower(ann$text)), '" ',
      'title="', htmltools::htmlEscape(tooltip_text), '" ',
      'onclick="showAnnotationDetails(\'', ann$ann_id, '\', \'', htmltools::htmlEscape(ann_label), '\', \'', htmltools::htmlEscape(ann$text), '\')">',
      '<span class="ann-label">', ann_label, '</span>',
      htmltools::htmlEscape(ann$text),
      '</span>'
    )
    
    last_pos <- ann_end_in_seg + 1
  }
  
  # Add remaining text
  if (last_pos <= nchar(segment_text)) {
    remaining_text <- substr(segment_text, last_pos, nchar(segment_text))
    segment_html <- paste0(segment_html, htmltools::htmlEscape(remaining_text))
  }
  
  # Wrap segment in a div
  annotated_text_html <- paste0(
    annotated_text_html,
    '<div class="text-segment" data-segment-idx="', seg_idx, '" data-segment-type="', seg_type, '">',
    '<span class="segment-label">', seg_type, '</span>',
    segment_html,
    '</div>'
  )
}

# ========================================
# COMPUTE STATISTICS
# ========================================
total_tv <- length(tv_hierarchy_filtered)
total_v <- sum(sapply(tv_hierarchy_filtered, function(h) length(h$variants)))
total_orphans <- length(orphan_terms)
total_errors <- nrow(segments_csv)

stats_by_cat <- list()
for (i in 1:nrow(segments_csv)) {
  cat_letter <- substr(segments_csv$error_category[i], 1, 1)
  if (is.null(stats_by_cat[[cat_letter]])) stats_by_cat[[cat_letter]] <- 0
  stats_by_cat[[cat_letter]] <- stats_by_cat[[cat_letter]] + 1
}

# ========================================
# HTML HELPER FUNCTIONS
# ========================================

escape_html <- function(text) {
  if (is.null(text) || is.na(text)) return("")
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  return(text)
}

highlight_term_in_text <- function(text, term, highlight_class) {
  if (is.null(text) || text == "" || is.null(term) || term == "") {
    return(escape_html(text))
  }
  
  term_lower <- tolower(term)
  text_lower <- tolower(text)
  pos <- regexpr(term_lower, text_lower, fixed = TRUE)
  
  if (pos == -1) {
    return(escape_html(text))
  }
  
  before <- substr(text, 1, pos - 1)
  matched <- substr(text, pos, pos + attr(pos, "match.length") - 1)
  after <- substr(text, pos + attr(pos, "match.length"), nchar(text))
  
  paste0(escape_html(before),
         '<span class="', highlight_class, '">', escape_html(matched), '</span>',
         escape_html(after))
}

build_single_error_html <- function(error) {
  cat_color <- color_map[[error$category_letter]]
  if (is.null(cat_color)) cat_color <- "#999"
  
  level_html <- ""
  if (!is.null(error$error_level) && error$error_level != "") {
    level_html <- paste0('<span class="error-level">Level ', error$error_level, '</span>')
  }
  
  comment_html <- ""
  if (!is.null(error$comment) && error$comment != "") {
    comment_html <- paste0('<div class="error-comment">', escape_html(error$comment), '</div>')
  }
  
  paste0(
    '<div class="single-error" style="border-left-color: ', cat_color, ';">',
    '<div class="error-header-row">',
    '<span class="error-code" style="background: ', cat_color, ';">', error$error_category, '</span>',
    level_html,
    '</div>',
    comment_html,
    '</div>'
  )
}

build_occurrence_html <- function(occ, occ_index) {
  term_id <- occ$term_id
  english_term <- occ$english_term
  french_text <- if (length(occ$errors) > 0) occ$errors[[1]]$french_translation else ""
  
  locate_btn <- paste0('<button class="locate-btn-small" onclick="locateInText(&#39;', term_id, '&#39;)">Locate</button>')
  
  # Build context - show full segments EN and FR
  context_html <- ""
  context_en <- if (!is.null(occ$context_en)) occ$context_en else ""
  context_fr <- if (!is.null(occ$context_fr)) occ$context_fr else ""
  
  if (context_en != "" || context_fr != "") {
    en_highlighted <- highlight_term_in_text(context_en, english_term, "highlight-en")
    fr_highlighted <- highlight_term_in_text(context_fr, french_text, "highlight-fr")
    
    context_html <- paste0(
      '<div class="context-display">',
      '<div class="context-row"><span class="context-label ctx-en">EN</span>',
      '<div class="context-text">', en_highlighted, '</div></div>',
      '<div class="context-row"><span class="context-label ctx-fr">FR</span>',
      '<div class="context-text">', fr_highlighted, '</div></div>',
      '</div>'
    )
  }
  
  # Build errors
  errors_html <- ""
  for (err in occ$errors) {
    errors_html <- paste0(errors_html, build_single_error_html(err))
  }
  
  n_errors <- length(occ$errors)
  
  paste0(
    '<div class="occurrence-card">',
    '<div class="occurrence-header" onclick="toggleOccurrence(this.parentElement)">',
    '<span class="occurrence-index">Occ. ', occ_index, '</span>',
    '<span class="occurrence-translation">', escape_html(french_text), '</span>',
    '<span class="occurrence-error-count">', n_errors, ' error(s)</span>',
    locate_btn,
    '<span class="expand-icon">+</span>',
    '</div>',
    '<div class="occurrence-content">',
    context_html,
    '<div class="errors-list">', errors_html, '</div>',
    '</div>',
    '</div>'
  )
}

build_occurrences_list_html <- function(occurrences) {
  if (length(occurrences) == 0) {
    return('<div class="no-errors">No errors annotated</div>')
  }
  html <- ""
  for (i in seq_along(occurrences)) {
    html <- paste0(html, build_occurrence_html(occurrences[[i]], i))
  }
  return(html)
}

# ========================================
# BUILD HTML COMPONENTS
# ========================================
cat("Building HTML components...\n")

# TOC
toc_html <- ""
for (bt_name in names(tv_hierarchy_filtered)) {
  safe_id <- gsub("[^a-zA-Z0-9]", "-", bt_name)
  toc_html <- paste0(toc_html, '<a href="#tv-', safe_id, '" class="toc-item">', escape_html(bt_name), '</a>')
}

# Hierarchy HTML
hierarchy_html <- ""
for (bt_name in names(tv_hierarchy_filtered)) {
  h <- tv_hierarchy_filtered[[bt_name]]
  safe_id <- gsub("[^a-zA-Z0-9]", "-", bt_name)
  
  n_base_occ <- length(h$base_occurrences)
  bt_occurrences_html <- build_occurrences_list_html(h$base_occurrences)
  
  variants_html <- ""
  n_variants <- length(h$variants)
  
  for (v in h$variants) {
    n_v_occ <- length(v$occurrences)
    v_occurrences_html <- build_occurrences_list_html(v$occurrences)
    
    variants_html <- paste0(variants_html,
                            '<div class="variant-card">',
                            '<div class="variant-header" onclick="toggleVariant(this.parentElement)">',
                            '<span class="variant-text">', escape_html(v$text), '</span>',
                            '<span class="variation-type">', escape_html(v$variation_type), '</span>',
                            '<span class="variant-occ-count">', n_v_occ, ' occ.</span>',
                            '<span class="expand-icon">+</span>',
                            '</div>',
                            '<div class="variant-content">', v_occurrences_html, '</div>',
                            '</div>'
    )
  }
  
  # Add drop zones
  variants_html <- paste0(variants_html,
                          '<div class="drop-zone-variants" data-base-term="', escape_html(bt_name), '">',
                          '<span class="drop-hint">Drop orphan here to add as variant</span></div>'
  )
  
  hierarchy_html <- paste0(hierarchy_html,
                           '<div class="tv-block" id="tv-', safe_id, '" data-base-term="', escape_html(bt_name), '" ',
                           'ondragover="handleDragOver(event)" ondragleave="handleDragLeave(event)" ondrop="handleDrop(event)">',
                           '<div class="tv-header" onclick="toggleTV(this.parentElement)">',
                           '<span class="tv-badge">TV</span>',
                           '<span class="tv-text">', escape_html(bt_name), '</span>',
                           '<span class="tv-counts"><span class="base-occ-count">', n_base_occ, '</span> occ. | <span class="variant-count">', n_variants, '</span> var.</span>',
                           '<span class="expand-icon">v</span>',
                           '</div>',
                           '<div class="tv-content">',
                           '<div class="section-block">',
                           '<div class="section-header" onclick="toggleSection(this.parentElement)">',
                           '<span class="section-title">Errors for Base Term</span>',
                           '<span class="section-count base-section-count">', n_base_occ, ' occurrence(s)</span>',
                           '<span class="expand-icon">v</span></div>',
                           '<div class="section-content expanded base-term-container">', bt_occurrences_html,
                           '<div class="drop-zone-base" data-base-term="', escape_html(bt_name), '">',
                           '<span class="drop-hint">Drop TV orphan here to merge</span></div>',
                           '</div></div>',
                           '<div class="section-block">',
                           '<div class="section-header" onclick="toggleSection(this.parentElement)">',
                           '<span class="section-title">Variants with Errors</span>',
                           '<span class="section-count variant-section-count">', n_variants, ' variant(s)</span>',
                           '<span class="expand-icon">v</span></div>',
                           '<div class="section-content expanded variants-container">', variants_html, '</div></div>',
                           '</div></div>'
  )
}

# Orphan HTML
orphans_html <- ""
orphan_data_list <- list()

for (term_key in names(orphan_terms)) {
  o <- orphan_terms[[term_key]]
  safe_id <- gsub("[^a-zA-Z0-9]", "-", term_key)
  
  type_badge <- if(o$term_type == "TV") "TV" else "V"
  type_color <- if(o$term_type == "TV") "#2196F3" else "#9c27b0"
  
  n_occ <- length(o$occurrences)
  occurrences_html <- build_occurrences_list_html(o$occurrences)
  
  orphan_data_list[[term_key]] <- list(
    english_term = o$english_term,
    term_type = o$term_type,
    occurrences = o$occurrences
  )
  
  orphans_html <- paste0(orphans_html,
                         '<div class="orphan-block" id="orphan-', safe_id, '" draggable="true" ',
                         'data-orphan-key="', escape_html(term_key), '" ',
                         'data-orphan-term="', escape_html(o$english_term), '" ',
                         'data-orphan-type="', o$term_type, '" ',
                         'ondragstart="handleDragStart(event)" ondragend="handleDragEnd(event)">',
                         '<div class="orphan-header" onclick="toggleOrphan(this.parentElement)">',
                         '<span class="drag-handle">&#9776;</span>',
                         '<span class="orphan-badge" style="background: ', type_color, ';">', type_badge, '</span>',
                         '<span class="orphan-text">', escape_html(o$english_term), '</span>',
                         '<span class="orphan-occ-count">', n_occ, ' occurrence(s)</span>',
                         '<span class="expand-icon">v</span>',
                         '</div>',
                         '<div class="orphan-content">', occurrences_html, '</div>',
                         '</div>'
  )
}

orphan_section_html <- ""
if (length(orphan_terms) > 0) {
  orphan_section_html <- paste0(
    '<div class="orphan-section" id="orphan-section">',
    '<div class="orphan-section-header">',
    '<h3>Orphan Terms (drag to assign)</h3>',
    '<span class="orphan-count" id="orphan-count">', length(orphan_terms), ' term(s)</span>',
    '</div>',
    '<div class="orphan-list" id="orphan-list">', orphans_html, '</div>',
    '</div>'
  )
}

# Stats bars
stats_bars_html <- ""
category_names <- list(
  "A" = "Lexical", "B" = "Terminological", "C" = "Syntactic", "D" = "Grammatical",
  "E" = "Semantic", "F" = "Textual", "G" = "Style", "H" = "Register", "I" = "No Error"
)

for (cat_letter in names(color_map)) {
  count <- ifelse(is.null(stats_by_cat[[cat_letter]]), 0, stats_by_cat[[cat_letter]])
  if (count > 0) {
    pct <- round(count / total_errors * 100, 1)
    cat_name <- category_names[[cat_letter]]
    stats_bars_html <- paste0(stats_bars_html,
                              '<div class="stat-bar-container">',
                              '<div class="stat-bar-label">', cat_letter, ' - ', cat_name, ' (', count, ')</div>',
                              '<div class="stat-bar-bg">',
                              '<div class="stat-bar-fill" style="width: ', pct, '%; background: ', color_map[[cat_letter]], ';"></div>',
                              '</div></div>'
    )
  }
}

# JSON data for JavaScript
original_tv_v_json <- toJSON(tv_v_links, auto_unbox = TRUE)
orphan_data_json <- toJSON(orphan_data_list, auto_unbox = TRUE)

# ========================================
# BUILD FINAL HTML
# ========================================
cat("Building final HTML...\n")

html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Terminology Viewer - V10 Final</title>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); min-height: 100vh; }
.main-container { max-width: 1600px; margin: 0 auto; padding: 20px; }
.header { background: white; padding: 25px 30px; border-radius: 15px 15px 0 0; box-shadow: 0 4px 20px rgba(0,0,0,0.1); }
h1 { font-size: 26px; color: #333; margin-bottom: 5px; }
.subtitle { color: #666; font-size: 13px; }
.tabs { display: flex; gap: 5px; margin-top: 20px; }
.tab-btn { padding: 12px 24px; border: none; background: #e0e0e0; border-radius: 8px 8px 0 0; cursor: pointer; font-size: 14px; font-weight: 600; color: #666; }
.tab-btn:hover { background: #d0d0d0; }
.tab-btn.active { background: #667eea; color: white; }
.tab-content { display: none; background: white; border-radius: 0 15px 15px 15px; box-shadow: 0 10px 40px rgba(0,0,0,0.2); }
.tab-content.active { display: block; }
.action-bar { background: #f0f4ff; padding: 12px 20px; border-bottom: 2px solid #667eea; display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
.action-btn { padding: 8px 16px; border: none; border-radius: 6px; cursor: pointer; font-size: 13px; font-weight: 600; }
.action-btn.export { background: #4caf50; color: white; }
.action-btn.export:hover { background: #43a047; }
.action-btn.export:disabled { background: #ccc; cursor: not-allowed; }
.action-btn.reset { background: #f44336; color: white; }
.action-btn.reset:hover { background: #e53935; }
.action-btn.reset:disabled { background: #ccc; cursor: not-allowed; }
.action-btn.undo { background: #ff9800; color: white; }
.action-btn.undo:hover { background: #fb8c00; }
.action-btn.undo:disabled { background: #ccc; cursor: not-allowed; }
.changes-indicator { padding: 6px 12px; background: #fff3cd; border-radius: 15px; font-size: 12px; color: #856404; display: none; }
.changes-indicator.visible { display: inline-block; }
.toc-container { background: #f8f9fa; padding: 15px 20px; border-bottom: 2px solid #e0e0e0; }
.toc-title { font-size: 14px; font-weight: 600; color: #333; margin-bottom: 10px; }
.toc-items { display: flex; flex-wrap: wrap; gap: 8px; }
.toc-item { display: inline-block; padding: 6px 12px; background: white; border: 2px solid #667eea; border-radius: 20px; color: #667eea; text-decoration: none; font-size: 12px; font-weight: 500; }
.toc-item:hover { background: #667eea; color: white; }
.search-box { margin-top: 12px; }
.search-box input { width: 100%; max-width: 400px; padding: 8px 15px; border: 2px solid #ddd; border-radius: 20px; font-size: 13px; outline: none; }
.search-box input:focus { border-color: #667eea; }
.hierarchy-content { padding: 20px; max-height: 700px; overflow-y: auto; }
.tv-block { background: white; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-left: 5px solid #2196F3; overflow: hidden; }
.tv-block.hidden { display: none; }
.tv-block.drag-over { border-left-color: #4caf50; box-shadow: 0 0 15px rgba(76, 175, 80, 0.5); }
.tv-header { padding: 15px 20px; cursor: pointer; display: flex; align-items: center; gap: 12px; background: #e3f2fd; }
.tv-header:hover { background: #bbdefb; }
.tv-badge { background: #2196F3; color: white; padding: 4px 10px; border-radius: 5px; font-size: 12px; font-weight: bold; }
.tv-text { flex: 1; font-weight: 600; font-size: 16px; color: #1565c0; }
.tv-counts { background: #bbdefb; padding: 4px 12px; border-radius: 15px; font-size: 12px; color: #1565c0; }
.expand-icon { color: #1565c0; font-size: 12px; font-weight: bold; }
.tv-block.expanded .tv-header .expand-icon { transform: rotate(180deg); }
.tv-content { display: none; padding: 15px; background: #fafafa; }
.tv-block.expanded .tv-content { display: block; }
.section-block { background: white; border-radius: 8px; margin-bottom: 12px; border: 1px solid #e0e0e0; overflow: hidden; }
.section-header { padding: 12px 15px; cursor: pointer; display: flex; align-items: center; gap: 10px; background: #f5f5f5; }
.section-header:hover { background: #eeeeee; }
.section-title { flex: 1; font-weight: 600; font-size: 14px; color: #555; }
.section-count { background: #e0e0e0; padding: 3px 10px; border-radius: 12px; font-size: 11px; color: #666; }
.section-block.collapsed .section-content { display: none; }
.section-content { padding: 12px; }
.drop-zone-variants, .drop-zone-base { border: 2px dashed #ccc; border-radius: 8px; padding: 15px; text-align: center; margin-top: 10px; }
.drop-zone-variants.drag-over, .drop-zone-base.drag-over { border-color: #4caf50; background: #e8f5e9; }
.drop-zone-base { border-color: #2196F3; background: #f5f9ff; }
.drop-zone-base.drag-over { border-color: #1976D2; background: #e3f2fd; }
.drop-hint { color: #999; font-size: 12px; font-style: italic; }
.merged-occurrences { background: #e8f5e9; border-radius: 8px; padding: 12px; margin-bottom: 10px; border-left: 4px solid #4caf50; }
.merged-header { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; padding-bottom: 8px; border-bottom: 1px solid #c8e6c9; }
.merged-badge { background: #4caf50; color: white; padding: 4px 10px; border-radius: 12px; font-size: 11px; font-weight: 600; }
.remove-merged-btn, .remove-variant-btn { padding: 3px 8px; background: #f44336; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 10px; margin-left: auto; }
.variant-card { background: #fafafa; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid #9c27b0; overflow: hidden; }
.variant-card.newly-added { border-left-color: #4caf50; animation: highlight-new 2s ease-out; }
@keyframes highlight-new { 0%, 50% { background: #e8f5e9; } 100% { background: #fafafa; } }
.variant-header { padding: 12px 15px; cursor: pointer; display: flex; align-items: center; gap: 10px; }
.variant-header:hover { background: #f0f0f0; }
.variant-text { flex: 1; font-size: 14px; color: #333; font-weight: 500; }
.variation-type { background: #f3e5f5; color: #7b1fa2; padding: 3px 10px; border-radius: 12px; font-size: 11px; font-weight: 500; }
.variation-type.editable { cursor: pointer; background: #fff3e0; color: #e65100; }
.variant-occ-count { background: #e1bee7; padding: 3px 8px; border-radius: 10px; font-size: 10px; color: #7b1fa2; }
.variant-card .expand-icon { color: #9c27b0; }
.variant-card.collapsed .variant-content { display: none; }
.variant-content { padding: 12px; background: white; }
.occurrence-card { background: white; border-radius: 6px; margin-bottom: 8px; border: 1px solid #e0e0e0; overflow: hidden; }
.occurrence-header { padding: 10px 12px; cursor: pointer; display: flex; align-items: center; gap: 8px; background: #fafafa; }
.occurrence-header:hover { background: #f0f0f0; }
.occurrence-index { background: #667eea; color: white; padding: 2px 8px; border-radius: 4px; font-size: 10px; font-weight: bold; }
.occurrence-translation { flex: 1; font-size: 13px; color: #333; font-style: italic; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.occurrence-error-count { background: #e0e0e0; padding: 2px 8px; border-radius: 10px; font-size: 10px; color: #666; }
.occurrence-card .expand-icon { color: #667eea; font-size: 12px; }
.occurrence-card.collapsed .occurrence-content { display: none; }
.occurrence-content { padding: 10px; background: #fafafa; }
.context-display { background: white; border-radius: 8px; padding: 12px; margin-bottom: 12px; border: 1px solid #e0e0e0; }
.context-row { display: flex; gap: 10px; margin-bottom: 8px; align-items: flex-start; }
.context-row:last-child { margin-bottom: 0; }
.context-label { flex-shrink: 0; width: 28px; height: 20px; display: flex; align-items: center; justify-content: center; border-radius: 4px; font-size: 10px; font-weight: bold; color: white; }
.ctx-en { background: #2196F3; }
.ctx-fr { background: #9c27b0; }
.context-text { flex: 1; font-size: 12px; line-height: 1.6; color: #333; }
.highlight-en { background: #bbdefb; padding: 1px 4px; border-radius: 3px; font-weight: 600; border-bottom: 2px solid #2196F3; }
.highlight-fr { background: #e1bee7; padding: 1px 4px; border-radius: 3px; font-weight: 600; border-bottom: 2px solid #9c27b0; }
.errors-list { margin-top: 10px; padding-top: 10px; border-top: 1px dashed #ddd; }
.single-error { background: white; padding: 8px 12px; border-radius: 4px; margin-bottom: 6px; border-left: 3px solid #999; }
.error-header-row { display: flex; align-items: center; gap: 8px; }
.error-code { color: white; padding: 2px 8px; border-radius: 4px; font-size: 10px; font-weight: bold; }
.error-level { background: #e0e0e0; padding: 2px 6px; border-radius: 8px; font-size: 9px; color: #666; }
.error-comment { font-size: 11px; color: #666; padding: 6px 10px; background: #fff3cd; border-radius: 4px; margin-top: 6px; }
.no-errors, .no-variants { color: #999; font-style: italic; font-size: 13px; padding: 10px; background: #f5f5f5; border-radius: 5px; }
.locate-btn-small { padding: 3px 8px; background: #667eea; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 10px; }
.locate-btn-small:hover { background: #5a6fd6; }
.orphan-section { margin-top: 30px; padding-top: 20px; border-top: 3px dashed #ff9800; }
.orphan-section-header { display: flex; align-items: center; gap: 15px; margin-bottom: 15px; }
.orphan-section-header h3 { font-size: 16px; color: #ff9800; }
.orphan-count { background: #fff3e0; color: #ff9800; padding: 4px 12px; border-radius: 15px; font-size: 12px; }
.orphan-block { background: white; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-left: 5px solid #ff9800; overflow: hidden; cursor: grab; }
.orphan-block:active { cursor: grabbing; }
.orphan-block.dragging { opacity: 0.5; transform: scale(0.95); }
.orphan-header { padding: 15px 20px; cursor: pointer; display: flex; align-items: center; gap: 12px; background: #fff3e0; }
.orphan-header:hover { background: #ffe0b2; }
.drag-handle { color: #ff9800; font-size: 16px; cursor: grab; }
.orphan-badge { color: white; padding: 4px 10px; border-radius: 5px; font-size: 12px; font-weight: bold; }
.orphan-text { flex: 1; font-weight: 600; font-size: 16px; color: #e65100; }
.orphan-occ-count { background: #ffe0b2; padding: 4px 12px; border-radius: 15px; font-size: 12px; color: #e65100; }
.orphan-block .expand-icon { color: #e65100; }
.orphan-block.expanded .orphan-header .expand-icon { transform: rotate(180deg); }
.orphan-content { display: none; padding: 15px; background: #fafafa; }
.orphan-block.expanded .orphan-content { display: block; }
.modal-overlay { display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; justify-content: center; align-items: center; }
.modal-overlay.active { display: flex; }
.modal-content { background: white; padding: 25px; border-radius: 15px; max-width: 400px; width: 90%; }
.modal-content h3 { margin-bottom: 15px; color: #333; }
.modal-content input { width: 100%; padding: 10px 15px; border: 2px solid #ddd; border-radius: 8px; font-size: 14px; margin-bottom: 15px; }
.modal-content input:focus { border-color: #667eea; outline: none; }
.modal-buttons { display: flex; gap: 10px; justify-content: flex-end; }
.modal-btn { padding: 10px 20px; border: none; border-radius: 6px; cursor: pointer; font-weight: 600; }
.modal-btn.cancel { background: #e0e0e0; color: #666; }
.modal-btn.confirm { background: #667eea; color: white; }
.stats-panel { padding: 25px; }
.stats-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 15px; margin-bottom: 25px; }
.stat-box { background: #f8f9fa; padding: 20px; border-radius: 10px; text-align: center; }
.stat-number { font-size: 32px; font-weight: bold; color: #667eea; }
.stat-label { font-size: 12px; color: #666; text-transform: uppercase; }
.stat-bar-container { margin-bottom: 10px; }
.stat-bar-label { font-size: 13px; color: #555; margin-bottom: 4px; }
.stat-bar-bg { background: #e0e0e0; height: 20px; border-radius: 10px; overflow: hidden; }
.stat-bar-fill { height: 100%; border-radius: 10px; }
.text-view-panel { padding: 20px; }
.text-view-controls { background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; }
.search-filter-row { display: flex; gap: 15px; align-items: flex-start; margin-bottom: 15px; }
.search-filter-row input { flex: 1; padding: 10px 15px; border: 2px solid #ddd; border-radius: 8px; font-size: 14px; }
.search-filter-row input:focus { border-color: #667eea; outline: none; }
.filters-section { display: flex; gap: 20px; flex-wrap: wrap; }
.filter-group { display: flex; flex-direction: column; gap: 8px; }
.filter-group-title { font-size: 12px; font-weight: 600; color: #666; text-transform: uppercase; }
.filter-checkboxes { display: flex; gap: 8px; flex-wrap: wrap; }
.filter-checkbox { display: flex; align-items: center; gap: 5px; padding: 5px 10px; background: white; border: 2px solid #ddd; border-radius: 6px; cursor: pointer; font-size: 12px; }
.filter-checkbox input { cursor: pointer; }
.filter-checkbox:hover { border-color: #667eea; }
.filter-checkbox.active { border-color: #667eea; background: #f0f4ff; }
.text-content { background: white; padding: 25px; border-radius: 10px; border: 1px solid #e0e0e0; max-height: 600px; overflow-y: auto; line-height: 1.8; font-size: 14px; }
.text-segment { margin-bottom: 20px; padding: 15px; background: #fafafa; border-radius: 8px; border-left: 4px solid #ddd; position: relative; }
.text-segment[data-segment-type="EN"] { border-left-color: #2196F3; background: #f0f7ff; }
.text-segment[data-segment-type="FR"] { border-left-color: #9c27b0; background: #f9f0ff; }
.segment-label { position: absolute; top: 5px; right: 10px; background: rgba(0,0,0,0.6); color: white; padding: 2px 8px; border-radius: 4px; font-size: 10px; font-weight: bold; }
.term-annotation, .error-annotation { padding: 2px 4px; border-radius: 3px; cursor: pointer; position: relative; display: inline-block; }
.term-annotation:hover, .error-annotation:hover { opacity: 0.9; transform: translateY(-1px); }
.term-annotation.search-highlight, .error-annotation.search-highlight { box-shadow: 0 0 0 3px #ffff00; animation: search-pulse 0.5s ease-in-out; }
@keyframes search-pulse { 0%, 100% { box-shadow: 0 0 0 3px #ffff00; } 50% { box-shadow: 0 0 0 5px #ffff00; } }
.ann-label { display: inline-block; background: rgba(0,0,0,0.7); color: white; padding: 1px 4px; border-radius: 3px; font-size: 9px; font-weight: bold; margin-right: 3px; vertical-align: super; }
.highlighted-annotation { animation: pulse 1s ease-in-out 3; }
@keyframes pulse { 0%, 100% { transform: scale(1); } 50% { transform: scale(1.05); box-shadow: 0 0 10px rgba(102, 126, 234, 0.5); } }
.footer-stats { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px 20px; border-radius: 0 0 15px 15px; font-size: 13px; }
.footer-stats strong { color: #ffd700; }
</style>
</head>
<body>
<div class="main-container">
<div class="header">
<h1>Terminology and Translation Error Viewer</h1>
<div class="tabs">
<button class="tab-btn active" onclick="switchTab(event, \'hierarchy\')">TV-Variant Hierarchy</button>
<button class="tab-btn" onclick="switchTab(event, \'text\')">Text View</button>
<button class="tab-btn" onclick="switchTab(event, \'stats\')">Statistics</button>
</div>
</div>

<div class="tab-content active" id="tab-hierarchy">
<div class="action-bar">
<button class="action-btn export" id="export-btn" onclick="exportChanges()" disabled>Export Updated CSV</button>
<button class="action-btn undo" id="undo-btn" onclick="undoLastChange()" disabled>Undo Last</button>
<button class="action-btn reset" id="reset-btn" onclick="resetAllChanges()" disabled>Reset All</button>
<span class="changes-indicator" id="changes-indicator">0 changes</span>
</div>

<div class="toc-container">
<div class="toc-title">Table of Contents (', length(tv_hierarchy_filtered), ' Base Terms)</div>
<div class="toc-items">', toc_html, '</div>
<div class="search-box">
<input type="text" id="hierarchy-search" placeholder="Search terms..." onkeyup="filterHierarchy()">
</div>
</div>

<div class="hierarchy-content" id="hierarchy-content">
', hierarchy_html, '
', orphan_section_html, '
</div>
</div>

<div class="tab-content" id="tab-text">
<div class="text-view-panel">
<div class="text-view-controls">
<div class="search-filter-row">
<input type="text" id="text-search" placeholder="Search in text..." onkeyup="searchInText()">
</div>
<div class="filters-section">
<div class="filter-group">
<div class="filter-group-title">Terms</div>
<div class="filter-checkboxes">
<label class="filter-checkbox active" data-filter="TV">
<input type="checkbox" checked onchange="toggleFilter(this, \'TV\')"> TV
</label>
<label class="filter-checkbox active" data-filter="V">
<input type="checkbox" checked onchange="toggleFilter(this, \'V\')"> V
</label>
</div>
</div>
<div class="filter-group">
<div class="filter-group-title">Error Categories</div>
<div class="filter-checkboxes">
<label class="filter-checkbox active" data-filter="A">
<input type="checkbox" checked onchange="toggleFilter(this, \'A\')"> A - Lexical
</label>
<label class="filter-checkbox active" data-filter="B">
<input type="checkbox" checked onchange="toggleFilter(this, \'B\')"> B - Terminological
</label>
<label class="filter-checkbox active" data-filter="C">
<input type="checkbox" checked onchange="toggleFilter(this, \'C\')"> C - Syntactic
</label>
<label class="filter-checkbox active" data-filter="D">
<input type="checkbox" checked onchange="toggleFilter(this, \'D\')"> D - Grammatical
</label>
<label class="filter-checkbox active" data-filter="E">
<input type="checkbox" checked onchange="toggleFilter(this, \'E\')"> E - Semantic
</label>
<label class="filter-checkbox active" data-filter="F">
<input type="checkbox" checked onchange="toggleFilter(this, \'F\')"> F - Textual
</label>
<label class="filter-checkbox active" data-filter="G">
<input type="checkbox" checked onchange="toggleFilter(this, \'G\')"> G - Style
</label>
<label class="filter-checkbox active" data-filter="H">
<input type="checkbox" checked onchange="toggleFilter(this, \'H\')"> H - Register
</label>
<label class="filter-checkbox active" data-filter="I">
<input type="checkbox" checked onchange="toggleFilter(this, \'I\')"> I - No Error
</label>
</div>
</div>
</div>
</div>
<div class="text-content" id="text-content">
', annotated_text_html, '
</div>
</div>
</div>

<div class="tab-content" id="tab-stats">
<div class="stats-panel">
<div class="stats-grid">
<div class="stat-box"><div class="stat-number">', total_tv, '</div><div class="stat-label">Base Terms</div></div>
<div class="stat-box"><div class="stat-number" style="color: #9c27b0;">', total_v, '</div><div class="stat-label">Variants</div></div>
<div class="stat-box"><div class="stat-number" style="color: #ff9800;">', total_orphans, '</div><div class="stat-label">Orphan Terms</div></div>
<div class="stat-box"><div class="stat-number" style="color: #e6a21b;">', total_errors, '</div><div class="stat-label">Total Errors</div></div>
</div>
<h3 style="margin-bottom: 15px; font-size: 16px;">Error Distribution</h3>
<div style="max-width: 600px;">', stats_bars_html, '</div>
</div>
</div>

<div class="footer-stats">
<strong>Summary:</strong> ', total_tv, ' base terms | ', total_v, ' variants | ', total_orphans, ' orphan terms | ', total_errors, ' total errors
</div>
</div>

<div class="modal-overlay" id="variation-modal">
<div class="modal-content">
<h3>Add as Variant</h3>
<p style="margin-bottom: 15px; color: #666; font-size: 13px;">Adding "<span id="modal-variant-name"></span>" to "<span id="modal-base-name"></span>"</p>
<input type="text" id="variation-type-input" placeholder="e.g., VL extension, VG synonyme...">
<div class="modal-buttons">
<button class="modal-btn cancel" onclick="cancelModal()">Cancel</button>
<button class="modal-btn confirm" onclick="confirmModal()">Confirm</button>
</div>
</div>
</div>

<script>
var originalTVVData = ', original_tv_v_json, ';
var orphanData = ', orphan_data_json, ';
var changes = [];
var pendingDrop = null;
var activeFilters = new Set(["TV", "V", "A", "B", "C", "D", "E", "F", "G", "H", "I"]);

function loadSavedChanges() {
  var saved = localStorage.getItem("tv_v_changes_v10");
  if (saved) {
    try {
      changes = JSON.parse(saved);
      changes.forEach(function(change) { applyChangeToDOM(change); });
      updateUI();
    } catch (e) { changes = []; }
  }
}

function saveChanges() { localStorage.setItem("tv_v_changes_v10", JSON.stringify(changes)); }

function updateUI() {
  var hasChanges = changes.length > 0;
  document.getElementById("export-btn").disabled = !hasChanges;
  document.getElementById("reset-btn").disabled = !hasChanges;
  document.getElementById("undo-btn").disabled = !hasChanges;
  var indicator = document.getElementById("changes-indicator");
  indicator.textContent = changes.length + " change(s)";
  indicator.classList.toggle("visible", hasChanges);
  var orphanList = document.getElementById("orphan-list");
  if (orphanList) {
    var remaining = orphanList.querySelectorAll(".orphan-block").length;
    var countEl = document.getElementById("orphan-count");
    if (countEl) countEl.textContent = remaining + " term(s)";
  }
}

function switchTab(event, tabName) {
  document.querySelectorAll(".tab-btn").forEach(function(btn) { btn.classList.remove("active"); });
  document.querySelectorAll(".tab-content").forEach(function(c) { c.classList.remove("active"); });
  event.target.classList.add("active");
  document.getElementById("tab-" + tabName).classList.add("active");
}

function showAnnotationDetails(annId, label, text) {
  event.stopPropagation();
  alert("Annotation: " + label + "\\nText: " + text + "\\nID: " + annId);
}

function toggleFilter(checkbox, category) {
  var label = checkbox.closest(".filter-checkbox");
  
  if (checkbox.checked) {
    activeFilters.add(category);
    label.classList.add("active");
  } else {
    activeFilters.delete(category);
    label.classList.remove("active");
  }
  
  applyFilters();
}

function applyFilters() {
  var segments = document.querySelectorAll(".text-segment");
  
  segments.forEach(function(seg) {
    var annotations = seg.querySelectorAll(".term-annotation, .error-annotation");
    var hasVisibleAnnotation = false;
    
    annotations.forEach(function(ann) {
      var category = ann.dataset.category;
      if (activeFilters.has(category)) {
        ann.style.display = "inline-block";
        hasVisibleAnnotation = true;
      } else {
        ann.style.display = "none";
      }
    });
    
    // Show segment if it has at least one visible annotation
    seg.style.opacity = hasVisibleAnnotation ? "1" : "0.3";
  });
}

function toggleTV(block) { block.classList.toggle("expanded"); }
function toggleOrphan(block) { if (!block.classList.contains("dragging")) block.classList.toggle("expanded"); }
function toggleSection(block) { event.stopPropagation(); block.classList.toggle("collapsed"); }
function toggleVariant(card) { event.stopPropagation(); card.classList.toggle("collapsed"); }
function toggleOccurrence(card) { event.stopPropagation(); card.classList.toggle("collapsed"); }

function filterHierarchy() {
  var query = document.getElementById("hierarchy-search").value.toLowerCase().trim();
  document.querySelectorAll(".tv-block, .orphan-block").forEach(function(block) {
    block.classList.toggle("hidden", query && !block.textContent.toLowerCase().includes(query));
  });
}

function searchInText() {
  var query = document.getElementById("text-search").value.trim();
  var textContent = document.getElementById("text-content");
  var segments = textContent.querySelectorAll(".text-segment");
  
  if (!query) {
    // Show all segments if search is empty
    segments.forEach(function(seg) {
      seg.style.display = "block";
    });
    return;
  }
  
  var queryLower = query.toLowerCase();
  var matchingSegmentIndices = new Set();
  
  // Find all segments that contain the search term
  segments.forEach(function(seg) {
    var segIdx = parseInt(seg.dataset.segmentIdx);
    var annotations = seg.querySelectorAll(".term-annotation, .error-annotation");
    var hasMatch = false;
    
    // Check if any annotation in this segment matches
    annotations.forEach(function(ann) {
      var annText = ann.dataset.text || ann.textContent.toLowerCase();
      if (annText.includes(queryLower)) {
        hasMatch = true;
        ann.classList.add("search-highlight");
      } else {
        ann.classList.remove("search-highlight");
      }
    });
    
    if (hasMatch) {
      matchingSegmentIndices.add(segIdx);
      // Also add the paired segment (EN/FR)
      if (segIdx % 2 === 1) {
        matchingSegmentIndices.add(segIdx + 1);
      } else {
        matchingSegmentIndices.add(segIdx - 1);
      }
    }
  });
  
  // Show/hide segments based on matches
  var firstVisibleSegment = null;
  segments.forEach(function(seg) {
    var segIdx = parseInt(seg.dataset.segmentIdx);
    if (matchingSegmentIndices.has(segIdx)) {
      seg.style.display = "block";
      if (!firstVisibleSegment) firstVisibleSegment = seg;
    } else {
      seg.style.display = "none";
    }
  });
  
  // Scroll to first visible segment
  if (firstVisibleSegment) {
    firstVisibleSegment.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

function locateInText(termId) {
  event.stopPropagation();
  
  // Switch to text view tab
  var textTab = Array.from(document.querySelectorAll(".tab-btn")).find(function(btn) {
    return btn.textContent.includes("Text View");
  });
  if (textTab) textTab.click();
  
  // Find and highlight the annotation
  var annotation = document.getElementById("ann-" + termId);
  if (annotation) {
    annotation.scrollIntoView({ behavior: "smooth", block: "center" });
    annotation.classList.add("highlighted-annotation");
    setTimeout(function() {
      annotation.classList.remove("highlighted-annotation");
    }, 3000);
  } else {
    alert("Annotation not found in text view (ID: " + termId + ")");
  }
}

function handleDragStart(event) {
  var orphanBlock = event.target.closest(".orphan-block");
  if (!orphanBlock) return;
  orphanBlock.classList.add("dragging");
  event.dataTransfer.setData("text/plain", orphanBlock.dataset.orphanKey);
  event.dataTransfer.effectAllowed = "move";
}

function handleDragEnd(event) {
  var orphanBlock = event.target.closest(".orphan-block");
  if (orphanBlock) orphanBlock.classList.remove("dragging");
  document.querySelectorAll(".tv-block").forEach(function(b) { b.classList.remove("drag-over"); });
  document.querySelectorAll(".drop-zone-variants, .drop-zone-base").forEach(function(z) { z.classList.remove("drag-over"); });
}

function handleDragOver(event) {
  event.preventDefault();
  event.dataTransfer.dropEffect = "move";
  var tvBlock = event.target.closest(".tv-block");
  if (tvBlock) tvBlock.classList.add("drag-over");
  var dropZone = event.target.closest(".drop-zone-variants, .drop-zone-base");
  if (dropZone) dropZone.classList.add("drag-over");
}

function handleDragLeave(event) {
  var tvBlock = event.target.closest(".tv-block");
  if (tvBlock && !tvBlock.contains(event.relatedTarget)) tvBlock.classList.remove("drag-over");
  var dropZone = event.target.closest(".drop-zone-variants, .drop-zone-base");
  if (dropZone && !dropZone.contains(event.relatedTarget)) dropZone.classList.remove("drag-over");
}

function handleDrop(event) {
  event.preventDefault();
  var tvBlock = event.target.closest(".tv-block");
  if (!tvBlock) return;
  tvBlock.classList.remove("drag-over");
  document.querySelectorAll(".drop-zone-base, .drop-zone-variants").forEach(function(z) { z.classList.remove("drag-over"); });
  
  var orphanKey = event.dataTransfer.getData("text/plain");
  var baseTerm = tvBlock.dataset.baseTerm;
  if (!orphanKey || !baseTerm) return;
  
  var orphanBlock = document.querySelector(".orphan-block[data-orphan-key=\\"" + orphanKey + "\\"]");
  if (!orphanBlock) return;
  
  var variantTerm = orphanBlock.dataset.orphanTerm;
  var orphanType = orphanBlock.dataset.orphanType;
  
  var dropZoneBase = event.target.closest(".drop-zone-base");
  if (dropZoneBase) {
    var change = { type: "merge_tv", baseTerm: baseTerm, mergedTerm: variantTerm, orphanKey: orphanKey, orphanType: orphanType };
    changes.push(change);
    saveChanges();
    applyChangeToDOM(change);
    updateUI();
    return;
  }
  
  pendingDrop = { orphanKey: orphanKey, orphanBlock: orphanBlock, baseTerm: baseTerm, variantTerm: variantTerm, orphanType: orphanType, tvBlock: tvBlock };
  document.getElementById("modal-variant-name").textContent = variantTerm;
  document.getElementById("modal-base-name").textContent = baseTerm;
  document.getElementById("variation-type-input").value = "";
  document.getElementById("variation-modal").classList.add("active");
  document.getElementById("variation-type-input").focus();
}

function cancelModal() {
  document.getElementById("variation-modal").classList.remove("active");
  pendingDrop = null;
}

function confirmModal() {
  if (!pendingDrop) return;
  var variationType = document.getElementById("variation-type-input").value.trim() || "[non specifie]";
  var change = { type: "add_variant", baseTerm: pendingDrop.baseTerm, variantTerm: pendingDrop.variantTerm, variationType: variationType, orphanKey: pendingDrop.orphanKey, orphanType: pendingDrop.orphanType };
  changes.push(change);
  saveChanges();
  applyChangeToDOM(change);
  document.getElementById("variation-modal").classList.remove("active");
  pendingDrop = null;
  updateUI();
}

function applyChangeToDOM(change) {
  var orphanBlock = document.querySelector(".orphan-block[data-orphan-key=\\"" + change.orphanKey + "\\"]");
  var tvBlock = document.querySelector(".tv-block[data-base-term=\\"" + change.baseTerm + "\\"]");
  if (!tvBlock) return;
  
  var occurrencesHTML = "";
  if (orphanBlock) {
    var content = orphanBlock.querySelector(".orphan-content");
    if (content) occurrencesHTML = content.innerHTML;
  }
  
  if (change.type === "add_variant") {
    var variantCard = document.createElement("div");
    variantCard.className = "variant-card newly-added";
    variantCard.innerHTML = "<div class=\\"variant-header\\" onclick=\\"toggleVariant(this.parentElement)\\"><span class=\\"variant-text\\">" + escapeHtml(change.variantTerm) + "</span><span class=\\"variation-type editable\\" onclick=\\"editVariationType(event, this, \\x27" + escapeHtml(change.baseTerm) + "\\x27, \\x27" + escapeHtml(change.variantTerm) + "\\x27)\\">" + escapeHtml(change.variationType) + "</span><button class=\\"remove-variant-btn\\" onclick=\\"removeAddedVariant(event, \\x27" + escapeHtml(change.baseTerm) + "\\x27, \\x27" + escapeHtml(change.variantTerm) + "\\x27)\\">Remove</button><span class=\\"expand-icon\\">+</span></div><div class=\\"variant-content\\">" + occurrencesHTML + "</div>";
    
    var variantsContainer = tvBlock.querySelector(".variants-container");
    var dropZone = variantsContainer.querySelector(".drop-zone-variants");
    variantsContainer.insertBefore(variantCard, dropZone);
    
    var variantCount = tvBlock.querySelector(".variant-count");
    var variantSectionCount = tvBlock.querySelector(".variant-section-count");
    var currentCount = parseInt(variantCount.textContent) || 0;
    variantCount.textContent = currentCount + 1;
    variantSectionCount.textContent = (currentCount + 1) + " variant(s)";
  }
  
  if (change.type === "merge_tv") {
    var baseTermContainer = tvBlock.querySelector(".base-term-container");
    if (baseTermContainer) {
      var dropZoneBase = baseTermContainer.querySelector(".drop-zone-base");
      var mergedDiv = document.createElement("div");
      mergedDiv.className = "merged-occurrences";
      mergedDiv.dataset.mergedTerm = change.mergedTerm;
      mergedDiv.innerHTML = "<div class=\\"merged-header\\"><span class=\\"merged-badge\\">Merged: " + escapeHtml(change.mergedTerm) + "</span><button class=\\"remove-merged-btn\\" onclick=\\"removeMergedTerm(event, \\x27" + escapeHtml(change.baseTerm) + "\\x27, \\x27" + escapeHtml(change.mergedTerm) + "\\x27)\\">Remove</button></div>" + occurrencesHTML;
      if (dropZoneBase) baseTermContainer.insertBefore(mergedDiv, dropZoneBase);
      else baseTermContainer.appendChild(mergedDiv);
    }
  }
  
  if (orphanBlock) orphanBlock.remove();
  tvBlock.classList.add("expanded");
}

function removeAddedVariant(event, baseTerm, variantTerm) {
  event.stopPropagation();
  if (!confirm("Remove this variant?")) return;
  var idx = changes.findIndex(function(c) { return c.type === "add_variant" && c.baseTerm === baseTerm && c.variantTerm === variantTerm; });
  if (idx === -1) return;
  var change = changes[idx];
  changes.splice(idx, 1);
  saveChanges();
  var tvBlock = document.querySelector(".tv-block[data-base-term=\\"" + baseTerm + "\\"]");
  if (tvBlock) {
    tvBlock.querySelectorAll(".variant-card").forEach(function(card) {
      if (card.querySelector(".variant-text").textContent === variantTerm) {
        restoreOrphanBlock(change);
        var variantCount = tvBlock.querySelector(".variant-count");
        var variantSectionCount = tvBlock.querySelector(".variant-section-count");
        var currentCount = parseInt(variantCount.textContent) || 0;
        variantCount.textContent = Math.max(0, currentCount - 1);
        variantSectionCount.textContent = Math.max(0, currentCount - 1) + " variant(s)";
        card.remove();
      }
    });
  }
  updateUI();
}

function removeMergedTerm(event, baseTerm, mergedTerm) {
  event.stopPropagation();
  if (!confirm("Remove this merged term?")) return;
  var idx = changes.findIndex(function(c) { return c.type === "merge_tv" && c.baseTerm === baseTerm && c.mergedTerm === mergedTerm; });
  if (idx === -1) return;
  var change = changes[idx];
  changes.splice(idx, 1);
  saveChanges();
  var tvBlock = document.querySelector(".tv-block[data-base-term=\\"" + baseTerm + "\\"]");
  if (tvBlock) {
    tvBlock.querySelectorAll(".merged-occurrences").forEach(function(section) {
      if (section.dataset.mergedTerm === mergedTerm) {
        restoreOrphanBlock(change);
        section.remove();
      }
    });
  }
  updateUI();
}

function restoreOrphanBlock(change) {
  var orphanList = document.getElementById("orphan-list");
  if (!orphanList || !orphanData[change.orphanKey]) return;
  var data = orphanData[change.orphanKey];
  var safeId = change.orphanKey.replace(/[^a-zA-Z0-9]/g, "-");
  var typeColor = data.term_type === "TV" ? "#2196F3" : "#9c27b0";
  var orphanBlock = document.createElement("div");
  orphanBlock.className = "orphan-block";
  orphanBlock.id = "orphan-" + safeId;
  orphanBlock.draggable = true;
  orphanBlock.dataset.orphanKey = change.orphanKey;
  orphanBlock.dataset.orphanTerm = data.english_term;
  orphanBlock.dataset.orphanType = data.term_type;
  orphanBlock.ondragstart = handleDragStart;
  orphanBlock.ondragend = handleDragEnd;
  orphanBlock.innerHTML = "<div class=\\"orphan-header\\" onclick=\\"toggleOrphan(this.parentElement)\\"><span class=\\"drag-handle\\">&#9776;</span><span class=\\"orphan-badge\\" style=\\"background: " + typeColor + ";\\">" + data.term_type + "</span><span class=\\"orphan-text\\">" + escapeHtml(data.english_term) + "</span><span class=\\"orphan-occ-count\\">" + data.occurrences.length + " occ.</span><span class=\\"expand-icon\\">v</span></div><div class=\\"orphan-content\\"><div class=\\"no-errors\\">Restored</div></div>";
  orphanList.appendChild(orphanBlock);
}

function editVariationType(event, element, baseTerm, variantTerm) {
  event.stopPropagation();
  var newType = prompt("Enter variation type:", element.textContent);
  if (newType === null) return;
  element.textContent = newType || "[non specifie]";
  var change = changes.find(function(c) { return c.type === "add_variant" && c.baseTerm === baseTerm && c.variantTerm === variantTerm; });
  if (change) { change.variationType = newType || "[non specifie]"; saveChanges(); }
}

function undoLastChange() {
  if (changes.length === 0) return;
  var lastChange = changes.pop();
  saveChanges();
  if (lastChange.type === "add_variant") {
    var tvBlock = document.querySelector(".tv-block[data-base-term=\\"" + lastChange.baseTerm + "\\"]");
    if (tvBlock) {
      tvBlock.querySelectorAll(".variant-card").forEach(function(card) {
        if (card.querySelector(".variant-text").textContent === lastChange.variantTerm) {
          restoreOrphanBlock(lastChange);
          var variantCount = tvBlock.querySelector(".variant-count");
          var variantSectionCount = tvBlock.querySelector(".variant-section-count");
          var currentCount = parseInt(variantCount.textContent) || 0;
          variantCount.textContent = Math.max(0, currentCount - 1);
          variantSectionCount.textContent = Math.max(0, currentCount - 1) + " variant(s)";
          card.remove();
        }
      });
    }
  }
  if (lastChange.type === "merge_tv") {
    var tvBlock = document.querySelector(".tv-block[data-base-term=\\"" + lastChange.baseTerm + "\\"]");
    if (tvBlock) {
      tvBlock.querySelectorAll(".merged-occurrences").forEach(function(section) {
        if (section.dataset.mergedTerm === lastChange.mergedTerm) {
          restoreOrphanBlock(lastChange);
          section.remove();
        }
      });
    }
  }
  updateUI();
}

function resetAllChanges() {
  if (!confirm("Reset all changes?")) return;
  localStorage.removeItem("tv_v_changes_v10");
  location.reload();
}

function exportChanges() {
  var csvContent = "base_term;variant;variation_type\\n";
  originalTVVData.forEach(function(row) {
    csvContent += escapeCSV(row.base_term) + ";" + escapeCSV(row.variant) + ";" + escapeCSV(row.variation_type) + "\\n";
  });
  changes.forEach(function(change) {
    if (change.type === "add_variant") {
      csvContent += escapeCSV(change.baseTerm) + ";" + escapeCSV(change.variantTerm) + ";" + escapeCSV(change.variationType) + "\\n";
    }
  });
  var blob = new Blob([csvContent], { type: "text/csv;charset=utf-8;" });
  var link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = "Texte1_Extraction_updated.csv";
  link.click();
}

function escapeCSV(str) {
  if (!str) return "";
  if (str.indexOf(";") !== -1 || str.indexOf("\\"") !== -1 || str.indexOf("\\n") !== -1) {
    return \'"\' + str.replace(/"/g, \'""\') + \'"\';
  }
  return str;
}

function escapeHtml(text) {
  var div = document.createElement("div");
  div.textContent = text;
  return div.innerHTML;
}

document.querySelectorAll(".toc-item").forEach(function(link) {
  link.addEventListener("click", function(e) {
    e.preventDefault();
    var target = document.getElementById(this.getAttribute("href").substring(1));
    if (target) {
      target.classList.add("expanded");
      target.scrollIntoView({ behavior: "smooth", block: "start" });
    }
  });
});

document.getElementById("variation-type-input").addEventListener("keyup", function(e) {
  if (e.key === "Enter") confirmModal();
  if (e.key === "Escape") cancelModal();
});

loadSavedChanges();
</script>
</body>
</html>')

con <- file(output_file, "w", encoding = "UTF-8")
writeLines(html_content, con)
close(con)

cat("\n==============================================\n")
cat("SUCCESS!\n")
cat("==============================================\n")
cat("Output file:", output_file, "\n\n")
cat("Features:\n")
cat("  - Full segments from CSV (english_segment, french_segment)\n")
cat("  - Perfect EN/FR context alignment\n")
cat("  - Text View with search & filtering\n")
cat("  - Interactive annotations\n")
cat("  - All drag-and-drop features\n\n")
cat(sprintf("Results: %d base terms, %d variants, %d orphans, %d errors\n", total_tv, total_v, total_orphans, total_errors))

browseURL(output_file)