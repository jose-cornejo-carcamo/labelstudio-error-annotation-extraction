#!/usr/bin/env Rscript
# =============================================================================
# labelstudio-mt-error-annotation
# Extraction of machine translation (MT) error annotations from Label Studio
# =============================================================================
#
# What this script does
#   For each annotated text, it reads the Label Studio JSON export and writes
#   one sheet of an Excel workbook listing every annotated English term
#   (base term "TV" or variant "V"), its terminological family, the French
#   translation span annotated as erroneous, the error category(ies), level,
#   comment, and the aligned source/target segments.
#
# Inputs (one set per text, declared in texts_config below)
#   - json  : Label Studio JSON export of the annotated text (required)
#   - csv   : base term / variant pairs of the text, used to group terms into
#             families (optional). Semicolon-separated, with columns named
#             "base_term" and "variant" (case-insensitive).
#   - excel : aligned bilingual text, English segments in the first column and
#             French segments in the second (optional). Without it, segments
#             are rebuilt from the task text (EN line / FR line / blank line).
#
# Output
#   An Excel workbook with one sheet per text ("Texte 1", "Texte 2", ...).
#
# How to use
#   1. Install the packages below if needed.
#   2. Replace the placeholders between [ ] (look for the "[CHANGE]" comments).
#   3. Run the whole script.
# =============================================================================

# --- Required packages -------------------------------------------------------
# If needed, install them once with:
# install.packages(c("jsonlite", "dplyr", "readxl", "openxlsx"))
# Original title: EXTRACTION TERMINOLOGIQUE UNIFIÉE — v3 (data.frames + dplyr)

library(jsonlite)
library(dplyr)
library(readxl)
library(openxlsx)

# Opérateur null-safe (au cas où rlang n'est pas chargé)
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# CONFIGURATION
# =============================================================================
# [CHANGE IF NEEDED] Folder containing the input files and folder where the
# Excel workbook is written (by default, the working directory).
input_dir  <- getwd()
output_dir <- getwd()

# [CHANGE] One list(...) per annotated text. Replace the placeholders with
# your file names (csv and excel can be set to NULL if not available) and
# give each text a number (num), used to name the sheet ("Texte <num>").
# To process several texts, add one list(...) per text, separated by commas.
texts_config <- list(
  list(json  = "[text1_label_studio_export].json",
       csv   = "[text1_base_term_variant_pairs].csv",
       excel = "[text1_aligned_EN_FR].xlsx",
       num   = 1)
)

# =============================================================================
# FONCTIONS
# =============================================================================

# --- Helper: singular/plural variants of a term, used to match terms that
# differ from the CSV only by number ---------------------------------------
# Variantes singulier/pluriel
plural_variants <- function(txt) {
  v <- c()
  if (grepl("ies$", txt))  v <- c(v, sub("ies$", "y", txt))
  if (grepl("es$", txt))   v <- c(v, sub("es$", "", txt), sub("es$", "e", txt))
  if (grepl("s$", txt))    v <- c(v, sub("s$", "", txt))
  if (!grepl("s$", txt))   v <- c(v, paste0(txt, "s"))
  if (grepl("y$", txt))    v <- c(v, sub("y$", "ies", txt))
  unique(v)
}

# --- Step 1: read the JSON export ------------------------------------------
# Each annotated region becomes one row: id, parentID (link to the region it
# depends on), selected taxonomy values (category and level), text, start/end
# positions, name of the taxonomy group (from_name) and comment (meta text).
# Extraire toutes les annotations JSON en un data.frame plat
parse_annotations <- function(json_path) {
  jd   <- fromJSON(json_path, simplifyVector = FALSE)
  anns <- jd$annotations[[1]]$result
  ft   <- jd$data$text
  
  rows <- lapply(anns, function(a) {
    tax <- a$value$taxonomy
    tax1 <- if (!is.null(tax) && length(tax) > 0 && length(tax[[1]]) > 0) tax[[1]][[1]] else NA_character_
    tax2 <- if (!is.null(tax) && length(tax) > 1 && length(tax[[2]]) > 0) tax[[2]][[1]] else NA_character_
    cmt  <- if (!is.null(a$meta) && !is.null(a$meta$text) && length(a$meta$text) > 0) a$meta$text[[1]] else NA_character_
    
    data.frame(
      ann_id    = a$id %||% NA_character_,
      parent_id = a$parentID %||% NA_character_,
      taxonomy  = tax1,
      tax_level = tax2,
      text      = a$value$text %||% NA_character_,
      start     = a$value$start %||% NA_integer_,
      end       = a$value$end %||% NA_integer_,
      from_name = a$from_name %||% NA_character_,
      comment   = cmt,
      stringsAsFactors = FALSE
    )
  })
  
  df <- bind_rows(rows)
  list(annotations = df, full_text = ft)
}

# --- Segments, option A: rebuilt from the task text, expected to alternate
# English line / French line / blank line ------------------------------------
# Construire l'index de segments depuis le full_text (paires EN/FR)
build_segment_pairs_from_text <- function(full_text) {
  lines <- strsplit(full_text, "\n", fixed = TRUE)[[1]]
  n <- length(lines)
  pairs <- list()
  cum <- 0L
  
  i <- 1L
  while (i <= n) {
    if (nchar(trimws(lines[i])) > 0 && i + 1 <= n) {
      en <- trimws(lines[i])
      fr <- trimws(lines[i + 1])
      en_start <- cum
      en_end   <- cum + nchar(lines[i])
      pairs[[length(pairs) + 1]] <- data.frame(
        seg_en = en, seg_fr = fr,
        seg_start = en_start, seg_end = en_end,
        stringsAsFactors = FALSE
      )
      # Avancer : EN, FR, blanc
      cum <- cum + nchar(lines[i]) + 1L       # après EN
      if (i + 1 <= n) cum <- cum + nchar(lines[i + 1]) + 1L  # après FR
      if (i + 2 <= n) cum <- cum + nchar(lines[i + 2]) + 1L  # après blanc
      i <- i + 3L
    } else {
      cum <- cum + nchar(lines[i]) + 1L
      i <- i + 1L
    }
  }
  bind_rows(pairs)
}

# --- Segments, option B: read from the aligned Excel file (EN in column 1,
# FR in column 2) and located in the task text --------------------------------
# Construire l'index de segments depuis l'Excel aligné
build_segment_pairs_from_excel <- function(full_text, excel_path) {
  df <- read_excel(excel_path)
  en_col <- trimws(as.character(df[[1]]))
  fr_col <- trimws(as.character(df[[2]]))
  
  pairs <- list()
  search_from <- 1L
  
  for (i in seq_along(en_col)) {
    en <- en_col[i]
    fr <- fr_col[i]
    if (is.na(en) || nchar(en) == 0) next
    pos <- regexpr(en, substr(full_text, search_from, nchar(full_text)), fixed = TRUE)
    if (pos > 0) {
      abs_start <- search_from - 1L + pos - 1L  # 0-based
      abs_end   <- abs_start + nchar(en)
      pairs[[length(pairs) + 1]] <- data.frame(
        seg_en = en, seg_fr = if (is.na(fr)) "" else fr,
        seg_start = abs_start, seg_end = abs_end,
        stringsAsFactors = FALSE
      )
      search_from <- search_from + pos + nchar(en) - 1L
    }
  }
  cat(sprintf("  Excel : %d/%d segments retrouvés dans le texte\n", length(pairs), length(en_col)))
  bind_rows(pairs)
}

# --- Finds the aligned segment containing a given position ------------------
# Pour une position donnée, trouver le segment (EN, FR)
find_segment <- function(pos, seg_index) {
  hit <- seg_index %>% filter(seg_start <= pos, pos < seg_end)
  if (nrow(hit) > 0) return(hit[1, ])
  # Fallback : segment le plus proche avant cette position
  before <- seg_index %>% filter(seg_start <= pos) %>% arrange(desc(seg_start))
  if (nrow(before) > 0) return(before[1, ])
  # Dernier recours
  data.frame(seg_en = NA_character_, seg_fr = NA_character_,
             seg_start = NA_integer_, seg_end = NA_integer_,
             stringsAsFactors = FALSE)
}

# =============================================================================
# TRAITEMENT D'UN TEXTE
# =============================================================================

process_text <- function(json_path, csv_path, excel_path, text_num) {
  cat(sprintf("\n%s\nTEXTE %d\n%s\n", strrep("=", 60), text_num, strrep("=", 60)))
  
  # 1. Parser le JSON
  parsed    <- parse_annotations(json_path)
  all_anns  <- parsed$annotations
  full_text <- parsed$full_text
  
  cat(sprintf("  %d annotations\n", nrow(all_anns)))
  
  # [CHANGE IF NEEDED] "TV" and "V" must match the choices of the "english"
  # taxonomy in the labeling interface (base term / variant). Every other
  # annotated region linked to a term through parentID is treated as an error.
  # 2. Séparer : termes racines, enfants composés, erreurs
  termes_racines <- all_anns %>%
    filter(taxonomy %in% c("TV", "V"), is.na(parent_id))
  
  termes_enfants <- all_anns %>%
    filter(taxonomy %in% c("TV", "V"), !is.na(parent_id))
  
  erreurs <- all_anns %>%
    filter(!(taxonomy %in% c("TV", "V")) | is.na(taxonomy), !is.na(parent_id))
  
  cat(sprintf("  Termes racines : %d, Enfants composés : %d, Erreurs : %d\n",
              nrow(termes_racines), nrow(termes_enfants), nrow(erreurs)))
  
  # Discontinuous terms: constituents linked to a root term through parentID
  # are merged. If the gap between constituents is small (5 characters or
  # less), the text between them is kept; otherwise the parts are joined
  # with a space.
  # 3. Fusionner les termes composés
  # Pour chaque racine qui a des enfants, reconstruire le texte complet
  if (nrow(termes_enfants) > 0) {
    # Trouver le texte fusionné pour chaque racine avec enfants
    parents_with_children <- unique(termes_enfants$parent_id)
    
    for (pid in parents_with_children) {
      parent_row <- termes_racines %>% filter(ann_id == pid)
      if (nrow(parent_row) == 0) next
      
      children <- termes_enfants %>% filter(parent_id == pid)
      all_parts <- bind_rows(
        data.frame(text = parent_row$text[1], start = parent_row$start[1],
                   end = parent_row$end[1], stringsAsFactors = FALSE),
        children %>% select(text, start, end)
      ) %>% arrange(start)
      
      merged_start <- min(all_parts$start)
      merged_end   <- max(all_parts$end)
      
      # Vérifier adjacence
      is_adj <- TRUE
      if (nrow(all_parts) > 1) {
        for (k in 1:(nrow(all_parts) - 1)) {
          if (all_parts$start[k + 1] - all_parts$end[k] > 5) {
            is_adj <- FALSE; break
          }
        }
      }
      
      merged_text <- if (is_adj) {
        substr(full_text, merged_start + 1, merged_end)  # R 1-based
      } else {
        paste(all_parts$text, collapse = " ")
      }
      
      # Mettre à jour la racine
      termes_racines$text[termes_racines$ann_id == pid]  <- merged_text
      termes_racines$start[termes_racines$ann_id == pid] <- merged_start
      termes_racines$end[termes_racines$ann_id == pid]   <- merged_end
    }
  }
  
  # Errors attached to a constituent are reassigned to the root term.
  # 4. Remonter les erreurs dont le parent est un enfant composé
  child_to_root <- termes_enfants %>% select(ann_id, parent_id) %>% rename(child_id = ann_id, root_id = parent_id)
  erreurs <- erreurs %>%
    left_join(child_to_root, by = c("parent_id" = "child_id")) %>%
    mutate(parent_id = ifelse(!is.na(root_id), root_id, parent_id)) %>%
    select(-root_id)
  
  # 5. Fusionner TOUTES les erreurs d'une même occurrence en UNE seule ligne
  #    Catégories séparées par " ## ", niveaux/sources/IDs par " ## "
  #    Traduction FR : une seule fois (pas de répétition)
  erreurs_valides <- erreurs %>%
    filter(parent_id %in% termes_racines$ann_id)
  
  if (nrow(erreurs_valides) > 0) {
    # D'abord fusionner les constituantes (même parent + même catégorie) en un seul texte
    err_par_cat <- erreurs_valides %>%
      group_by(parent_id, taxonomy) %>%
      arrange(start) %>%
      summarise(
        trad_parts       = paste(text, collapse = " "),
        niveau_erreur    = { x <- tax_level[!is.na(tax_level)]; if (length(x) > 0) x[1] else NA_character_ },
        source_erreur    = { x <- from_name[!is.na(from_name)]; if (length(x) > 0) x[1] else NA_character_ },
        error_id         = ann_id[1],
        commentaire      = { x <- comment[!is.na(comment)]; if (length(x) > 0) paste(x, collapse = " ## ") else NA_character_ },
        .groups = "drop"
      )
    
    # Puis fusionner toutes les catégories d'une même occurrence
    erreurs_fusionnees <- err_par_cat %>%
      group_by(parent_id) %>%
      arrange(taxonomy) %>%
      summarise(
        traduction_fr    = first(trad_parts),
        categorie_erreur = paste(taxonomy, collapse = " ## "),
        niveau_erreur    = paste(ifelse(is.na(niveau_erreur), "NA", niveau_erreur), collapse = " ## "),
        source_erreur    = paste(ifelse(is.na(source_erreur), "NA", source_erreur), collapse = " ## "),
        error_id         = paste(error_id, collapse = " ## "),
        commentaire      = {
          x <- commentaire[!is.na(commentaire)]
          if (length(x) > 0) paste(x, collapse = " ## ") else NA_character_
        },
        .groups = "drop"
      ) %>%
      rename(term_id = parent_id)
  } else {
    erreurs_fusionnees <- data.frame(
      term_id = character(0),
      traduction_fr = character(0), categorie_erreur = character(0),
      niveau_erreur = character(0), source_erreur = character(0),
      error_id = character(0), commentaire = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  cat(sprintf("  Erreurs fusionnées : %d\n", nrow(erreurs_fusionnees)))
  
  # 6. Index de segments (Excel si dispo, sinon full_text)
  if (!is.null(excel_path) && file.exists(excel_path)) {
    seg_index <- build_segment_pairs_from_excel(full_text, excel_path)
  } else {
    cat("  Pas d'Excel → segments par lignes du texte\n")
    seg_index <- build_segment_pairs_from_text(full_text)
  }
  
  # Attacher le segment à chaque terme
  termes_racines$segment_source     <- NA_character_
  termes_racines$segment_traduction <- NA_character_
  
  for (r in seq_len(nrow(termes_racines))) {
    seg <- find_segment(termes_racines$start[r], seg_index)
    termes_racines$segment_source[r]     <- seg$seg_en[1]
    termes_racines$segment_traduction[r] <- seg$seg_fr[1]
  }
  
  # 7. Familles terminologiques (CSV)
  termes_racines$text_lower <- tolower(trimws(termes_racines$text))
  termes_racines$famille    <- NA_character_
  
    # The CSV is read line by line; semicolons inside quoted fields are kept.
  if (!is.null(csv_path) && file.exists(csv_path)) {
    # Parser manuel : gère les ; à l'intérieur de champs entre guillemets
    raw_lines <- readLines(csv_path, encoding = "UTF-8", warn = FALSE)
    raw_lines <- trimws(raw_lines)
    raw_lines <- raw_lines[nchar(raw_lines) > 0]
    
    parse_line <- function(line) {
      fields <- character(0)
      current <- ""
      in_quote <- FALSE
      chars <- strsplit(line, "")[[1]]
      for (ch in chars) {
        if (ch == '"') {
          in_quote <- !in_quote
        } else if (ch == ';' && !in_quote) {
          fields <- c(fields, current)
          current <- ""
        } else {
          current <- paste0(current, ch)
        }
      }
      fields <- c(fields, current)
      fields
    }
    
    csv_headers <- tolower(trimws(parse_line(raw_lines[1])))
    bt_col <- which(csv_headers == "base_term")
    vt_col <- which(csv_headers == "variant")
    
    if (length(bt_col) == 0 || length(vt_col) == 0) {
      cat(sprintf("  ⚠ Colonnes introuvables. Détectées : %s\n", paste(csv_headers, collapse=", ")))
    } else {
      csv_rows <- lapply(raw_lines[-1], function(l) {
        flds <- parse_line(l)
        bt <- if (bt_col <= length(flds)) trimws(flds[bt_col]) else ""
        vt <- if (vt_col <= length(flds)) trimws(flds[vt_col]) else ""
        data.frame(base_term = bt, variant = vt, stringsAsFactors = FALSE)
      })
      cd <- bind_rows(csv_rows)
      
      cat(sprintf("  CSV : %d lignes, bt_col=%d, vt_col=%d\n", nrow(cd), bt_col, vt_col))
      cat(sprintf("  Premiers base_term : %s\n", paste(head(cd$base_term, 3), collapse=" ## ")))
      cat(sprintf("  Premiers variant   : %s\n", paste(head(cd$variant, 3), collapse=" ## ")))
      
      # Table de mapping : texte_lower -> famille (base_term)
      mapping <- bind_rows(
        cd %>% transmute(text_lower = tolower(trimws(base_term)), famille = trimws(base_term)),
        cd %>% transmute(text_lower = tolower(trimws(variant)),   famille = trimws(base_term))
      ) %>% distinct(text_lower, .keep_all = TRUE)
      
      cat(sprintf("  Mapping : %d entrées\n", nrow(mapping)))
      
      # Jointure exacte
      termes_racines <- termes_racines %>%
        left_join(mapping, by = "text_lower", suffix = c("", "_csv")) %>%
        mutate(famille = ifelse(is.na(famille_csv), famille, famille_csv)) %>%
        select(-famille_csv)
      
      n_mapped <- sum(!is.na(termes_racines$famille))
      cat(sprintf("  Familles (exact) : %d/%d\n", n_mapped, nrow(termes_racines)))
      
      # Rattrapage singulier/pluriel
      non_mappes <- which(is.na(termes_racines$famille))
      for (r in non_mappes) {
        txt <- termes_racines$text_lower[r]
        for (v in plural_variants(txt)) {
          hit <- mapping %>% filter(text_lower == v)
          if (nrow(hit) > 0) {
            termes_racines$famille[r] <- hit$famille[1]
            break
          }
        }
      }
      n_mapped2 <- sum(!is.na(termes_racines$famille))
      cat(sprintf("  Familles (+ pluriel) : %d/%d\n", n_mapped2, nrow(termes_racines)))
    }
  }
  
  # Non mappés → leur propre famille
  termes_racines$famille[is.na(termes_racines$famille)] <-
    termes_racines$text[is.na(termes_racines$famille)]
  
  # 8. Calculer family_order = position de la 1re occurrence TV de chaque famille
  family_order <- termes_racines %>%
    filter(taxonomy == "TV") %>%
    group_by(famille) %>%
    summarise(family_order = min(start), .groups = "drop")
  
  # Pour les familles sans TV (que des V), prendre la 1re position V
  familles_sans_tv <- setdiff(unique(termes_racines$famille), family_order$famille)
  if (length(familles_sans_tv) > 0) {
    fo_v <- termes_racines %>%
      filter(famille %in% familles_sans_tv) %>%
      group_by(famille) %>%
      summarise(family_order = min(start), .groups = "drop")
    family_order <- bind_rows(family_order, fo_v)
  }
  
  termes_racines <- termes_racines %>%
    left_join(family_order, by = "famille")
  
  # Diagnostic
  diag <- termes_racines %>%
    group_by(famille) %>%
    summarise(TV = sum(taxonomy == "TV"), V = sum(taxonomy == "V"), .groups = "drop")
  cat("  Composition des familles :\n")
  for (r in seq_len(nrow(diag))) {
    cat(sprintf("    %-40s TV=%d  V=%d\n", substr(diag$famille[r], 1, 40), diag$TV[r], diag$V[r]))
  }
  
  # 9. Joindre les erreurs aux termes
  result <- termes_racines %>%
    left_join(erreurs_fusionnees, by = c("ann_id" = "term_id"))
  
  # 10. Trier : famille → TV avant V → position
  type_order <- c(TV = 0L, V = 1L)
  result <- result %>%
    mutate(type_rank = type_order[taxonomy]) %>%
    arrange(family_order, type_rank, start) %>%
    select(-type_rank, -text_lower, -family_order, -parent_id, -tax_level,
           -from_name, -comment, -end)
  
  # 11. Colonnes finales dans le bon ordre
  result <- result %>%
    transmute(
      famille,
      type              = taxonomy,
      terme_anglais     = text,
      position          = start,
      traduction_fr     = traduction_fr,
      categorie_erreur  = categorie_erreur,
      niveau_erreur     = niveau_erreur,
      source_erreur     = source_erreur,
      error_id          = error_id,
      commentaire       = commentaire,
      segment_source    = segment_source,
      segment_traduction = segment_traduction
    )
  
  cat(sprintf("  Lignes : %d (%d TV, %d V)\n",
              nrow(result),
              sum(result$type == "TV", na.rm = TRUE),
              sum(result$type == "V", na.rm = TRUE)))
  result
}

# =============================================================================
# ÉCRITURE EXCEL
# =============================================================================

# --- Writes one formatted sheet per text (TV rows in blue, V rows in green,
# a line separating terminological families) -------------------------------
write_xlsx_out <- function(all_results, out_path) {
  wb <- createWorkbook()
  
  hs <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
                    fgFill = "#2F5496", halign = "center", valign = "center",
                    textDecoration = "bold", wrapText = TRUE,
                    border = "TopBottomLeftRight", borderColour = "#B4C6E7")
  tv_s <- createStyle(fontName = "Arial", fontSize = 10, fgFill = "#D6E4F0",
                      valign = "top", wrapText = TRUE,
                      border = "TopBottomLeftRight", borderColour = "#B4C6E7")
  v_s  <- createStyle(fontName = "Arial", fontSize = 10, fgFill = "#E2EFDA",
                      valign = "top", wrapText = TRUE,
                      border = "TopBottomLeftRight", borderColour = "#B4C6E7")
  sep_s <- createStyle(border = "Top", borderColour = "#2F5496",
                       borderStyle = "medium")
  
  col_widths <- c(30, 6, 30, 8, 30, 22, 12, 14, 14, 30, 55, 55)
  
  for (num in names(all_results)) {
    df <- all_results[[num]]
    sn <- paste0("Texte ", num)
    addWorksheet(wb, sn)
    
    # En-têtes
    writeData(wb, sn, df, startRow = 1, colNames = TRUE)
    addStyle(wb, sn, hs, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
    
    if (nrow(df) > 0) {
      # Styles TV / V
      for (r in seq_len(nrow(df))) {
        sty <- if (!is.na(df$type[r]) && df$type[r] == "TV") tv_s else v_s
        addStyle(wb, sn, sty, rows = r + 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      }
      
      # Séparateurs entre familles
      prev <- df$famille[1]
      for (r in 2:nrow(df)) {
        if (!is.na(df$famille[r]) && df$famille[r] != prev) {
          addStyle(wb, sn, sep_s, rows = r + 1, cols = seq_len(ncol(df)),
                   gridExpand = TRUE, stack = TRUE)
        }
        prev <- df$famille[r]
      }
    }
    
    for (c in seq_along(col_widths))
      setColWidths(wb, sn, cols = c, widths = col_widths[c])
    addFilter(wb, sn, rows = 1, cols = seq_len(ncol(df)))
    freezePane(wb, sn, firstRow = TRUE)
  }
  
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat(sprintf("\nExcel : %s\n", out_path))
}

# =============================================================================
# EXÉCUTION
# =============================================================================

all_results <- list()
for (cfg in texts_config) {
  jp <- file.path(input_dir, cfg$json)
  cp <- if (!is.null(cfg$csv))   file.path(input_dir, cfg$csv)   else NULL
  ep <- if (!is.null(cfg$excel)) file.path(input_dir, cfg$excel) else NULL
  if (!file.exists(jp)) { cat(sprintf("IGNORÉ : %s\n", jp)); next }
  all_results[[as.character(cfg$num)]] <- process_text(jp, cp, ep, cfg$num)
}

write_xlsx_out(all_results, file.path(output_dir, "Extraction_Terminologique_Unifiee.xlsx"))

# [CHANGE] Name of the output workbook. Without this call, nothing is written.
write_xlsx_out(all_results, file.path(output_dir, "[MT_error_annotations].xlsx"))
