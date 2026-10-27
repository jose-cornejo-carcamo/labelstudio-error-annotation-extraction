# labelstudio-mt-error-annotation-extraction

R script that extracts machine translation (MT) error annotations made in Label Studio and organises them by terminological family. For each annotated text, it produces an Excel sheet listing every English base term and variant, the French translation span annotated as erroneous, the error category or categories, their level, comments, and the aligned source and target segments.

The annotation itself is done manually in Label Studio; this script only extracts and structures the resulting data from the JSON export files. It was developed to analyse how MT systems translate terms and their intratextual variants in research articles in Earth and Planetary Sciences (English → French). It complements [labelstudio-term-variation]([link to the variation repository]), which extracts the base term/variant pairs used here to group terms into families.

## Repository contents

| File | Content |
|---|---|
| `labeling_interface_errors.xml` | Configuration of the Label Studio labeling interface used for the error annotation |
| `extract_mt_error_annotations.R` | R script that extracts the annotations from the JSON exports |
| `README.md` | This file |

## Requirements

- R
- Packages: `jsonlite`, `dplyr`, `readxl`, `openxlsx`

```r
install.packages(c("jsonlite", "dplyr", "readxl", "openxlsx"))
```

## Labeling interface

`labeling_interface_errors.xml` contains the configuration used in Label Studio. To reuse it, create a project, open **Settings > Labeling Interface**, switch to the **Code** view and paste the content of the file.

Each task contains the aligned bilingual text: an English segment, its French machine translation, then a blank line, and so on. The interface provides a sticky panel with seven taxonomies, all applied to text spans:

| Taxonomy (`name`) | Header in the interface | Choices |
|---|---|---|
| `english` | English annotation | `TV` (base term), `V` (variant) |
| `lexi-termino` | Lexicales et terminologiques | `A1`–`A7`, `B1`–`B7` |
| `synt-struct` | Syntaxiques et structurelles | `C1`–`C3`, `D1`–`D2` |
| `lang-cib` | Adaptation à la langue cible | `E1`–`E2`, `F1`–`F4` |
| `transfert` | Transfert | `G1`–`G3`, `H1`–`H2` |
| `absence` | Absence erreur | `I_non_erreur` |
| `type-anno` | Type annotateur | `J_type_ann` |

The full list of error codes is in the XML file. Each error taxonomy (except `type-anno`) also offers the values `0` to `3`, used to assign a level to the error: [describe the scale].

Annotation workflow:

1. In the English segment, annotate each term with `TV` (base term) or `V` (variant).
2. In the French segment, annotate the erroneous translation span with an error category and a level.
3. Attach the error span to the English term it concerns (parent–child, by drag and drop in the *Regions* panel). This fills the `parentID` field, which the script uses to link errors to terms.
4. Comments can be added to a region; they are exported as `meta.text` and kept in the output.

Discontinuous English terms are annotated constituent by constituent and the constituents are attached to the first one in the same way.

The choice values (`TV`, `V`, error codes) must match those expected by the script. If you rename them in the interface, rename them in the script as well (see the `[CHANGE IF NEEDED]` comments).

## Inputs

For each text, the script expects up to three files, declared in `texts_config` at the beginning of the script:

| Input | Required | Content |
|---|---|---|
| `json` | yes | Label Studio JSON export of the annotated text |
| `csv` | no | Base term/variant pairs of the text, semicolon-separated, with columns named `base_term` and `variant` (case-insensitive). Used to group terms into families. |
| `excel` | no | Aligned bilingual text: English segments in the first column, French segments in the second. Used to retrieve the segment of each term. |

Without the CSV, each term forms its own family. Without the Excel file, segments are rebuilt from the task text, which must then alternate English line, French line and blank line.

The CSV can be derived from the `<file>_terminology_analysis_detailed.csv` output of [labelstudio-term-variation]([link to the variation repository]), after renaming its `Terme de base` and `Variant` columns to `base_term` and `variant`.

## Usage

1. Export each annotated text from Label Studio in JSON format.
2. Open `extract_mt_error_annotations.R` and replace the placeholders between `[ ]`:
   - in `texts_config`, the names of the JSON, CSV and Excel files of each text, and its number (required);
   - the name of the output workbook, in the last line of the script (required);
   - `input_dir` and `output_dir`, if the files are not in the working directory (optional).
3. Run the whole script.

To process several texts at once, add one `list(...)` per text in `texts_config`.

## Output

An Excel workbook with one sheet per text (`Texte 1`, `Texte 2`, …). Rows are sorted by family (in order of first appearance in the text), base terms before variants, then by position. Base term rows are shaded blue, variant rows green, and a line separates families.

| Column | Content |
|---|---|
| `famille` | Terminological family (base term) |
| `type` | `TV` (base term) or `V` (variant) |
| `terme_anglais` | English term as annotated |
| `position` | Start position of the term in the task text |
| `traduction_fr` | French span annotated as erroneous |
| `categorie_erreur` | Error category or categories |
| `niveau_erreur` | Level of each error (0–3) |
| `source_erreur` | Taxonomy group of each error (e.g. `lexi-termino`) |
| `error_id` | Label Studio ID(s) of the error region(s) |
| `commentaire` | Comment(s) on the error region(s) |
| `segment_source` | English segment containing the term |
| `segment_traduction` | Corresponding French segment |

When several errors are annotated on the same term occurrence, they are merged into one row and their values are separated by ` ## `. Terms without any error annotation are also listed, with empty error columns.

## How it works

1. **Reading** the JSON export into a flat table of annotated regions (`parse_annotations`).
2. **Separating** root terms (`TV`/`V` without parent), constituents of discontinuous terms (`TV`/`V` with a parent) and errors (other regions with a parent).
3. **Merging discontinuous terms**: constituents are joined to their root term. If the gap between them is 5 characters or less, the text in between is kept; otherwise the parts are joined with a space.
4. **Reassigning** errors attached to a constituent to the root term.
5. **Merging all the errors of one term occurrence** into a single row (categories, levels, sources, IDs and comments separated by ` ## `).
6. **Retrieving the aligned segments** of each term, from the Excel file if available, otherwise from the task text (`build_segment_pairs_from_excel`, `build_segment_pairs_from_text`, `find_segment`).
7. **Assigning families** from the CSV: exact match first, then singular/plural variants (`plural_variants`). Terms not found in the CSV form their own family.
8. **Ordering families** by the position of their first base term (or first variant if the family has no base term).
9. **Joining** errors to terms.
10. **Sorting** by family, type and position, and selecting the final columns.
11. **Writing** the formatted Excel workbook (`write_xlsx_out`).

The script prints diagnostic information for each text (number of annotations, errors, mapped families, family composition) to help check the extraction.
