library(stringr)
library(purrr)
library(dplyr)
library(refinr) # install.packages("refinr") if not already installed

# old method
# getUniqueNames <- function(str) {
#   str <- str |>
#     na.omit() |>
#     str_replace_all(" and ", ",") |>
#     str_replace_all(" & ", ",") |>
#     str_replace_all(" - ", ",") |>
#     str_replace_all("_\\d+\\s?", "") |>
#     paste(collapse = ", ") |>
#     str_split_1(",") |>
#     sapply(str_trim) |>
#     sapply(str_to_title) |>
#     unique() |>
#     sort()
#   str[nzchar(str)]
# }

# ---------------------------------------------------------------------------
# Parses ONE raw name-field string into one or more "First Last" names.
# Handles:
#   1. "First Last"
#   2. "Last, First"
#   3. "First1 + First2 Last"      (separators: +, &, and/And; shared last name)
#   4. "First Last/First Last"
#   5. "First Last, First Last"
# ---------------------------------------------------------------------------
parse_name_field <- function(x) {
  x <- str_squish(x)
  if (is.na(x) || x == "") {
    return(character(0))
  }

  # Matches "+", "&", or the standalone word "and"/"And" as a separator
  multi_sep <- regex("\\s*(\\+|&|\\band\\b)\\s*", ignore_case = TRUE)

  # Case 3: "First1 + First2 Last" / "First & Last" / "First and Last", etc.
  # A chunk with 2-3 words is already a complete name and is kept as-is.
  # A chunk with just 1 word is a bare first name that borrows the last
  # name from the final (complete) chunk.
  if (str_detect(x, multi_sep)) {
    parts <- str_split(x, multi_sep)[[1]] |> str_squish()
    parts <- parts[parts != ""]

    last_chunk <- str_split(parts[length(parts)], "\\s+")[[1]]
    shared_last <- last_chunk[length(last_chunk)]

    names_out <- map_chr(parts, function(p) {
      w <- str_split(p, "\\s+")[[1]]
      if (length(w) >= 2) {
        p # already a full name -> keep as-is
      } else {
        paste(w, shared_last) # bare first name -> attach shared last name
      }
    })
    return(names_out)
  }

  # Case 4: "First Last/First Last"
  if (str_detect(x, "/")) {
    parts <- str_split(x, "/")[[1]] |> str_squish()
    return(parts)
  }

  # Case 2 & 5: comma-separated
  if (str_detect(x, ",")) {
    parts <- str_split(x, ",")[[1]] |> str_squish()
    n_words <- map_int(parts, ~ length(str_split(.x, "\\s+")[[1]]))

    if (length(parts) == 2 && all(n_words == 1)) {
      # Case 2: "Last, First" -> single person
      return(paste(parts[2], parts[1]))
    } else {
      # Case 5: "First Last, First Last, ..." -> multiple people
      return(parts)
    }
  }

  # Case 1: "First Last"
  x
}

# ---------------------------------------------------------------------------
# Cleanup: strip underscores/digits, collapse whitespace, Title Case
# ---------------------------------------------------------------------------
clean_names_vec <- function(names_vec) {
  names_vec |>
    str_replace_all("_", " ") |>
    str_remove_all("[0-9]") |>
    str_squish() |>
    str_to_title()
}

# ---------------------------------------------------------------------------
# Fuzzy consolidation for near-duplicate names that survive exact-match
# cleanup, e.g. "R.c. Tripp", "R.c.trupp", "Rc Tripp", "R C Tripp",
# "R.c Trupp" -> all one person, despite the Tripp/Trupp typo.
#
# key_collision_merge() handles punctuation/spacing/word-order noise.
# n_gram_merge() then catches small typos (edit-distance based).
# Order matters less here since key_collision_merge is mostly a no-op
# once names are already Title Case with no punctuation, but running both
# maximizes the chance of catching everything.
#
# NOTE: this is a heuristic. It picks whichever variant is most common as
# the canonical spelling, and can occasionally over-merge two different
# people with similar names. Spot-check the result if precision matters,
# and tighten/loosen edit_threshold (n_gram_merge default max = 1) as needed.
# ---------------------------------------------------------------------------
consolidate_similar_names <- function(names_vec, edit_threshold = 2) {
  names_vec |>
    key_collision_merge() |>
    n_gram_merge(edit_threshold = edit_threshold) |>
    unique() |>
    sort()
}

# ---------------------------------------------------------------------------
# Main entry point: raw messy name vector -> sorted unique clean names
# Use inside summarise()/reframe() for a per-group result, or wrap in
# list() inside mutate() to broadcast the group's unique-name vector to
# every row of that group.
#
# Set fuzzy = TRUE to additionally merge near-duplicate spellings/typos
# (e.g. "Tripp" vs "Trupp") via consolidate_similar_names().
# ---------------------------------------------------------------------------
get_unique_names <- function(raw_names, fuzzy = TRUE, edit_threshold = 2) {
  cleaned <- raw_names |>
    discard(is.na) |>
    map(parse_name_field) |>
    unlist() |>
    clean_names_vec() |>
    unique() |>
    sort()

  if (fuzzy) {
    cleaned <- consolidate_similar_names(
      cleaned,
      edit_threshold = edit_threshold
    )
  }

  cleaned
}
