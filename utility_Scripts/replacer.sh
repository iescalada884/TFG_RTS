#!/bin/bash

# Usage: ./replacer /path/to/BASE_FOLDER /path/to/to_replace file1 file2 ...

BASE_FOLDER="$1"
TO_REPLACE_FOLDER="$2"
shift 2
FILES_TO_CHECK=("$@")

BACKUP_FOLDER="$BASE_FOLDER/replaced"

# Validate input
if [[ ! -d "$BASE_FOLDER" ]]; then
  echo "Error: Target folder '$BASE_FOLDER' not found."
  exit 1
fi

if [[ ! -d "$TO_REPLACE_FOLDER" ]]; then
  echo "Error: Replacement folder '$TO_REPLACE_FOLDER' not found."
  exit 1
fi

mkdir -p "$BACKUP_FOLDER"

for base_name in "${FILES_TO_CHECK[@]}"; do
  mapfile -t target_matches < <(find "$BASE_FOLDER" -maxdepth 1 -type f -name "${base_name}.*")
  mapfile -t replacement_matches < <(find "$TO_REPLACE_FOLDER" -maxdepth 1 -type f -name "${base_name}.*")

  if (( ${#replacement_matches[@]} == 0 )); then
    echo "Notice: No replacement files found for '$base_name' in '$TO_REPLACE_FOLDER'."
    continue
  fi

  if (( ${#target_matches[@]} == 0 )); then
    echo "Notice: No target files found for '$base_name' in '$BASE_FOLDER'."
    continue
  fi

  for target_file in "${target_matches[@]}"; do
    file_ext="${target_file##*.}"
    replacement_file="$TO_REPLACE_FOLDER/${base_name}.${file_ext}"

    if [[ ! -f "$replacement_file" ]]; then
      echo "Notice: No matching replacement for '$target_file'."
      continue
    fi

    if cmp -s "$replacement_file" "$target_file"; then
      echo "Identical: '$target_file' — no action needed."
    else
      rel_path="${target_file#$BASE_FOLDER/}"
      echo "Replacing: '$rel_path'"
      mv "$target_file" "$BACKUP_FOLDER"
      mkdir -p "$(dirname "$target_file")"
      cp "$replacement_file" "$target_file"
    fi
  done
done
