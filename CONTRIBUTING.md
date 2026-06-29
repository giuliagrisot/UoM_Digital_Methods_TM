# Contributing

Thank you for helping improve these teaching materials.

This repository is primarily maintained as course material for Text Analysis
with R at the University of Manchester Digital Methods Summer School. Changes
should keep the student experience clear, reproducible, and usable in both local
RStudio and Posit Cloud.

## Workflow

- Keep `main` as the stable course-ready version.
- Create a branch for each meaningful change, for example
  `lesson/sentiment-exercises`, `fix/corpus-import`, or `docs/license-note`.
- Make small commits with clear messages.
- Open a pull request before merging into `main`, even for small changes, when
  the change affects teaching flow, package requirements, or prepared data.
- Before merging, run the edited notebook chunks or companion `.R` script where
  practical.

## What to Commit

Commit source materials:

- Quarto notebooks (`.qmd`)
- R scripts (`.R`)
- small sample texts and metadata
- README, citation, and license files

Avoid committing local or generated files:

- `.Rhistory`
- `.RData` session files
- `.Rproj.user/`
- `Rplots.pdf`
- cache folders
- large regenerated data unless they are intentionally part of the class

## Course Material Principles

- Prefer one clear workflow over several competing workflows.
- Add comments that explain what code does for beginner students.
- Keep processing manageable for Posit Cloud.
- Treat corpus construction, sampling, and preprocessing decisions as
  methodological choices, not neutral setup.
- When adding analyses, include a way for students to return to actual text
  examples.

## Release Practice

Before teaching, create a GitHub release such as `v2026.0` so students and
future users can access a stable version of the materials.
