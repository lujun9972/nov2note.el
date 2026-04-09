# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`nov2note.el` is an Emacs package that integrates EPUB reading (via the `nov` package) with Org-mode note-taking. It allows users to capture highlights and selections from EPUB files into synchronized Org-mode notes, preserving the book's structure.

## Architecture

### Core Files

- **nov2note.el** - Main package providing navigation and capture functionality
- **nov2note-convent2org.el** - Content conversion utilities (HTML → Org format)

### Key Components

1. **Entry Point**: `nov2note-capture()` - The primary user-facing function that initiates note capture from an EPUB buffer

2. **Navigation System** (`nov2note.el`):
   - Parses EPUB NCX (table of contents) files
   - `nov2note-navpoint2heading()` - Converts navigation points to Org headings
   - `nov2note--get-current-heading-id()` - Creates unique IDs for EPUB sections
   - `nov2note-find-the-location()` - Finds/navigates to corresponding notes in Org file

3. **Content Conversion** (`nov2note-convent2org.el`):
   - `nov2note-convent2org()` - Main conversion pipeline
   - `nov2note-convent2org--hanlder-image()` - Copies images, creates Org links
   - `nov2note-convent2org--hanlder-url()` - Converts URLs to Org links
   - `nov2note-convent2org--handler-face()` - Maps HTML formatting to Org syntax
   - `nov2note-convent2org--handler-stars()` - Handles Org reserved characters

### Integration Points

- Uses `org-capture` for inserting notes into predefined locations
- Stores `NOV2NOTE_ID` property on Org headings for bidirectional linking
- Leverages `nov.el` for EPUB metadata and content access
- Uses `xml` and `xml-query` for parsing EPUB NCX files

## Development Notes

### Emacs Lisp Conventions

- All files use `;; -*- lexical-binding: t; -*-`
- Private functions are prefixed with `nov2note--` (double dash)
- Public functions are prefixed with `nov2note-` (single dash)
- Use `;;;###autoload` for autoloading entry points
- Use `(provide 'feature-name)` for module loading

### Configuration Variables

- `nov2note-directory` - Notes directory (default: ~/Reading)
- `nov2note-capture-template` - Org capture template

### No Build System

This is a pure Emacs Lisp package with no build step, no tests, and no linting configuration. Development involves:
1. Editing `.el` files
2. Loading them in Emacs for testing
3. Using `eval-region` or `load-file` for quick iteration
