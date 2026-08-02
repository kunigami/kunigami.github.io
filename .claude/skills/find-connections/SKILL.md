---
name: find-connections
description: >-
  Find connections between a draft blog post or book summary (provided as a
  file) and the existing corpus of posts and book summaries, and flag concepts
  that already have a cheat sheet. Use when the user points at a file under
  blog/_posts/ or _books/ and asks to find related/previous posts and books,
  surface cross-link opportunities, or check which concepts are covered in the
  _docs cheat sheets.
---

# Find Connections

Given an input file (a draft blog post or a book summary), discover how it
relates to the rest of this blog and produce concrete cross-linking
suggestions. Three outputs:

1. **Related posts** — earlier posts that share concepts/themes.
2. **Related book summaries** — book summaries that share concepts/themes.
3. **Cheat-sheet concepts** — concepts in the input that already have a cheat
   sheet under `_docs/`, so the author can link to or stay consistent with them.

## Corpus locations

- Posts: `/Users/kunigami/github/blog-draft-posts/blog/_posts/` (~280 files,
  `YYYY-MM-DD-<slug>.md`)
- Book summaries: `/Users/kunigami/github/blog-draft-posts/_books/`
  (`<vanity>.md`)
- Cheat sheets: `/Users/kunigami/github/blog-draft-posts/_docs/` (organized in
  topic subfolders: `math/`, `cpp/`, `python/`, …; each file is `layout: doc`
  with a title like `"X Cheat Sheet"`)

## Procedure

### 1. Read and characterize the input

Read the input file fully. Extract:

- **Title** and **tags** (from frontmatter). For a post, also read the
  **date** from the filename (`YYYY-MM-DD`) and the `vanity`.
- **Key concepts** — a list of the substantive terms: named theorems/algorithms,
  technical terms, mathematical objects, tools/libraries, people, and domain
  topics. Skip generic words. This list drives all searches, so be thorough
  (aim for ~10–25 terms, including synonyms/aliases, e.g. "eigenvalue" ↔
  "eigenvector", "DAG" ↔ "directed acyclic graph").

**Search for the concrete nouns, not the abstract topic.** The author writes
about specifics; the abstraction is usually your summary of his point, and it
often appears nowhere in the corpus. So for every theme in the input, also list
the proper nouns that theme would be *expressed* through — tool and product
names, book titles, author surnames, library and function names — and grep those
too.

Real example: for a post about relying on AI instead of textbooks, searching
`textbook`, `self-study`, and `reading habit` found nothing, because the relevant
posts say **"ChatGPT"** and **"Ahlfors"**. The whole cluster (*The Weierstrass
℘-Function*, *[Book] Complex Analysis*, *Functionals*, *Sobolev Spaces*) was
missed until `grep -ri chatgpt` surfaced it — one of those posts lists ChatGPT as
its only reference.

Names worth adding to the concept list whenever the input touches AI, learning,
or tooling: `chatgpt`, `claude`, `codex`, `copilot`, `gpt-`, `llm`, `vibe.cod`.
For a math or CS post, add the surnames of the authors whose books he's working
through (`ahlfors`, `cummings`, `susskind`, …). Reference lists (`* [N] …` at the
end of posts) are a good place to harvest these.

### 2. Find related posts and book summaries

Search the corpus for the key concepts with local `grep` (or `rg`) over
`blog/_posts/` and `_books/` — one search per term/alias. Also match on shared
**tags**.

**Search full post bodies, not just tags/titles — this is where the best
connections come from.** The strongest links are often posts whose tag/title
gives no hint of the overlap; only a body-text search finds them. Real example:
the Delaunay post's best match was "On Lifetime", tagged `opinion`, whose body
happens to discuss triangulation and convex-hull algorithms — a tag/title-only
search would have missed it entirely. So treat tags as a secondary signal and
always grep the content.

> Tooling note: do NOT use the `search_files` MCP tool or any Meta
> infrastructure here — this is a plain local git repo, not a Biggrep-onboarded
> one, so `search_files` errors out. Use `grep -rl -i "<term>" blog/_posts
> _books` to find candidate files, then read excerpts to confirm. A case-
> insensitive search that ORs a few aliases is efficient, e.g.
> `grep -rl -iE "map-?reduce|mapreduce" blog/_posts _books`.

Then **confirm** candidates — a keyword hit is not a connection. Read the
relevant excerpt of each candidate and keep only those with a genuine
conceptual/thematic relationship. Note *why* they connect (shared theorem,
prerequisite concept, same tool, contrasting approach, continuation of a
series, etc.).

Chronology: when the input is a post, prefer **earlier** posts ("previous"
posts the new one can reference). Still surface relevant later posts, but label
them as such. Book summaries have no date — include any that fit.

Rank by strength of connection (shared core concept > shared tag > incidental
mention). Useful helpers (all local, no external infra):

- Titles of candidates: `grep -h "^title:" blog/_posts/<file>.md`
- Posts sharing a tag: `grep -rl "tags:.*<tag>" blog/_posts`

### 3. Flag cheat-sheet concepts

For each key concept, check whether `_docs/` has a matching cheat sheet —
match against cheat-sheet **titles** and **section headings** (`##`/`###`), not
just filenames. List all titles with `grep -rh "^title:" _docs`, find a file
with `grep -rl -i "<term>" _docs`, and inspect its headings with
`grep -E "^#{2,3} " <file>`. Report every concept in the input that is also
covered by a cheat sheet, with the cheat-sheet file and the specific section, so
the author can cross-link or align terminology/notation. A topic-level tag match
with no overlapping section is a weak flag — say so rather than overstating it.

### 4. Produce the report

Output three sections. For each related item include the connection reason and
a ready-to-paste link in this blog's link style.

**Link formats:**

- Post `blog/_posts/YYYY-MM-DD-<slug>.md` →
  `[Title]({{blog}}/YYYY/MM/DD/<slug>.html)` (slash-separated date; e.g.
  `2026-06-01-velox-vectors.md` → `{{blog}}/2026/06/01/velox-vectors.html`).
- Book `_books/<vanity>.md` → `[Title]({{site.url}}/books/<vanity>.html)`.
- Cheat sheet `_docs/<topic>/<name>.md` → `{{site.url}}/docs/<topic>/<name>.html`
  (verify the exact URL against `_config.yml` / an existing link if linking).

Report shape:

```
## Related posts
- **<Title>** — <one-line why> · <link>

## Related book summaries
- **<Title>** — <one-line why> · <link>

## Cheat-sheet concepts to flag
- **<concept>** → `_docs/<topic>/<file>.md` (section "<heading>")
```

Keep connection reasons to one line. If a section has no hits, say so
explicitly rather than padding with weak matches. Surface the strongest
connections first; it's fine to cap each list at the ~10 most relevant and note
how many were dropped.

## Notes

- Don't link a post to itself, and don't propose a post that doesn't exist —
  every suggestion must correspond to a real file you found.
- Be conservative: a shared keyword in passing is not a connection. When unsure,
  read more of the candidate before including it.
