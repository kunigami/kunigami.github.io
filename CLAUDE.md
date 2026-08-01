# NP-Incompleteness Blog

Jekyll-based blog hosted at https://www.kuniga.me.

## URL to file mapping

A URL like `https://www.kuniga.me/blog/2025/04/16/residue-theorem.html` maps to:

    blog/_posts/2025-04-16-residue-theorem.md

The pattern is: `blog/_posts/YYYY-MM-DD-<slug>.md` where the URL path `/blog/YYYY/MM/DD/<slug>.html` uses `/` separators for the date while the filename uses `-`.

## Post frontmatter

Each post has YAML frontmatter:

```yaml
layout: post
title: "The Residue Theorem"
tags: [analysis]
vanity: "2025-04-16-residue-theorem"
```

The `vanity` field must match the `YYYY-MM-DD-<slug>` portion of the filename (and URL). It is used to locate resources (images, code) for the post.

## Key directories

- `blog/_posts/` - blog posts (Markdown)
- `resources/blog/<vanity>/` - images and assets for a post (keyed by `vanity`)
- `blog/code/<vanity>/` - code files for a post
- `_includes/` - Jekyll includes (e.g. `blog_vars.html` sets up resource paths)
- `_layouts/` - Jekyll layouts
- `_docs/`, `_books/`, `_amuseum/`, `_nature/` - other Jekyll collections

## Editing posts

When asked to fix typos in a post, correct **only** misspellings, grammar (agreement,
tense, missing words), and punctuation. Do **not** rephrase sentences for style,
concision, or clarity — the prose should reflect the author's own voice.

Leave block quotes untouched: quoted AI prompts and quotes of the author's earlier posts
are reproduced verbatim on purpose.

Anything else worth changing — awkward-but-correct phrasing, factual errors, stale
cross-post references, code bugs in snippets — should be listed as a suggestion at the
end of the response, not applied.

## Common include

Posts start with `{% include blog_vars.html %}` which defines:
- `resources_path` → `https://www.kuniga.me/resources/blog/<vanity>`
- `github` → GitHub link to the post's code directory
