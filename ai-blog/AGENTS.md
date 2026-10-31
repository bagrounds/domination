# Writing AI Blog Posts

Every PR gets a post in this directory that explains the work (see the root
[`AGENTS.md`](../AGENTS.md)). Posts are copied to [bagrounds.org/ai-blog](https://bagrounds.org/ai-blog)
by automation in another repository, which also adds the site's cross-links there.

## Links

- **Do not write `[[wiki links]]` in the body.** Nothing in this repository resolves
  them, and the publishing automation adds links to books, topics and other pages on
  its side.
- Only link to things that exist:
  - files in this repository, by their GitHub URL
    (`https://github.com/bagrounds/domination/blob/master/<path>`);
  - issues and PRs, by their GitHub URL;
  - external sources, by their canonical URL;
  - other posts in this directory, as `./<slug>` (the file name without `.md`).
- Keep the frontmatter exactly as shown below, including the `Author` value. It is
  metadata for the site, not part of the body.

## File

- Name: `YYYY-MM-DD-<slug>.md`, using the date the work was done.
- One post per PR. If a PR changes direction (for example, a design that then gets
  implemented), update earlier posts in the same PR so they stay accurate.

## Template

```markdown
---
share: true
aliases:
  - YYYY-MM-DD | 🔗 Title 🚪
title: YYYY-MM-DD | 🔗 Title 🚪
URL: https://bagrounds.org/ai-blog/YYYY-MM-DD-<slug>
Author: "[[github-copilot-agent]]"
tags:
---
[🏠 Home](../) > [🤖 AI Blog](./) | [⏮️ <previous post title>](./<previous-slug>)

# 🔗 YYYY-MM-DD | Title 🚪

## 🧑‍💻 Author's Note

👋 Who is writing, what was asked (link the issue), and the result in one or two lines.

## 🎯 The Problem

## ✅ The Change

## 🧪 How I Checked It

## 💡 Takeaways

## ✍️ Signed

🤖 Built with care by **GitHub Copilot Coding Agent**
📅 Month D, YYYY
🏠 For [bagrounds.org](https://bagrounds.org/)
```

- The `⏮️` link points to the newest post before this one, by date and then by name.
- Pick section headings that fit the work. Keep the Author's Note first and Signed last.
- An optional `## 📚 Book Recommendations` section after Signed may list titles and
  authors in plain text, with a short reason for each, and no links.

## Style

- Start each line or bullet with an emoji, as the existing posts do.
- Prefer short sentences and concrete facts: file names, test counts, what was verified.
- Describe what was checked and how. Do not claim anything that was not run.
- Use fenced code blocks with a language tag for code.
