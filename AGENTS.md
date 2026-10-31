## PR Blogs
- With every PR, please generate a blog post describing your work in the ai-blog directory at the root of the repo. See some posts in https://bagrounds.org/ai-blog for prior examples.
- Name posts `ai-blog/YYYY-MM-DD-<slug>.md`. Copy the frontmatter, `⏮️` breadcrumb (newest earlier post) and section style (emoji H1 and bullets, Author's Note first, Signed last) from the latest post. Keep earlier posts in the same PR accurate.
- No `[[wiki links]]` in the body: posts are copied to bagrounds.org, whose automation adds those links. Only link to things that exist (GitHub URLs, external sources, `./<slug>` for other posts). Book recommendations, if any, are plain text.

## Engineering Excellence
- Strong static types - inspiration from Haskell
- Thorough test coverage, ideally with property based tests
- Functional declarative programming patterns. Avoid for loops and conditional logic in favor of principled functional abstractions.
- Prefer principled abstractions. Good ideas often come from category theory
- Unix Philosophy & Modularity
- Domain Driven Design
- Self Documenting Code - never write comments that tell what the code is doing; always write well named functions and variables so that it's obvious

## Scientific Engineering
- Always test your work and iterate until your tests succeed and verify that the goal has been achieved.

## Markdown Issues
- Keep issue files in `issues/open/` or `issues/closed/` according to their status; prefix filenames with the date they were first added to this repository (`YYYY-MM-DD`).
- Give related issues a shared topic prefix, such as `matchmaking-`.
