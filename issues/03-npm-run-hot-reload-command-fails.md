# Issue 3: npm run hot-reload command fails

**Source:** GitLab issue #3 (https://gitlab.com/bagrounds/purescript-wip/-/issues/3)

**Status:** Closed (merged to master)

## Description

The `npm run hot-reload` command fails because asset files are not copied to the dist directory before bundling.

## Resolution

Fixed by copying files from assets to dist in the hot-reload script to avoid an error.

See commit: `d78dd41`
