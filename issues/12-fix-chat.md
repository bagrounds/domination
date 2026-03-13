# Issue 12: Fix chat

**Source:** GitLab issue #12 (https://gitlab.com/bagrounds/purescript-wip/-/issues/12)

**Status:** Closed (merged to master)

## Description

Chat messages were not being sent properly. The message was saved locally but not actually transmitted to peers.

## Resolution

Fixed chat to actually send the chat message after saving it locally.

See commit: `a3b244f`
