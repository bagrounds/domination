---
name: bug-report-research
description: Investigate reported software bugs by tracing symptoms to code, validating likely causes, and comparing fixes with evidence and trade-offs.
---

# Research a bug report

Use this skill when asked to investigate a bug report, explain its likely cause, or recommend ways to fix it.

## Workflow

1. **Extract the report's facts.** Separate observed behavior, expected behavior, environment, reproduction steps, and diagnostics. Treat a reporter's suggested cause as a hypothesis, not a fact.
2. **Trace the relevant path.** Find the UI, data, API, or system components involved. Follow their actual composition and runtime flow; do not infer behavior from class names or isolated declarations.
3. **Gather repository evidence.** Inspect the implementation, related tests, configuration, and relevant history or documentation. Cite precise files and locations for important claims.
4. **Test the leading explanation.** Reproduce the behavior when practical, or create a focused test that exercises the suspected condition. If the environment prevents either, say what remains unverified and lower the confidence of the conclusion.
5. **Separate evidence from hypotheses.** Explain the symptom's most likely cause and confidence level. Note plausible alternative causes and any diagnostic clues that do not appear related.
6. **Compare multiple approaches.** Give at least three viable options, with their scope, benefits, costs, compatibility implications, and regression risks. Recommend the smallest option that fully addresses the observed behavior.
7. **Define verification.** Name focused tests or manual scenarios that distinguish a real fix from a superficial change. Include relevant edge cases and explicitly state any platform or environment not tested.

## Report format

- **Observed behavior and expected behavior**
- **Technical context and evidence**
- **Likely root cause**, including confidence and unresolved uncertainty
- **Potential approaches**, with trade-offs
- **Recommended fix and verification**

Do not present a plausible explanation as confirmed unless it has been reproduced or supported by direct implementation evidence. Do not expand the scope to unrelated defects.
