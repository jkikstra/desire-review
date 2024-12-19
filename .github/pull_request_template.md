<!--
Delete each of these instruction comments as you complete it.

Title: use a short, declarative statement similar to a commit message,
e.g. “Change {thing X} to {fix solve bug|enable feature Y}”
ideally with the topic/theme in the beginning between square brackets []
-->

**Required:** write a single sentence that describes the changes made by this PR.

<!--
Optional: write a longer description to help a colleague understand the PR in ~3 minutes.
-->

## How to review

**Required** (when merging to `main`). You may want to write “TBA”, then update this section when the PR is ready for review.

<!--
- Describe specific tasks that reviewer(s) must do to check that the PR achieves
  the description, above.
- If ≥2 reviews are needed (e.g. content vs. code), describe each separately.
- Include the estimated time to review and any required knowledge/skills that
  reviewer(s) must have.
- If no review is required, write “No review:” and describe why.

Some examples:

- Read the diff (specific file(s)?) and note that the CI checks all pass.
  Time: 30 minutes.
- Run a specific code snippet or command and check the output.
  Time: 1 hour, from someone familiar with [topic/content].
- Ensure that changes/additions are self-documenting (i.e. that another team
  member will be able to understand what the code does in the future).
- Check the implementation of example_function() for style/optimization.
  Time: 1 hour from a Python/pandas expeert.
- Build the documentation and look at a certain page.
-->

## PR checklist
<!--
- Add any items particular to your own PR to this list.
- The first two items (code style, CI checks) are *required*.
- The remaining items are all *required* if the PR results in changes to user-
  facing behaviour, e.g. new features or fixes to existing behaviour. They are
  *optional* if the changes are solely to documentation, CI configuration, etc.

  If they are not needed, strike them out (in full or in part) and add a short
  explanation after the strike out:

  - ~Add or expand tests.~ No change in behaviour, simply refactoring.
  - ~Add or expand tests.~ Will be added at a later time, for now files are
    excluded from coverage.
-->

- [ ] Continuous integration checks all ✅; local
- [ ] Continuous integration checks all ✅; Github Actions
- [ ] Add or expand tests
  <!--
  Best, test all functions and classes in new/modified code files.
  At minimum, add new modules to `message_data.tests.test_import.MODULES_WITHOUT_TESTS`.
  -->
- [ ] Add, expand, or update documentation.
  <!--
  - Add at least "stub" documentation: even one page with a few sentences
    describing high-level things that the code does, and how that's split across
    how the different Python files you've added.
  - The "stub" page can include placeholders for more description that you intend
    to write later. Without those placeholders, there's a situation of "unknown
    unknowns" and it's hard/slow for people to do work.
  - Imagine your colleagues basic/initial questions and answer them, so they can
    understand how to use/modify it, etc., without having to take up your time.
  -->
Confirm that the following `R` scripts still work out of the box:
- [ ] dle_run.R
- [ ] calculator_run.R
- [ ] tests/testthat.R
- [ ] collect_*.R

Delete these final lines to confirm you've completed the template.
Request a review from a teammate or \@jkikstra when all are complete.
💡 To reduce conflicts/divergence from `main`, rebase the PR branch frequently.
