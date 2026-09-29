# Expansion phases

An imported library view carries an import level. Level 0 bindings are visible
at every phase; a view imported at level *n* is visible at phase *n*. When
multiple views provide a name, the highest visible level wins.

Within a library instance, value definitions are visible at phase 0,
transformer definitions at expansion phases, and `eval-when` definitions only
at their declared phase. Each import level gets its own library instance.

Expansion-time helpers come from the current library's expansion definitions,
phase-aware imports, or the implementation primitive set. The same rules apply
to file and library expansion.

Per-level instances, visibility, and import behavior are covered by the
expander tests.
