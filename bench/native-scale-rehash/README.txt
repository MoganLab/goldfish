Post-repair bounded set growth

Repair commit: 27f46391. Native runtime only; no S7 oracle.
Historical levels: 10, 100, 1000, 10000, 100000, 1000000, in order.
One exploratory sample per level; these are growth probes, not throughput estimates.
Sample deadline: 60 seconds. Setup stage deadline: 90 seconds.
Whole runner deadline per level: 240 seconds, with its documented kill grace.
Run only after the semantic gates complete, with no concurrent test driver.

Stop on a failing or timed-out level. Before each next level, estimate total
process time as current wall time minus list-build, set-build and check time,
plus those three variable phases multiplied by the size ratio. Stop if that
estimate exceeds the 60-second sample deadline. For completed adjacent levels
starting at size 100, stop if construction time grows over three times the
size ratio. Predictions are scheduling guards, not measured results.

Command per completed level (SIZE and PREVIOUS_CACHE supplied explicitly):
sh tools/bench-native-scale.sh --smoke --case=million-set --size=SIZE --phases --timeout=60 --setup-timeout=90 --run-timeout=240 --cache=PREVIOUS_CACHE --output=EMPTY_OUTPUT
Initial cache: /tmp/gf-resize-fix/cache; later levels seed from the preceding
level's output/cache. The runner copies each cache and validates its version.
Archive output files only, excluding cache directories. Each level retains
commands, binary/source/input fingerprints, raw logs, time and peak RSS.
Peak RSS is for the whole process, not the inner construction phase.
Keep historical pre-repair probes and the original full-suite failure intact.

Outcome: sizes 10 through 10000 pass. Size 100000 times out during construction
at 60.02 seconds; its check phase never begins. The ladder stops there and
At that stage, 1000000 was not run. See summary.tsv and decisions.tsv. Fixed-cost fluctuations
made the preceding 54.88-second prediction optimistic; the real deadline
bounded the run. Preserve this timeout as a measured limitation, not a skip.
This is a historical record of the repair investigation. Its bounded samples
were superseded by the later insertion optimization and exact one-million-element
validation documented in `../native-million-set/README.md`. Keep the compact
summary and decision tables as evidence of the earlier timeout; single samples
are not scaling estimates.
