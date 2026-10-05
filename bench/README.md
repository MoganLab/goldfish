# Benchmark records

Use the checked benchmark sources and the newest result archive for each area:

| Area | Runner / source | Current record |
|---|---|---|
| Runtime scale and large sets | [`native-scale/`](native-scale/), [`tools/bench-native-scale.sh`](../tools/bench-native-scale.sh) | [`native-million-set/`](native-million-set/) |
| JSON parse, serialization, lookup and enumeration | [`json-phases/`](json-phases/) | [`json-phases/README.md`](json-phases/README.md) |
| Startup cache | [`native-startup-cache/`](native-startup-cache/) | [`native-startup-cache/README.md`](native-startup-cache/README.md) |
| Vector access and set profile | [`native-vector-access/`](native-vector-access/) | [`native-vector-access/README.md`](native-vector-access/README.md) |
| Hash-table growth repair | [`native-scale-rehash/`](native-scale-rehash/) | [`native-scale-rehash/README.txt`](native-scale-rehash/README.txt) and `summary.tsv` |

The `native-scale-baseline/` directory is the original harness baseline. It is
not evidence for the final million-set workload. Old scale ladders, runner
deadline experiments and the undivided JSON application profile were removed
after their conclusions were superseded by the current records above.

Most result directories retain the command metadata, input, phase timings,
stdout/stderr and checksums needed to review that run. A single sample is a
reproduction point, not a statistical performance guarantee.
