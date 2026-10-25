use strict; use warnings;

my $in = shift @ARGV;
open my $fh, '<', $in or die "cannot open $in\n";
my @pairs;
while (<$fh>) {
  if (/other="([^"]*)"\s+territory="001"\s+type="([^"]*)"/) {
    push @pairs, [$1, $2];
  }
}
close $fh;
@pairs = sort { $a->[0] cmp $b->[0] } @pairs;

print <<'HEAD';
//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

// Windows-to-IANA timezone mapping table, generated from the CLDR
// supplemental data file common/supplemental/windowsZones.xml
// (https://github.com/unicode-org/cldr).
//
// Data source copyright and license:
//   Copyright (C) 1991-2013 Unicode, Inc.
//   Unicode License - https://www.unicode.org/copyright.html
//
// Only the "golden" mappings (territory="001") are kept: each Windows
// timezone name maps to its primary IANA timezone. Regenerate with:
//   perl tools/windows-tz/gen.pl common/supplemental/windowsZones.xml \
//     > src/liii_date_time_windows_tz.h
// after updating the CLDR checkout.

#ifndef LIII_DATE_TIME_WINDOWS_TZ_H
#define LIII_DATE_TIME_WINDOWS_TZ_H

#include <string.h>

struct goldfish_windows_tz_entry {
  const char* windows;
  const char* iana;
};

// Sorted by `windows` for binary search. Do not edit by hand.
static const struct goldfish_windows_tz_entry goldfish_windows_tz_map[]= {
HEAD

for my $p (@pairs) {
  printf "  {\"%s\", \"%s\"},\n", $p->[0], $p->[1];
}

print <<'TAIL';
};

// Return the IANA timezone name for a Windows timezone name, or NULL if
// the name is not in the table.
static const char*
goldfish_windows_tz_to_iana (const char* windows_name) {
  if (windows_name == NULL)
    return NULL;
  int lo= 0;
  int hi= (int) (sizeof (goldfish_windows_tz_map) / sizeof (goldfish_windows_tz_map[0])) - 1;
  while (lo <= hi) {
    int mid= (lo + hi) / 2;
    int cmp = strcmp (windows_name, goldfish_windows_tz_map[mid].windows);
    if (cmp == 0)
      return goldfish_windows_tz_map[mid].iana;
    if (cmp < 0)
      hi= mid - 1;
    else
      lo= mid + 1;
  }
  return NULL;
}

#endif /* LIII_DATE_TIME_WINDOWS_TZ_H */
TAIL
