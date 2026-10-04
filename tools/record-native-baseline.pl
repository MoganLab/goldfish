#!/usr/bin/env perl
use strict;
use warnings;

my ($input, $output) = @ARGV;
die "usage: perl tools/record-native-baseline.pl FULL_LOG OUTPUT_DIR\n" unless defined $output;
open my $in, '<', $input or die "$input: $!\n";
my @lines = <$in>;
for (@lines) { s/\e\[[0-9;]*m//g; }
my (%results, %evidence, %guards);
my $summary = 0;
for my $i (0 .. $#lines) {
    $summary = 1 if $lines[$i] =~ /^=== Test Summary ===/;
    next unless $summary;
    if ($lines[$i] =~ /^  (tests\/\S+-test\.scm) \.\.\. (PASS|FAIL)\s*$/) {
        die "duplicate result: $1\n" if exists $results{$1};
        $results{$1} = lc $2;
        $evidence{$1} = $i + 1;
    }
}
die "no complete test summary\n" unless %results && grep { /^=== Summary ===/ } @lines;
my ($total) = map { /^  Total:  (\d+)/ ? $1 : () } @lines;
my ($passed) = map { /^  Passed: (\d+)/ ? $1 : () } @lines;
my ($failed_count) = map { /^  Failed: (\d+)/ ? $1 : () } @lines;
$failed_count //= 0;
my $actual_failed = grep { $_ eq 'fail' } values %results;
die "summary counts do not match file results\n"
    unless defined $failed_count && $total == scalar(keys %results)
        && $failed_count == $actual_failed && $passed + $failed_count == $total;
for my $file (keys %results) {
    open my $source, '<', $file or die "$file: $!\n";
    local $/;
    my $text = <$source>;
    # These nine tests exit before checks when the opt-in variable is absent.
    $guards{$file} = 1 if !exists $ENV{GOLDFISH_TEST_HTTP}
        && $text =~ /getenv "GOLDFISH_TEST_HTTP"/ && $text =~ /\(exit 0\)/;
}
open my $log, '>', "$output/full-run.log" or die "$!\n";
print {$log} @lines;
close $log;
open my $out, '>', "$output/results.tsv" or die "$!\n";
print {$out} "path\tverdict\tcoverage\tevidence\n";
for my $file (sort keys %results) {
    my $coverage = $guards{$file} ? 'http-opt-in' : 'executed';
    print {$out} join("\t", $file, $results{$file}, $coverage,
                      "tests/native-baseline/full-run.log:$evidence{$file}"), "\n";
}
close $out;
my $failed = grep { $_ eq 'fail' } values %results;
printf "%d files: %d pass, %d fail; %d HTTP opt-in files\n",
       scalar(keys %results), scalar(keys %results) - $failed, $failed, scalar(keys %guards);
