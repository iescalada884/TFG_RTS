#!/usr/bin/perl -w
# ------------------------------------------------------------
# This is a list of scripts that you may want to modify to
# parse your own time measuremets.
# ------------------------------------------------------------
use strict;

# Check that we entered a log file parameter
my $num_args = $#ARGV + 1;
if ($num_args < 1) {
        die "How to use it: ./perl_log_parser_examples.pl file.log\n";
}

# Open the file
my $LOGFILE = $ARGV[0];
open(FILE, $LOGFILE) or die("Could not open log file $LOGFILE.\n");

# ------------------------------------------------------------
# Read the lines of the file and print them
# ------------------------------------------------------------
# my $line;
# while ($line = <FILE>) {
#       print $line\n;
# }
# ------------------------------------------------------------

# ------------------------------------------------------------
# posix measuremets
# "evaluate_token:(27,322371468)->(27,322373000)"
# ------------------------------------------------------------
my $line;
my $time;

while ($line = <FILE>) {
        $line =~ m/(\w+):\((\d+),(\d+)\)->\((\d+),(\d+)\)/;
# print "$1 $2 $3 $4 $5\n";
         $time = $5 - $3;
         print "$1: $time\n";
}

# ------------------------------------------------------------
# Match a line of the format
# "measurement_name[i]: 0123456789s 123456789ns\n"
# ------------------------------------------------------------
# my $str = "measurement_name[45]: 0123456789s 123456789ns\n";
#
# $str =~ m/(\w+)\[(\d+)]: (\d+)s (\d+)ns/;
#
# print "$1\n$2\n$3\n$4\n";

# ------------------------------------------------------------
# Go through the file and match the lines
# "measurement_name[i]: 0123456789s 123456789ns\n"
# ------------------------------------------------------------
# my $line;
# while ($line = <FILE>) {
#         $line =~ m/(\w+)\[\s*(\d+)]:\s*(\d+)s\s*(\d+)ns/;
#         print "$1\n$2\n$3\n$4\n";
# }
# ------------------------------------------------------------

# ------------------------------------------------------------
# Get the timings begin-end (very tricky, but it's perl!)
# "name_begin[i]: 0123456789s 123456789ns\n"
# ------------------------------------------------------------
# my $line;
# my @entries;
#
# while ($line = <FILE>) {
#         if ($line =~ m/(\w+)_begin\[\s*(\d+)]:\s*(\d+)s\s*(\d+)ns/) {
#                 my %entry;
#                 $entry{begin} = $4 + $3*10e9;
#                 $entries[$2] = \%entry;
#         }
#         if ($line =~ m/(\w+)_end\[\s*(\d+)]:\s*(\d+)s\s*(\d+)ns/) {
#                 $entries[$2]->{end} = $4 + $3*10e9;
#         }
# }
#
# my $value;
# my $i;
#
# foreach $i (0 .. $#entries) {
#         $value = $entries[$i]->{end} - $entries[$i]->{begin};
#         print "begin-end[$i]: $value nsecs\n";
# }
# ------------------------------------------------------------

# ------------------------------------------------------------
# Get the timings begin-end. Put only index and time so it
# it can be copy-pasted to openoffice and get histograms, etc..
# ------------------------------------------------------------
# my $line;
# my @entries;
#
# while ($line = <FILE>) {
#         if ($line =~ m/(\w+)_begin\[\s*(\d+)]:\s*(\d+)s\s*(\d+)ns/) {
#                 my %entry;
# # print "[$2] $3 s  $4 ns => $4 + $3*10e9 = " . ($4 + $3*1e9) . "\n";
#                 $entry{begin} = $4 + $3*1e9;
#                 $entries[$2] = \%entry;
#         }
#         if ($line =~ m/(\w+)_end\[\s*(\d+)]:\s*(\d+)s\s*(\d+)ns/) {
# # print "[$2] $3 s  $4 ns => $4 + $3*10e9 = " . ($4 + $3*1e9) . "\n";
#                 $entries[$2]->{end} = $4 + $3*1e9;
#         }
# }
#
# my $value;
# my $i;
#
# foreach $i (0 .. $#entries) {
#         $value = $entries[$i]->{end} - $entries[$i]->{begin};
#         print "$i $value\n";
# }
# ------------------------------------------------------------

# ------------------------------------------------------------
# Display the number of station, number of measurement and the
# measuremet in nanoseconds so it can be copied to a spreadsheet
# ------------------------------------------------------------
# my $line;
# my @entries;
#
# while ($line = <FILE>) {
#         if ($line =~ m/station(\d+)_(\w+)\[\s*(\d+)]:\s*(\d+)s\s*(\d+)ns/) {
# # print "station $1 $2 measure_number $3 " . ($5 + $4*1e9) . "\n";
#                 print "$1 $2 $3 " . ($5 + $4*1e9) . "\n";
#         }
# }

# ------------------------------------------------------------

# Close the file
close(FILE);
