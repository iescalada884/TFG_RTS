#!/usr/bin/perl -w
#
# MaRTE OS.
#
# Change some libraries and object files in the GNAT directory to generate
# 32bits or 64bits code (64bits is the original configuration of GNAT compiler).
#
# These libraries and object files have been placed in their directories by
# minstall.

# Use:
my $USE_MODE = "mcopy_32-64bits_gnat_libs.pl [32|64]";

use strict;

# get MaRTE utils directory 
use File::Basename;
use File::Spec;
my $UTILS_PATH;
BEGIN {
    $UTILS_PATH= dirname(File::Spec->rel2abs( __FILE__ ));
}

# Require local_config.pl
require "$UTILS_PATH/local_config.pl";
use vars qw(%GNAT_LIBS_PATH);

# Require current_arch.pl
require "$UTILS_PATH/current_arch.pl";
use vars qw($CURRENT_ARCH);

# Require globals.pl
require "$UTILS_PATH/globals.pl";
use vars qw(&exec_command_no_echo);

# directories of libraries
my %LIB_DIR = ("32" => "marte_32bits_libs",
	       "64" => "marte_copy_of_original_64bits_libs");

# Arguments processing
$#ARGV == 0 or 
    die "Wrong parameter, use: $USE_MODE\n";

my $bits_num = $ARGV[0];

exists $LIB_DIR{$bits_num} or
    die "Wrong parameter, use: $USE_MODE\n";

# Copy libs
exec_command_no_echo "cd $GNAT_LIBS_PATH{x86} && " .
    "cp $LIB_DIR{$bits_num}/* .";
print "Set ${bits_num}bits libs in $GNAT_LIBS_PATH{x86}\n"; 
