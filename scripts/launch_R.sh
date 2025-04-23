#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
module load r/4.4.0-yycctsj 
exec /oscar/rt/9.2/software/0.20-generic/0.20.1/opt/spack/linux-rhel9-x86_64_v3/gcc-11.3.1/r-4.4.0-yycctsjvszuj5o2q4gfbaehsq7rkl4bz/bin/R --no-save --no-restore "$@"

