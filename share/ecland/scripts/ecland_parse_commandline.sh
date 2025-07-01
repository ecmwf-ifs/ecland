# (C) Copyright 2021- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

function usage() {
  echo "USAGE:"
  echo "    $(basename ${0}) --help"
  echo "    $(basename ${0}) [--launch LAUNCH_CMD] [-np NTASKS] [-nt NTHREADS]"
if [[ ${ECLAND_CONTEXT:-unset} == "test" ]]; then
  echo "                     [--test-dir TEST_DIR] [--tolerance REL_TOL]"
fi
}

function help() {
  usage
  echo
  echo "Optional arguments"
  echo "    -h, --help                 Show this help message and exit."
if [[ ${ECLAND_CONTEXT:-unset} == "test" ]]; then
  echo "    -t, --test-dir TEST_DIR    Test directory."
  echo "    --tolerance                Relative error tolerance for the test. Must be an array of length equal to "
  echo "                               the number of variables tested. This overrides defaults set by the 'REL_TOL'"
  echo "                               environment variable."
fi
  echo "    --launch LAUNCH            Used as prefix to launch execution, e.g. \"mpirun -np <NTASKS>\", or \"ddt\""
  echo "                               This overrides defaults set by 'LAUNCH' environment variable"
  echo "    -np NTASKS                 Number of MPI tasks (ignored when LAUNCH is set, either via command line or environment)"
  echo "    -nt NTHREADS               Number of OpenMP threads per MPI task. This exports OMP_NUM_THREADS=<NTRHEADS>"
  echo ""
  echo "    "
  echo
  echo "Advanced options via environment variables"
  echo
  echo "    LAUNCH=<COMMAND>            # Used as prefix to launch execution, e.g. \"mpirun -np <NTASKS>\", or \"ddt\""
  echo "    OMP_NUM_THREADS=<NTHREADS>  # OpenMP threads"
}

test_dir=""
nthreads=1
ntasks=1
launch_cmd=""
rel_tol=""

while test $# -gt 0; do

    # Split --option=value in $opt="--option" and $val="value"

    opt=""
    val=""
    without_equal_sign=true
    case "$1" in
    --*=*)
      opt=`echo "$1" | sed 's/=.*//'`
      val=`echo "$1" | sed 's/--[_a-zA-Z0-9-]*=//'`
      without_equal_sign=false
      ;;
    --*)
      opt=$1
      val=$2
      ;;
    -*)
      opt=$1
      val=$2
      ;;
    *)
      break
      ;;
    esac

    #echo "debug_opt $opt $val $without_equal_sign"

    # Parse options
    case "$opt" in
      --help|-h)
        help; exit 0
        ;;
      --test-dir|-t)
        if [[ ${val::1} == "-" || ${val} == "" ]] ; then
         echo "Argument for --test-dir|-t TEST_DIR is missing"
         echo; usage; exit 1
        fi
        test_dir=${val}
        [[ $without_equal_sign == true ]] && shift
        ;;
      --tolerance)
        rel_tol="${val}"
        [[ $without_equal_sign == true ]] && shift
        ;;
      --launch)
        launch_cmd=${val}
        [[ $without_equal_sign == true ]] && shift
        ;;
      -np)
        if [[ ${val::1} == "-" || ${val} == "" ]] ; then
         echo "Argument for --np NTASKS is missing"
         echo; usage; exit 1
        fi
        ntasks=${val}
        [[ $without_equal_sign == true ]] && shift
        ;;
      -nt)
        if [[ ${val::1} == "-" || ${val} == "" ]] ; then
         echo "Argument for --nt NTHREADS is missing"
         echo; usage; exit 1
        fi
        nthreads=${val}
        [[ $without_equal_sign == true ]] && shift
        ;;
      *)
        echo "Unknown option: $opt"; echo; usage; exit 1
        ;;
    esac
    [[ $# -gt 0 ]] && shift
done

if [[ ${ECLAND_CONTEXT:-unset} == "test" ]]; then
  if [[ ${test_dir} != "" ]]; then
    TEST_DIR=${test_dir}
  fi

  if [[ ${rel_tol} != "" ]]; then
    REL_TOL=${rel_tol}
  fi
fi

if [[ ${launch_cmd} != "" ]]; then
  LAUNCH=${launch_cmd}
fi

if [[ ${nthreads} != 0 ]]; then
  export OMP_NUM_THREADS=${nthreads}
fi
if [[ ${ntasks} != 0 ]]; then
  if [[ "$LAUNCH" == "" ]]; then
    export LAUNCH=ecland-launch
  fi
  if [[ "$LAUNCH" =~ .*"ecland-launch".* ]]; then
    export ECLAND_LAUNCH_NTASKS=${ntasks};
  else
    if [[ ${ntasks} != 1 ]]; then
      echo "WARNING: ignoring argument '-np ${ntasks}'"
    fi
  fi
fi
