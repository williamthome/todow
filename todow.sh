#!/bin/sh

set -e

case $1 in
  ""|shell)
    rebar3 shell
  ;;
  compile)
    rebar3 compile
  ;;
  clean)
    rebar3 clean
  ;;
  format)
    rebar3 fmt
  ;;
  dialyzer)
    rebar3 dialyzer
  ;;
  test)
    case $2 in
      ""|unit)
        rebar3 eunit
      ;;
      integration)
        rebar3 ct
      ;;
      all)
        $0 test unit && $0 test integration
      ;;
      *)
      echo "
--------------------------------------------------------------------------------
Error:
    $2 is invalid.

Valid options:
    unit          Run unit tests.
    integration   Run integration tests.
    all           Run unit an integration tests.

    If none is passed it runs unit tests.
--------------------------------------------------------------------------------
"
      exit 1
    ;;
    esac
  ;;
  *)
    echo "
--------------------------------------------------------------------------------
Error:
    $1 is invalid.

Valid options:
    compile       Compile the needed dependencies and the project's
                  apps .app.src and .erl files.
    clean         Removes compiled beam files from apps.
    shell         Runs a shell with project apps and deps in path.
    format        Format all erlang files.
    dialyzer      Carry out success typing analysis.
    test          Pass 'unit', 'integration' or 'all' flag to test.

    If none is passed it runs shell.
--------------------------------------------------------------------------------
"
    exit 1
  ;;
esac

exit 0
