#!/bin/sh

. ./scripts/todow_printer.sh
. ./scripts/todow_commands.sh

set -e

case $1 in
  shell)
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
      site)
        site_start_and_test
      ;;
      all)
        $0 test unit && $0 test integration && $0 test site
      ;;
      *)
        print_test_info $2
        exit 1
      ;;
    esac
  ;;
  site)
    case $2 in
      compile)
        site_start_and_compile
      ;;
      start)
        site_start
      ;;
      stop)
        site_stop
      ;;
      shell)
        site_shell
      ;;
      debug)
        site_debug
      ;;
      test)
        site_start_and_test
      ;;
      *)
        print_site_info $2
        exit 1
      ;;
    esac
  ;;
  *)
    print_main_info $1
    exit 1
  ;;
esac

exit 0
