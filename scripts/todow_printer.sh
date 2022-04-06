#!/bin/sh

print_separator() {
  echo "--------------------------------------------------------------------------------"
}

print_todow() {
  echo " _____         _
|_   _|__   __| | _____      __
  | |/ _ \ / _  |/ _ \ \ /\ / /
  | | (_) | (_| | (_) \ V  V /
  |_|\___/ \__,_|\___/ \_/\_/
"
}

print_empty_command() {
  echo "A command is expected.\n"
}

print_invalid_command() {
  echo "Error: '$1' it's an invalid command.\n"
}

maybe_print_invalid_command() {
  [ -z $1 ] && print_empty_command || print_invalid_command $1
}

print_default_command() {
  echo "\n\tIf none is passed it runs $1."
}

print_commands_label() {
  echo "Valid commands:"
}

print_command() {
  printf "\t%-15s%s\n" "$1" "$2"
}

print_main_info() {
  print_separator
  print_todow
  maybe_print_invalid_command $1
  print_commands_label
  print_command "compile" "Compile the needed dependencies and the project's apps .app.src and .erl files."
  print_command "clean" "Removes compiled beam files from apps."
  print_command "shell" "Runs a shell with project apps and deps in path."
  print_command "format" "Format all erlang files."
  print_command "dialyzer" "Carry out success typing analysis."
  print_command "test" "Pass 'unit', 'integration', 'site' or 'all' flag to test."
  print_separator
}

print_test_info() {
  print_separator
  print_todow
  maybe_print_invalid_command $1
  print_commands_label
  print_command "unit" "Run unit tests."
  print_command "integration" "Run integration tests."
  print_command "site" "Run site tests."
  print_command "all" "Run all tests."
  print_default_command "unit"
  print_separator
}

print_site_info() {
  print_separator
  print_todow
  maybe_print_invalid_command $1
  print_commands_label
  print_command "compile" "Compiles the erlang site code."
  print_command "start" "Starts the site."
  print_command "stop" "Stops the site."
  print_command "shell" "Site shell if site is running."
  print_command "debug" "Starts the site in debug mode."
  print_command "test" "Runs site tests."
  print_separator
}
