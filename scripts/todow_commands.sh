site_command() {
  [ -z $2 ] && ../../bin/zotonic $1 || ../../bin/zotonic $1 $2
}

site_compile() { site_command compile ; }

site_start() { site_command start ; }

site_stop() { site_command stop ; }

site_shell() { site_command shell ; }

site_debug() { site_command debug ; }

site_test() { site_command sitetest $sitename ; }

site_wait_start() {
  site_start
  site_command wait
}

site_start_and_compile() {
  site_wait_start
  site_compile
  site_stop
}

site_start_and_test() {
  site_wait_start
  site_compile
  site_test
  site_stop
}
