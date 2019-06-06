
type qubesdb = Mirage_impl_qubesdb.qubesdb
let qubesdb = Mirage_impl_qubesdb.qubesdb
let default_qubesdb = Mirage_impl_qubesdb.default_qubesdb

type time = Mirage_impl_time.time
let time = Mirage_impl_time.time
let default_time = Mirage_impl_time.default_time

type pclock = Mirage_impl_pclock.pclock
let pclock = Mirage_impl_pclock.pclock
let default_posix_clock = Mirage_impl_pclock.default_posix_clock

type mclock = Mirage_impl_mclock.mclock
let mclock = Mirage_impl_mclock.mclock
let default_monotonic_clock = Mirage_impl_mclock.default_monotonic_clock

type random = Mirage_impl_random.random
let random = Mirage_impl_random.random
let stdlib_random = Mirage_impl_random.stdlib_random
let default_random = Mirage_impl_random.default_random
let nocrypto = Mirage_impl_random.nocrypto
let nocrypto_random = Mirage_impl_random.nocrypto_random

type console = Mirage_impl_console.console
let console = Mirage_impl_console.console
let default_console = Mirage_impl_console.default_console
let custom_console = Mirage_impl_console.custom_console

type kv_ro = Mirage_impl_kv.ro
let kv_ro = Mirage_impl_kv.ro
let direct_kv_ro = Mirage_impl_kv.direct_kv_ro
let crunch = Mirage_impl_kv.crunch

type kv_rw = Mirage_impl_kv.rw
let kv_rw = Mirage_impl_kv.rw
let direct_kv_rw = Mirage_impl_kv.direct_kv_rw
let kv_rw_mem = Mirage_impl_kv.mem_kv_rw

type block = Mirage_impl_block.block
let block = Mirage_impl_block.block
let archive_of_files = Mirage_impl_block.archive_of_files
let archive = Mirage_impl_block.archive
let generic_block = Mirage_impl_block.generic_block
let ramdisk = Mirage_impl_block.ramdisk
let block_of_xenstore_id = Mirage_impl_block.block_of_xenstore_id
let block_of_file = Mirage_impl_block.block_of_file

type fs = Mirage_impl_fs.t
let fs = Mirage_impl_fs.typ
let fat = Mirage_impl_fs.fat
let fat_of_files = Mirage_impl_fs.fat_of_files
let generic_kv_ro = Mirage_impl_fs.generic_kv_ro
let kv_ro_of_fs = Mirage_impl_fs.kv_ro_of_fs

type network = Mirage_impl_network.network
let network = Mirage_impl_network.network
let netif = Mirage_impl_network.netif
let default_network = Mirage_impl_network.default_network

type ethernet = Mirage_impl_ethernet.ethernet
let ethernet = Mirage_impl_ethernet.ethernet
let etif = Mirage_impl_ethernet.etif

type arpv4 = Mirage_impl_arpv4.arpv4
let arpv4 = Mirage_impl_arpv4.arpv4
let arp = Mirage_impl_arpv4.arp

type v4 = Mirage_impl_ip.v4
type v6 = Mirage_impl_ip.v6
type 'a ip = 'a Mirage_impl_ip.ip
type ipv4 = Mirage_impl_ip.ipv4
type ipv6 = Mirage_impl_ip.ipv6
let ipv4 = Mirage_impl_ip.ipv4
let ipv6 = Mirage_impl_ip.ipv6
let ipv4_qubes = Mirage_impl_ip.ipv4_qubes
let create_ipv4 = Mirage_impl_ip.create_ipv4
let create_ipv6 = Mirage_impl_ip.create_ipv6
type ipv4_config = Mirage_impl_ip.ipv4_config = {
  network : Ipaddr.V4.Prefix.t * Ipaddr.V4.t;
  gateway : Ipaddr.V4.t option;
}
type ipv6_config = Mirage_impl_ip.ipv6_config = {
  addresses : Ipaddr.V6.t list;
  netmasks  : Ipaddr.V6.Prefix.t list;
  gateways  : Ipaddr.V6.t list;
}

type 'a udp = 'a Mirage_impl_udp.udp
let udp = Mirage_impl_udp.udp
type udpv4 = Mirage_impl_udp.udpv4
type udpv6 = Mirage_impl_udp.udpv6
let udpv4 = Mirage_impl_udp.udpv4
let udpv6 = Mirage_impl_udp.udpv6
let direct_udp = Mirage_impl_udp.direct_udp
let socket_udpv4 = Mirage_impl_udp.socket_udpv4

type 'a tcp = 'a Mirage_impl_tcp.tcp
let tcp = Mirage_impl_tcp.tcp
type tcpv4 = Mirage_impl_tcp.tcpv4
type tcpv6 = Mirage_impl_tcp.tcpv6
let tcpv4 = Mirage_impl_tcp.tcpv4
let tcpv6 = Mirage_impl_tcp.tcpv6
let direct_tcp = Mirage_impl_tcp.direct_tcp
let socket_tcpv4 = Mirage_impl_tcp.socket_tcpv4

type stackv4 = Mirage_impl_stackv4.stackv4
let stackv4 = Mirage_impl_stackv4.stackv4
let generic_stackv4 = Mirage_impl_stackv4.generic_stackv4
let static_ipv4_stack = Mirage_impl_stackv4.static_ipv4_stack
let dhcp_ipv4_stack = Mirage_impl_stackv4.dhcp_ipv4_stack
let qubes_ipv4_stack = Mirage_impl_stackv4.qubes_ipv4_stack
let direct_stackv4 = Mirage_impl_stackv4.direct_stackv4
let socket_stackv4 = Mirage_impl_stackv4.socket_stackv4

type conduit = Mirage_impl_conduit.conduit
let conduit = Mirage_impl_conduit.conduit
let conduit_direct = Mirage_impl_conduit.conduit_direct

type resolver = Mirage_impl_resolver.resolver
let resolver = Mirage_impl_resolver.resolver
let resolver_unix_system = Mirage_impl_resolver.resolver_unix_system
let resolver_dns = Mirage_impl_resolver.resolver_dns

type syslog = Mirage_impl_syslog.syslog
let syslog = Mirage_impl_syslog.syslog
let syslog_tls = Mirage_impl_syslog.syslog_tls
let syslog_tcp = Mirage_impl_syslog.syslog_tcp
let syslog_udp = Mirage_impl_syslog.syslog_udp
type syslog_config = Mirage_impl_syslog.syslog_config =
  { hostname : string;
    server   : Ipaddr.V4.t option;
    port     : int option;
    truncate : int option;
  }
let syslog_config = Mirage_impl_syslog.syslog_config

type http = Mirage_impl_http.http
let http = Mirage_impl_http.http
let http_server = Mirage_impl_http.cohttp_server
let cohttp_server = Mirage_impl_http.cohttp_server
let httpaf_server = Mirage_impl_http.httpaf_server

let default_argv = Mirage_impl_argv.default_argv
let no_argv = Mirage_impl_argv.no_argv

type reporter = Mirage_impl_reporter.reporter
let reporter = Mirage_impl_reporter.reporter
let default_reporter = Mirage_impl_reporter.default_reporter
let no_reporter = Mirage_impl_reporter.no_reporter

type tracing = Mirage_impl_tracing.tracing
let tracing = Mirage_impl_tracing.tracing
let mprof_trace = Mirage_impl_tracing.mprof_trace