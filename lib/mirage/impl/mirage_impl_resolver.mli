type resolver

open Functoria
open Mirage_impl_random
open Mirage_impl_mclock
open Mirage_impl_time

val resolver : resolver typ

val resolver_dns :
  ?ns:Ipaddr.V4.t ->
  ?ns_port:int ->
  ?time:time impl ->
  ?mclock:mclock impl ->
  ?random:random impl ->
  Mirage_impl_stack.stackv4 impl ->
  resolver impl

val resolver_unix_system : resolver Functoria.impl
