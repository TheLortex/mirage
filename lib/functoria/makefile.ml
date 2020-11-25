(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013-2020 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2015-2020 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = { depext : bool; name : string; extra_repo : string option }

let v ?extra_repo ~depext name = { depext; name; extra_repo }

let depext_rule =
  {|depext:
	$(OPAM) pin add -k path --no-action --yes $(UNIKERNEL_NAME)-install .
	$(OPAM) depext --yes --update $(UNIKERNEL_NAME)-install
	$(OPAM) pin remove --no-action $(UNIKERNEL_NAME)-install|}

let opam_repo_add_rule repo =
  Fmt.str
    {|repo-add:
	$(OPAM) repo add mirage-tmp %s ||\
	$(OPAM) repo set-url mirage-tmp %s|}
    repo repo

let opam_repo_remove_rule = Fmt.str {|repo-rm:
	$(OPAM) repo remove mirage-tmp|}

let pp_extra_rules ppf t =
  let rules, targets =
    match t.depext with
    | true -> ([ depext_rule ], [ "depext" ])
    | false -> ([], [])
  in
  let rules, targets =
    match t.extra_repo with
    | Some repo ->
        ( opam_repo_add_rule repo :: opam_repo_remove_rule :: rules,
          "repo-add" :: "repo-rm" :: targets )
    | None -> (rules, targets)
  in
  match rules with
  | [] -> ()
  | _ ->
      Fmt.pf ppf " %a\n\n"
        (Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ") Fmt.string)
        targets;
      Fmt.pf ppf "%a"
        (Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf "\n\n") Fmt.string)
        rules

let pp ppf t =
  let pp_depext ppf = function
    | true -> Fmt.pf ppf "\n\t$(MAKE) depext"
    | false -> ()
  and pp_add_repo ppf = function
    | Some _ -> Fmt.pf ppf "\n\t$(MAKE) repo-add"
    | None -> ()
  and pp_remove_repo ppf = function
    | Some _ ->
        Fmt.pf ppf
          " && $(MAKE) repo-rm || (ret=$?; $(MAKE) repo-rm && exit $ret)"
    | None -> ()
  in
  Fmt.pf ppf
    {|-include Makefile.user

UNIKERNEL_NAME = %s
OPAM = opam

.PHONY: all depend depends clean build%a

all:: build

depend depends::$(UNIKERNEL_NAME).opam.locked
	$(OPAM) monorepo pull

$(UNIKERNEL_NAME).opam.locked: $(UNIKERNEL_NAME).opam%a
	$(OPAM) install ./$(UNIKERNEL_NAME)-install.opam --deps-only --yes%a
	$(OPAM) monorepo lock --build-only $(UNIKERNEL_NAME)%a

build::
	dune build

clean::
	mirage clean|}
    t.name pp_extra_rules t pp_depext t.depext pp_add_repo t.extra_repo
    pp_remove_repo t.extra_repo
