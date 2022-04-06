open Yates_types.Types

module type Algorithm = sig
    val initialize : scheme -> unit
    val local_recovery : scheme -> topology -> failure -> demands -> scheme
    val solve : topology -> demands -> scheme
end

module AC = AC

module Ak = Ak

module Cspf  = Cspf

module Ecmp = Ecmp

module Edksp = Edksp

module Ffc = Ffc

module Ksp = Ksp

module Mcf = Mcf

module MwMcf = MwMcf

module Raeke = Raeke

module SemiMcf = SemiMcf

module Spf = Spf

module Vlb = Vlb

module MagicRouting = MagicRouting

module Helix = Helix

module HelixNoOpti = Helix
