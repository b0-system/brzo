(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

module Tool = struct
  let doxygen = B0_memo.Tool.by_name "doxygen"
  let dot = B0_memo.Tool.by_name "dot"
end

let cmd ~reads ~conf = failwith "TODO"
