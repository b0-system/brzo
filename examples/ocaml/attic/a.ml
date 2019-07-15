

open Map

module M : S = struct
  include Map.Make (struct type t = int let compare = compare end)
end
