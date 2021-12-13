module type UPD = sig
  type t
  type value
  val update: value -> t -> t
end

module MakeUpd (U: UPD) = struct
  type state = U.t * U.value list

  let rec update values t = match values with
    | [] -> t
    | value :: tail -> update tail U.(update value t)

  let return t : state = (t, [])

  let rec (>>=) (t, values) fn =
    let (t, values') = fn t in
    let values = values @ values' in
    let t' = update values t in
    (t', values)
end
