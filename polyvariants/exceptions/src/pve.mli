type base_error = [ `NotFound of string | `InvalidInput of string ]

val fetch_data : string -> (string, base_error) result

type extended_error =
  [ base_error | `NetworkError of string | `TimeoutError of string ]

val fetch_with_extended_error : string -> (string, extended_error) result
