[@deriving show({with_path: false})]
type t =
  | True
  | False
  | String(string)
  | Integer(int)
  | Real(float)
  // Date
  // Data
  | Array(list(t))
  | Dict(list((string, t)));

type decoder('a) = t => result('a, string);

let bool =
  fun
  | True => Ok(true)
  | False => Ok(false)
  | value => Error("Expected bool, got: " ++ show(value));

let string =
  fun
  | String(value) => Ok(value)
  | value => Error("Expected string, got: " ++ show(value));

let integer =
  fun
  | Integer(value) => Ok(value)
  | value => Error("Expected integer, got: " ++ show(value));

let real =
  fun
  | Real(value) => Ok(value)
  | value => Error("Expected real, got: " ++ show(value));

let array = decodeValue =>
  fun
  | Array(items) => {
      let rec loop = (values, acc) =>
        switch (values) {
        | [] => Ok(acc)
        | [value, ...rest] =>
          switch (decodeValue(value)) {
          | Ok(value) => loop(rest, [value, ...acc])
          | Error(message) => Error(message)
          }
        };

      loop(items, []);
    }
  | value => Error("Expected array, got: " ++ show(value));

let property = (key, decodeValue) =>
  fun
  | Dict(properties) as dict =>
    switch (List.assoc_opt(key, properties)) {
    | Some(value) => decodeValue(value)
    | None =>
      Error("Expected dict with property " ++ key ++ ", got " ++ show(dict))
    }
  | value => Error("Expected dict, got " ++ show(value));

type dictGetters = {
  required: 'a. (string, decoder('a)) => 'a,
  optional: 'a. (string, decoder('a)) => option('a),
};

let dict = f =>
  fun
  | Dict(properties) as dict => {
      exception DecodeError(string);

      let get = (key, decodeValue) =>
        switch (List.assoc_opt(key, properties)) {
        | Some(value) =>
          switch (decodeValue(value)) {
          | Ok(value) => value
          | Error(message) => raise(DecodeError(message))
          }
        | None =>
          raise(
            DecodeError(
              "Expected dict with property " ++ key ++ ", got " ++ show(dict),
            ),
          )
        };

      let getters = {
        required: get,
        optional: (key, decodeValue) =>
          switch (get(key, decodeValue)) {
          | value => Some(value)
          | exception (DecodeError(_)) => None
          },
      };

      switch (f(getters)) {
      | value => Ok(value)
      | exception (DecodeError(message)) => Error(message)
      };
    }
  | value => Error("Expected dict, got " ++ show(value));

let assoc = decodeValue =>
  fun
  | Dict(properties) => {
      let rec loop = (values, acc) =>
        switch (values) {
        | [] => Ok(acc)
        | [(key, value), ...rest] =>
          switch (decodeValue(value)) {
          | Ok(value) => loop(rest, [(key, value), ...acc])
          | Error(message) => Error(message)
          }
        };

      loop(properties, []);
    }
  | value => Error("Expected dict, got " ++ show(value));

let rec oneOf = (decoders, value) =>
  switch (decoders) {
  | [] =>
    Error("Expected one of several decoders to succeed on: " ++ show(value))
  | [decode, ...rest] =>
    switch (decode(value)) {
    | Ok(value) => Ok(value)
    | Error(_) => oneOf(rest, value)
    }
  };

let map = (f, decode, value) => decode(value) |> Result.map(f);

let option = (decode, value) =>
  switch (decode(value)) {
  | Ok(value) => Ok(Some(value))
  | Error(_) => Ok(None)
  };
