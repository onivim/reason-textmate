type value =
  | True
  | False
  | String(string)
  | Integer(int)
  | Real(float)
  // Date
  // Data
  | Array(list(value))
  | Dict(dict)

and dict = list(property)

and property = {
  key: string,
  value,
};

type plist = dict;

let find = (key, proplist) =>
  List.find_opt(prop => prop.key == key, proplist)
  |> Option.map(prop => prop.value);
