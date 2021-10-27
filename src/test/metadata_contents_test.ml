open Tezos_contract_metadata.Metadata_contents

module Example = struct
  let rec build = function
    | 0 -> make []
    | 1 ->
        make [] ~homepage:"https://gitlab.com/tezos/tezos"
          ~name:"example-from-the-source"
          ~description:
            {txt|This is a fake metadata blob constructed at
tezos_contract_metadata/lib/src/metadata_contents.ml.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.

Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est
laborum.
|txt}
          ~version:"0.42.0"
          ~license:License.{name= "MIT"; details= Some "The MIT License"}
    | 2 ->
        let m1 = build 1 in
        {m1 with interfaces= ["TZIP-16"; "TZIP-12"]}
    | 3 ->
        let m2 = build 2 in
        { m2 with
          unknown=
            [ ("source-repository", `String "https://gitlab.com/tezos/tezos")
            ; ("commit", `String "8d3077fb78ff157b36a72f15ff2d17df7c4763f7") ]
        }
    | 4 ->
        let m2 = build 2 in
        {m2 with views= [View.Example.build 0]}
    | 5 ->
        let m2 = build 2 in
        {m2 with views= [View.Example.build 0; View.Example.build 1]}
    | _ -> assert false

  let all () =
    let rec o n =
      try
        let ex = build n in
        ex :: o (n + 1)
      with _ -> [] in
    o 0
end

let%expect_test "empty metadata json" =
  let examples = Example.all () in
  let example = List.nth examples 0 in
  print_endline (to_json example) ;
  [%expect {|
    {} |}]

let%expect_test "short metadata json" =
  let examples = Example.all () in
  let example = List.nth examples 1 in
  print_endline (to_json example) ;
  [%expect
    {|
{
  "name": "example-from-the-source",
  "description": "This is a fake metadata blob constructed at\ntezos_contract_metadata/lib/src/metadata_contents.ml.\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do\neiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\nminim veniam, quis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat.\n\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum\ndolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non\nproident, sunt in culpa qui officia deserunt mollit anim id est\nlaborum.\n",
  "version": "0.42.0",
  "license": {
    "name": "MIT",
    "details": "The MIT License"
  },
  "homepage": "https://gitlab.com/tezos/tezos"
}|}]

let%expect_test "json with interfaces" =
  let examples = Example.all () in
  let example = List.nth examples 2 in
  print_endline (to_json example) ;
  [%expect
    {|
{
  "name": "example-from-the-source",
  "description": "This is a fake metadata blob constructed at\ntezos_contract_metadata/lib/src/metadata_contents.ml.\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do\neiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\nminim veniam, quis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat.\n\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum\ndolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non\nproident, sunt in culpa qui officia deserunt mollit anim id est\nlaborum.\n",
  "version": "0.42.0",
  "license": {
    "name": "MIT",
    "details": "The MIT License"
  },
  "homepage": "https://gitlab.com/tezos/tezos",
  "interfaces": [
    "TZIP-16",
    "TZIP-12"
  ]
}|}]

let%expect_test "json with unknown" =
  let examples = Example.all () in
  let example = List.nth examples 3 in
  print_endline (to_json example) ;
  [%expect
    {|
{
  "name": "example-from-the-source",
  "description": "This is a fake metadata blob constructed at\ntezos_contract_metadata/lib/src/metadata_contents.ml.\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do\neiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\nminim veniam, quis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat.\n\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum\ndolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non\nproident, sunt in culpa qui officia deserunt mollit anim id est\nlaborum.\n",
  "version": "0.42.0",
  "license": {
    "name": "MIT",
    "details": "The MIT License"
  },
  "homepage": "https://gitlab.com/tezos/tezos",
  "interfaces": [
    "TZIP-16",
    "TZIP-12"
  ],
  "source-repository": "https://gitlab.com/tezos/tezos",
  "commit": "8d3077fb78ff157b36a72f15ff2d17df7c4763f7"
}|}]

let%expect_test "json with a view" =
  let examples = Example.all () in
  let example = List.nth examples 4 in
  print_endline (to_json example) ;
  [%expect
    {|
{
  "name": "example-from-the-source",
  "description": "This is a fake metadata blob constructed at\ntezos_contract_metadata/lib/src/metadata_contents.ml.\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do\neiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\nminim veniam, quis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat.\n\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum\ndolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non\nproident, sunt in culpa qui officia deserunt mollit anim id est\nlaborum.\n",
  "version": "0.42.0",
  "license": {
    "name": "MIT",
    "details": "The MIT License"
  },
  "homepage": "https://gitlab.com/tezos/tezos",
  "interfaces": [
    "TZIP-16",
    "TZIP-12"
  ],
  "views": [
    {
      "name": "view0",
      "implementations": [
        {
          "michelsonStorageView": {
            "returnType": {
              "prim": "nat"
            },
            "code": []
          }
        },
        {
          "restApiQuery": {
            "specificationUri": "https://example.com/v1.json",
            "path": "/get-something"
          }
        }
      ],
      "pure": true
    }
  ]
}|}]

let%expect_test "json with 2 views" =
  let examples = Example.all () in
  let example = List.nth examples 5 in
  print_endline (to_json example) ;
  [%expect
    {|
{
  "name": "example-from-the-source",
  "description": "This is a fake metadata blob constructed at\ntezos_contract_metadata/lib/src/metadata_contents.ml.\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do\neiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\nminim veniam, quis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat.\n\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum\ndolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non\nproident, sunt in culpa qui officia deserunt mollit anim id est\nlaborum.\n",
  "version": "0.42.0",
  "license": {
    "name": "MIT",
    "details": "The MIT License"
  },
  "homepage": "https://gitlab.com/tezos/tezos",
  "interfaces": [
    "TZIP-16",
    "TZIP-12"
  ],
  "views": [
    {
      "name": "view0",
      "implementations": [
        {
          "michelsonStorageView": {
            "returnType": {
              "prim": "nat"
            },
            "code": []
          }
        },
        {
          "restApiQuery": {
            "specificationUri": "https://example.com/v1.json",
            "path": "/get-something"
          }
        }
      ],
      "pure": true
    },
    {
      "name": "view-01",
      "implementations": [
        {
          "michelsonStorageView": {
            "parameter": {
              "prim": "pair",
              "args": [
                {
                  "prim": "mutez",
                  "annots": [
                    "%amount"
                  ]
                },
                {
                  "prim": "string",
                  "annots": [
                    "%name"
                  ]
                }
              ]
            },
            "returnType": {
              "prim": "map",
              "args": [
                {
                  "prim": "string"
                },
                {
                  "prim": "string"
                }
              ]
            },
            "code": [
              {
                "prim": "DUP"
              },
              {
                "prim": "DIP",
                "args": [
                  [
                    {
                      "prim": "CDR"
                    },
                    {
                      "prim": "PUSH",
                      "args": [
                        {
                          "prim": "string"
                        },
                        {
                          "string": "Huh"
                        }
                      ]
                    },
                    {
                      "prim": "FAILWITH"
                    }
                  ]
                ]
              }
            ],
            "annotations": [
              {
                "name": "%amount",
                "description": "The amount which should mean something in context. It's in `mutez` which should also mean something more than lorem ipsum dolor whatever …"
              },
              {
                "name": "%name",
                "description": "The name of the thing being queried."
              }
            ],
            "version": "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"
          }
        }
      ]
    }
  ]
}|}]
