(*
  Unit tests for Semver
*)

open Printf

let semver = Alcotest.testable (Fmt.of_to_string Semver.to_string) Semver.equal

let of_string x =
  match Semver.of_string x with
  | None -> assert false
  | Some x -> x

let v = of_string

let option_to_string to_string x =
  match x with
  | None -> "None"
  | Some x -> sprintf "Some (%s)" (to_string x)

let optversion_to_string = option_to_string Semver.to_string

let test_parse_print ver str () =
  Alcotest.check semver "equal" ver (of_string str);
  Alcotest.(check string) "equal" (Semver.to_string ver) str

let eq v1 v2 = Alcotest.(check bool) "equal" true (Semver.equal v1 v2)

let gt v1 v2 =
  Alcotest.(check bool) "equal" true (Semver.compare v1 v2 > 0);
  Alcotest.(check bool) "equal" true (Semver.compare v2 v1 < 0)

let q input versions expected =
  let res = Semver.query_str input (List.map v versions) in
  printf "input: %s; expected result: %s; actual result: %s\n"
    input
    (optversion_to_string expected)
    (optversion_to_string res);
  Alcotest.(check int) "equal" (Option.compare Semver.compare res expected) 0

let suite : unit Alcotest.test_case list = [
  "3 parts", `Quick, test_parse_print ((1, 1, 1)) "1.1.1";
  "long parts", `Quick,
  test_parse_print ((1243, 32415, 54535)) "1243.32415.54535";

  "inc", `Quick, (fun _ ->
    eq (v"0.0.1") (Semver.succ `Patch (v"0.0.0"));
    eq (v"0.1.0") (Semver.succ `Minor (v"0.0.0"));
    eq (v"1.0.0") (Semver.succ `Major (v"0.0.0"));

    eq (v"0.1.3") (Semver.succ `Patch (v"0.1.2"));
    eq (v"1.1.0") (Semver.succ `Minor (v"1.0.2"));
    eq (v"1.0.0") (Semver.succ `Major (v"0.1.2"));
  );

  "cmp", `Quick, (fun _ ->
    eq (v"1.1.1") (v"1.1.1");

    gt (v"1.1.1") (v"1.1.0");
    gt (v"1.1.1") (v"1.0.1");
    gt (v"1.1.1") (v"0.1.1");
  );

  "query", `Quick, (fun _ ->
    q "1.1.1" ["1.1.1"] (Some (v"1.1.1"));
    q "1.1" ["1.1.1"] (Some (v"1.1.1"));
    q "1" ["1.1.1"] (Some (v"1.1.1"));
    q "2" ["1.1.1"] None;

    q "1.1.1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"1.1.1"));
    q "1.1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"1.1.2"));
    q "1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"1.2.1"));
    q "2.1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"2.1.1"));
    q "0.5" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] None;
  );
]

let () =
  Alcotest.run "Semver" ["Semver", suite]
