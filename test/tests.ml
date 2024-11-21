open OUnit2
open Lib.Engine

let simpleTests = [
    ("aaab", [
        "aaab", true;
        "aab",false;
        "aaaab",false;
        "aaaba", false;
        "aaa",false
    ]);
]
let unionTests = [
    ("aa(a|b)", [
        "aaa", true;
        "aab",true;
        "aaab",false;
        "aad", false;
    ]);
    ("aa(a|b|C)", [
        "aaa", true;
        "aab",true;
        "aaC",true;
        "aaab",false;
        "aaba", false;
    ]);
    ("aa(a|b|C*)", [
        "aaa", true;
        "aab",true;
        "aaC",true;
        "aaCC",true;
        "aa",true;
        "aaCd", false;
        "aaab",false;
        "aaba", false;
    ]);
    ("aa(a|b|(aad|d))", [
        "aaa", true;
        "aab",true;
        "aaaad",true;
        "aad",true;
        "aa",false;
        "aac", false;
        "aaad",false;
        "aadd", false;
    ]);
]
let starTests = [
    ("a*b", [
        "aab", true;
        "ab", true;
        "b", true;
        "aad", false;
        "aa", false;
        "aaCb", false;
    ]);
    ("(ab)*", [
        "", true;
        "ab", true;
        "abab", true;
        "ababab", true;
        "aba", false;
        "abc", false;
    ]);
    ("a*bc", [
        "bc", true;
        "abc", true;
        "aabc", true;
        "aaabc", true;
        "ac", false;
        "abbc", false;
    ]);
    ("(a|b)*", [
        "", true;
        "a", true;
        "b", true;
        "ab", true;
        "ba", true;
        "aa", true;
        "bb", true;
        "abab", true;
        "c", false;
    ]);
    ("(abc)*", [
        "", true;
        "abc", true;
        "abcabc", true;
        "abcabcabc", true;
        "abca", false;
        "abcab", false;
    ]);
    ("(a|b|c)*", [
        "", true;
        "abc", true;
        "cba", true;
        "aaabbbccc", true;
        "aabbcc", true;
        "abcd", false;
        "acbd", false;
    ]);
]
let plusandQuestionTests = [
    ("a+", [
        "a", true;
        "aa", true;
        "aaa", true;
        "", false;
        "b", false;
    ]);
    ("a?b", [
        "ab", true;
        "b", true;
        "aab", false;
        "", false;
        "bb", false;
    ]);
    ("a+b", [
        "ab", true;
        "aab", true;
        "aaab", true;
        "b", false;
        "aaa", false;
    ]);
    ("a?b?", [
        "", true;
        "a", true;
        "b", true;
        "ab", true;
        "aa", false;
        "bb", false;
    ]);
    ("(ab)?c", [
        "c", true;
        "abc", true;
        "abab", false;
        "ab", false;
    ]);
    ("a+(b|c)?", [
        "a", true;
        "ab", true;
        "ac", true;
        "aa", true;
        "aab", true;
        "aac", true;
        "aabc", false;
        "", false;
    ]);
    ("(a|b)+", [
        "a", true;
        "b", true;
        "aa", true;
        "bb", true;
        "ab", true;
        "ba", true;
        "abab", true;
        "", false;
        "c", false;
    ]);
]

let mtch match_expression create_machine tests exp= 
    let matcher = match_expression (create_machine exp) in
    List.iter (fun x ->match x with  input, expected -> assert_equal (matcher input ) expected) tests

let run l engine= 
    fun _ -> List.iter (function
        |exp, tests ->
            if (engine=1) then begin
                mtch Engine1.match_expression Engine1.create_machine tests exp;
            end
            else if (engine=2) then begin
                mtch Engine1.match_expression Engine1.create_machine tests exp;
            end
            else failwith "Engine not found"
    ) l

let tests1 = "test suite for RE 1" >::: [
  "simple" >:: run simpleTests 1;
  "Union tests">:: run unionTests 1;
  "Star tests">:: run starTests 1;
  "Plus and Question Tests">:: run plusandQuestionTests 1;
]
let tests2 = "test suite for RE 2" >::: [
  "simple" >:: run simpleTests 2;
  "Union tests">:: run unionTests 2;
  "Star tests">:: run starTests 2;
  "Plus and Question Tests">:: run plusandQuestionTests 2;
]

let _ = run_test_tt_main tests1; run_test_tt_main tests2