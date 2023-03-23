use assert_cmd::Command;
use predicates::str::contains;

#[test]
fn test_pl0_input_output() {
    Command::cargo_bin("pl0")
        .unwrap()
        .args(&["-s", "examples/fact.pl0"])
        .write_stdin("5")
        .assert()
        .stdout(contains("120"))
        .success();
}
