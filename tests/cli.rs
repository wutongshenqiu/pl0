use assert_cmd::Command;
use predicates::str::contains;

#[test]
fn test_pl0_cli_eval_fact() {
    Command::cargo_bin("pl0")
        .unwrap()
        .args(&["-s", "examples/fact.pl0", "-m", "eval"])
        .write_stdin("5\n")
        .assert()
        .stdout(contains("120\n"))
        .success();
}

#[test]
fn test_pl0_cli_ir_fact() {
    Command::cargo_bin("pl0")
        .unwrap()
        .args(&["-s", "examples/fact.pl0", "-m", "ir"])
        .write_stdin("5\n")
        .assert()
        .stdout(contains("120\n"))
        .success();
}

#[test]
fn test_pl0_cli_eval_complicate() {
    let mut input = String::new();
    let mut output = String::new();

    // 5 * 10 = 50
    input += "5\n10\n";
    output += "50\n";
    // 10 / 3 = 3 * 3 + 1
    input += "10\n3\n";
    output += "3\n1\n";
    // (2048, 512) = 512
    input += "2048\n512\n";
    output += "512\n";
    // 10! = 3628800
    input += "10\n";
    output += "3628800\n";

    Command::cargo_bin("pl0")
        .unwrap()
        .args(&["-s", "examples/complicate.pl0", "-m", "eval"])
        .write_stdin(input)
        .assert()
        .stdout(contains(output))
        .success();
}

#[test]
fn test_pl0_cli_ir_complicate() {
    let mut input = String::new();
    let mut output = String::new();

    // 5 * 10 = 50
    input += "5\n10\n";
    output += "50\n";
    // 10 / 3 = 3 * 3 + 1
    input += "10\n3\n";
    output += "3\n1\n";
    // (2048, 512) = 512
    input += "2048\n512\n";
    output += "512\n";
    // 10! = 3628800
    input += "10\n";
    output += "3628800\n";

    Command::cargo_bin("pl0")
        .unwrap()
        .args(&["-s", "examples/complicate.pl0", "-m", "ir"])
        .write_stdin(input)
        .assert()
        .stdout(contains(output))
        .success();
}
