use std::fs;
use crypto::digest::Digest;

fn get_pass(prefix : &str) -> (String, String) {
    let mut pass : Vec<char> = vec![];
    let mut pass2 : Vec<char> = vec![' '; 8];
    let mut sh = crypto::md5::Md5::new();
    for index in 0.. {
        sh.input_str(prefix);
        sh.input(index.to_string().into_bytes().as_slice());
        let m = sh.result_str();
        if &m[..5] == "00000" {
            pass.push(m.chars().nth(5).unwrap());
            match m.chars().nth(5).unwrap().to_digit(8) {
                Some(p) => {
                    let pos = p as usize;
                    let letter = m.chars().nth(6).unwrap();
                    if pass2[pos] == ' ' {
                        pass2[pos] = letter;
                        if pass2.iter().all(|x| *x != ' ') { break }
                    }
                }
                _ => {}
            }
        }
        sh.reset();
    }
    (pass.into_iter().take(8).collect(), pass2.into_iter().collect())
}

fn main() {
    let prefix = fs::read_to_string("input").unwrap();
    let (pass1, pass2) = get_pass(&prefix);
    println!("{}", pass1);
    println!("{}", pass2);
}
