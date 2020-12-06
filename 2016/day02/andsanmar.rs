use std::fs;

fn press_key_1(input : u8, command: char) -> u8 {
    match command {
        'U' => match input {
            1 | 2 | 3 => input,
            _ => input - 3
        },
        'R' => match input {
            3 | 6 | 9 => input,
            _ => input + 1
        },
        'D' => match input {
            7 | 8 | 9 => input,
            _ => input + 3
        },
        'L' => match input {
            1 | 4 | 7 => input,
            _ => input - 1
        },
        _ => 0
    }
}

fn press_key_2(input : char, command: char) -> char {
    match command {
        'U' => match input {
            '1' | '2' | '5' | '4' | '9' => input,
            '6' => '2', '7' => '3', '8' => '4', 'A' => '6', 'B' => '7', 'C' => '8', '3' => '1', 'D' => 'B',
            _ => panic!()
        },
        'R' => match input {
            '1' | '4' | '9' | 'C' | 'D' => input,
            '2' => '3', '3' => '4', '5' => '6', '6' => '7', '7' => '8', '8' => '9', 'A' => 'B', 'B' => 'C',
            _ => panic!()
        },
        'D' => match input {
            '5' | 'A' | 'D' | 'C' | '9' => input,
            '1' => '3', '2' => '6', '3' => '7', '4' => '8', '6' => 'A', '7' => 'B', '8' => 'C', 'B' => 'D',
            _ => panic!()
        },
        'L' => match input {
            '1' | '2' | '5' | 'A' | 'D' => input,
            '3' => '2', '4' => '3', '6' => '5', '7' => '6', '8' => '7', '9' => '8', 'B' => 'A', 'C' => 'B',
            _ => panic!()
        },
        _ => panic!()
    }
}


fn main() {
    let r = fs::read_to_string("input").unwrap();
    let star1 : Vec<u8> = r.lines().map(|l| l.chars().fold(5, |n, c| press_key_1(n, c))).collect();
    println!("{:?}", star1);
    let star2 : Vec<char> = r.lines().map(|l| l.chars().fold('5', |n, c| press_key_2(n, c))).collect();
    println!("{:?}", star2);
}
