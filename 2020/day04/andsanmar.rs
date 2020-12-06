use std::fs;

const FIELDS : [&str ; 7] = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]; // cid is optional

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let passports = r.split("\n\n").map( |x| x.split_whitespace().map(|f| {
        let v = f.split(":").collect::<Vec<_>>(); (v[0], v[1])
    }).collect());
    let complete_passports : Vec<Vec<(&str, &str)>> = passports.filter(|p : &Vec<(&str,&str)>| FIELDS.iter().all(|f| p.iter().any(|e| &e.0 == f))).collect();
    println!("{}", complete_passports.len());

    let valid_passports = complete_passports.iter().filter(|p| {
        p.iter().all ( |field| { match field.0 {
            "byr" => match field.1.parse::<u16>() { Ok(y) => y >= 1920 && y <= 2002, _ => false },
            "iyr" => match field.1.parse::<u16>() { Ok(y) => y >= 2010 && y <= 2020, _ => false },
            "eyr" => match field.1.parse::<u16>() { Ok(y) => y >= 2020 && y <= 2030, _ => false },
            "hgt" =>
                (field.1.ends_with("cm") && match field.1[..3].parse::<u8>() { Ok(h) => h >= 150 && h <= 193, _ => false }) ||
                (field.1.ends_with("in") && match field.1[..2].parse::<u8>() { Ok(h) => h >= 59 && h <= 76, _ => false }),
            "hcl" => field.1.len() == 7 && field.1.chars().nth(0).unwrap() == '#' && (1..7).all(|x| field.1.chars().nth(x).unwrap().is_ascii_hexdigit()),
            "ecl" => vec!["amb","blu","brn","gry","grn","hzl","oth"].contains(&field.1),
            "pid" => field.1.len() == 9 && (0..9).all(|x| field.1.chars().nth(x).unwrap().is_ascii_digit()),
            _ => true
        }})
    });
    println!("{:?}", valid_passports.count())
}
