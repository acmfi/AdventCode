// use std::fmt;

fn hex_to_bin(h : char) -> [bool;4] {
    match h {
        '0' => [false,false,false,false],
        '1' => [false,false,false,true],
        '2' => [false,false,true,false],
        '3' => [false,false,true,true],
        '4' => [false,true,false,false],
        '5' => [false,true,false,true],
        '6' => [false,true,true,false],
        '7' => [false,true,true,true],
        '8' => [true,false,false,false],
        '9' => [true,false,false,true],
        'A' => [true,false,true,false],
        'B' => [true,false,true,true],
        'C' => [true,true,false,false],
        'D' => [true,true,false,true],
        'E' => [true,true,true,false],
        'F' => [true,true,true,true],
        _ => panic!()
    }
}

fn bin_to_hex(acc : u64, n : &[bool]) -> u64 {
    match n {
        [] => acc,
        [false,..] => bin_to_hex(acc << 1, &n[1..]),
        [true,..] => bin_to_hex((acc << 1) | 1 , &n[1..])
    }
}

type Stream<'a> = &'a [bool];

#[derive(Debug)]
enum Operator {
    Sum,
    Multiply,
    Min,
    Max,
    Greater,
    Less,
    Equal
}

fn to_operator(n: u8) -> Operator {
    match n {
        0 => Operator::Sum,
        1 => Operator::Multiply,
        2 => Operator::Min,
        3 => Operator::Max,
        5 => Operator::Greater,
        6 => Operator::Less,
        7 => Operator::Equal,
        _ => panic!()
    }
}

#[derive(Debug)]
struct Packet {
    version : u8,
    value : Result<Vec<bool>,(Operator, Vec<Packet>)>
}

fn parse_input<'a>(s : &'a String) -> Vec<Vec<bool>> {
    s.lines().filter_map(|l| {
        match l.len() {
            0 => None,
            _ => Some(l.chars().map(|c| hex_to_bin(c)).flatten().collect())
        }
    }).collect()
}

fn decode_packet(base : Stream, offset : &mut usize) -> Packet {
    let v = &base[*offset..];
    let version = bin_to_hex(0, &v[0..3]) as u8;
    let operator = bin_to_hex(0, &v[3..6]) as u8;
    let value : Result<Vec<bool>,(Operator, Vec<Packet>)> = match operator {
        4 => {
            let mut num : Vec<bool> = Vec::new();
            let mut i : usize = 6;
            let mut keep_reading = true;
            while keep_reading {
                keep_reading = v[i];
                num.append(&mut v[i+1..i+5].to_vec());
                i += 5;
            }
            *offset += i;
            Ok(num)
        },
        _ => {
            let nest : Vec<Packet> = if v[6] {
                let mut packets = Vec::new();
                let num_packets = bin_to_hex(0, &v[7..18]);
                *offset += 18;
                for _ in 0..num_packets {
                    let p = decode_packet(base, offset);
                    packets.push(p);
                }
                packets
            } else {
                let mut packets = Vec::new();
                let length_packets = bin_to_hex(0, &v[7..22]);
                *offset += 22;
                let old_offset = *offset;
                while old_offset + length_packets as usize > *offset {
                    packets.push(decode_packet(base, offset));
                }
                packets
            };
            Err((to_operator(operator),nest))
        }
    };
    Packet { version: version, value: value }
}

impl Packet {
    fn sum_versions(&self) -> u64 {
        self.version as u64 + match &self.value {
            Ok(_) => 0,
            Err((_,n)) => {
                n.iter().map(|l| l.sum_versions()).sum()
            }
        }
    }

    fn get_value(&self) -> u64 {
        match &self.value {
            Ok(n) => bin_to_hex(0, &n),
            Err((operator, n)) => match operator {
                Operator::Sum => 
                    n.iter().map(|l| l.get_value()).sum(),
                Operator::Multiply => 
                    n.iter().map(|l| l.get_value()).product(),
                Operator::Min => 
                    n.iter().map(|l| l.get_value()).min().unwrap(),
                Operator::Max => 
                    n.iter().map(|l| l.get_value()).max().unwrap(),
                Operator::Greater => 
                    if &n[0].get_value() > &n[1].get_value() {1} else {0},
                Operator::Less => 
                    if &n[0].get_value() < &n[1].get_value() {1} else {0},
                Operator::Equal => 
                    if &n[0].get_value() == &n[1].get_value() {1} else {0},
            }
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: Vec<Vec<bool>> = parse_input(&filename);
    // star1(&s);
    // let new_map : Vec<Vec<u16>> = new_map(s);
    for l in &s {
        let p = &decode_packet(l.as_slice(), &mut 0);
        println!("{:?}", p.sum_versions());
        println!("{:?}", p.get_value());
    }
    // star1(&new_map);
}
