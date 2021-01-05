use std::cmp;
use std::fs::read_to_string;

struct MemSpace {
    mask: Vec<char>,
    memory: Vec<u64>,
}

enum Instruction {
    Mask(Vec<char>),
    Write(usize, u64),
}

impl MemSpace {
    fn new(memsize: usize) -> MemSpace {
        MemSpace {
            mask: vec!['X'; 36],
            memory: vec![0; memsize],
        }
    }

    fn execute_instr(&mut self, instr: Instruction) {
        match instr {
            Instruction::Mask(new_mask) => self.change_mask(new_mask),
            Instruction::Write(addr, value) => self.write_to_address(addr, value),
        }
    }

    fn change_mask(&mut self, new_mask: Vec<char>) {
        self.mask = new_mask;
    }

    // mask = 11100XX0000X1101X1010100X1010001XX0X
    fn write_to_address(&mut self, addr: usize, value: u64) {
        let mut value_after_mask = value;
        for i in 0..36 {
            match self.mask[i] {
                '0' => {
                    value_after_mask &= !(0x1 << (35 - i));
                }
                '1' => {
                    value_after_mask |= 0x1 << (35 - i);
                }
                'X' => (),
                _ => panic!("Invalid mask: {:?}", self.mask),
            }
        }
        self.memory[addr] = value_after_mask;
    }

    fn sum_memory_values(&self) -> u64 {
        self.memory.iter().sum()
    }
}

fn parse_instruction(line: &str) -> Instruction {
    let mut iterator = line.split(" = ");
    let instr_kind = iterator.next().expect("The program provided was invalid!");
    let instr_value = iterator.next().expect("The program provided was invalid!");
    match instr_kind {
        "mask" => Instruction::Mask(instr_value.chars().collect()),
        _ => Instruction::Write(
            instr_kind[4..]
                .chars()
                .take_while(|s| s.is_digit(10))
                .collect::<String>()
                .parse::<usize>()
                .expect("The address provided was invalid!"),
            instr_value
                .parse::<u64>()
                .expect("The value to write provided was invalid!"),
        ),
    }
}

fn find_memory_capacity(program: &Vec<&str>) -> usize {
    let mut max_addr: usize = 0;
    for instr in program {
        let instr_addr = match parse_instruction(instr) {
            Instruction::Mask(_) => 0,
            Instruction::Write(addr, _) => addr,
        };
        max_addr = cmp::max(instr_addr, max_addr);
    }
    max_addr + 1
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.lines().collect();
    let mut memspace = MemSpace::new(find_memory_capacity(&lines));
    for instr in lines {
	memspace.execute_instr(parse_instruction(instr));
    }
    let sum_of_values = memspace.sum_memory_values();
    println!("*** 1st star ***");
    println!("{}", sum_of_values);

    println!("*** 2nd star ***");
}
