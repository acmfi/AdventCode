use std::fs::read_to_string;

#[derive(Clone)]
enum Instruction {
    Acc(isize),
    Jmp(isize),
    Nop(isize),
}

struct Console {
    acc: isize,
    pc: isize,
}

type Program = Vec<Instruction>;

impl Console {
    fn reset(&mut self) {
	self.acc = 0;
	self.pc = 0;
    }
    
    fn execute_instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Acc(amt) => self.acc += amt,
            Instruction::Jmp(amt) => self.pc += amt - 1,
            Instruction::Nop(_) => (),
        }
        self.pc += 1;
    }

    // Executes a given program until a program wants to execute
    // and instruction for a second time. If it returns None,
    // the program wasn't able to fully execute properly (due to a loop);
    // if it returns (), then the program terminated with success
    fn execute_until_loop(&mut self, program: &Program) -> Option<()> {
        let mut already_executed: Vec<isize> = Vec::new();
        while !already_executed.iter().any(|&i| i == self.pc) {
            already_executed.push(self.pc);
            let next_instr = program.get(self.pc as usize);
            match next_instr {
                Some(instr) => self.execute_instruction(instr),
                None => return Some(()),
            }
        }
        return None;
    }
}

fn parse_instruction(line: &str) -> Instruction {
    let mut iterator = line.split(" ");
    let instr_kind = iterator.next().expect("The program provided was invalid!");
    let instr_value: isize = iterator
        .next()
        .expect("The program provided was invalid!")
        .parse()
        .expect("Couldn't parse the value of the instruction");
    match instr_kind {
        "acc" => Instruction::Acc(instr_value),
        "jmp" => Instruction::Jmp(instr_value),
        "nop" => Instruction::Nop(instr_value),
        _ => panic!("The program provided was invalid!"),
    }
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let mut program: Program = input.lines().map(|line| parse_instruction(line)).collect();
    let mut console = Console { acc: 0, pc: 0 };
    console.execute_until_loop(&program);
    println!("*** 1st star ***");
    println!("{}", console.acc);

    for i in 0..program.len() {
	let prev_instr = program[i].clone();
        match program[i] {
            Instruction::Jmp(value) => program[i] = Instruction::Nop(value),
            Instruction::Nop(value) => program[i] = Instruction::Jmp(value),
            _ => (),
        }
	console.reset();
	if console.execute_until_loop(&program) == Some(()) { break; }
	program[i] = prev_instr;
    }

    println!("*** 2nd star ***");
    println!("{}", console.acc);
}
