use std::fs;

#[derive(Debug)]
enum Ins { Nop(i16), Acc(i16), Jmp(i16) }

fn inf_loop(instructions : &Vec<Ins>, index : usize, acc : i32, executed : &mut Vec<usize>) -> (i32, bool) {
    if index == instructions.len() { (acc, true) } else 
        if executed.contains(&index) { (acc, false) } else {
            executed.push(index);
            match instructions[index] {
                Ins::Nop(_) => inf_loop(instructions, index+1, acc, executed),
                Ins::Jmp(n) => inf_loop(instructions, index+n as usize, acc, executed),
                Ins::Acc(n) => inf_loop(instructions, index+1, acc+n as i32, executed)
            }
        }
}

// executed will be modified in the inner executions, but it doesn't matter because when braking the loop a never reached instruction will be reached
fn solve_loop(instructions : &Vec<Ins>, index : usize, acc : i32, executed : &mut Vec<usize>) -> i32 {
    match instructions[index] {
        Ins::Nop(n) => match inf_loop(instructions, index+n as usize, acc, executed) {
            (acc, true) => acc,
            _ => solve_loop(instructions, index+1, acc, executed)
        },
        Ins::Jmp(n) => match inf_loop(instructions, index+1, acc, executed) {
            (acc, true) => acc,
            _ => solve_loop(instructions, index+n as usize, acc, executed)
        },
        Ins::Acc(n) => solve_loop(instructions, index+1, acc+n as i32, executed),
    }
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let instructions : Vec<Ins> = r.lines().map(|l| {
        let mut i = l.split_whitespace();
        let ins = match i.next().unwrap() {
            "nop" => Ins::Nop,
            "acc" => Ins::Acc,
            "jmp" => Ins::Jmp,
            _ => panic!()
        };
        ins(i.next().unwrap().parse::<i16>().unwrap())
    }).collect();
    let mut executed : Vec<usize> = Vec::new();
    let star1 = inf_loop(&instructions, 0, 0, &mut executed);
    println!("{:?}", star1);
    let star2 = solve_loop(&instructions, 0, 0, &mut executed);
    println!("{:?}", star2);    
}
