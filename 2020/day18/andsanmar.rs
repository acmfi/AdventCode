use std::fs;

// Returns tuple (result, index)
fn solve_operation_1(mut i : usize, operation : &str) -> (usize, usize) {
    let mut current_result : usize = 0;
    let mut current_operation : Option<char> = None;
    while i < operation.len() {
        let c = operation.chars().nth(i).unwrap();
        match c {
            ' ' => {},
            '0'..='9' => {
                let digit = c.to_digit(10).unwrap();
                match current_operation {
                    None => current_result = digit as usize,
                    Some('+') => current_result += digit as usize,
                    Some('*') => current_result *= digit as usize,
                    _ => panic!()
            }}
            '+' | '*' => current_operation = Some(c),
            '(' => {
                let op = solve_operation_1(i+1, operation);
                i = op.1;
                match current_operation {
                    None => current_result = op.0,
                    Some('+') => current_result += op.0 as usize,
                    Some('*') => current_result *= op.0 as usize,
                    _ => panic!()
                }
            },
            ')' => return (current_result,i),
            _ => panic!()
        }
        i+=1;
    }
    (current_result, operation.len())
}

fn solve_operation_2(mut i : usize, operation : &str) -> (usize, usize) {
    let mut current_operation : Option<char> = None;
    let mut current_result = 0; // The values currently added (because precedence)
    let mut elems : Vec<usize> = Vec::new(); // Values already added and left to multiply
    while i < operation.len() {
        let c = operation.chars().nth(i).unwrap();
        match c {
            ' ' => {},
            '0'..='9' => {
                let num = c.to_digit(10).unwrap();
                match current_operation {
                    None => current_result = num as usize,
                    Some('+') => current_result += num as usize,
                    Some('*') => {
                        elems.push(current_result as usize);
                        current_result = num as usize;
                    },
                    _ => panic!()
                }
            },
            '+' | '*' => current_operation = Some(c),
            '(' => {
                let op = solve_operation_2(i+1, operation);
                let num = op.0; i = op.1;
                match current_operation {
                    None => current_result = num as usize,
                    Some('+') => current_result += num as usize,
                    Some('*') => {
                        elems.push(current_result as usize);
                        current_result = num as usize;
                    },
                _ => panic!()
                }
            },
            ')' => {
                elems.push(current_result);
                return (elems.iter().product::<usize>(),i)},
            _ => panic!()
        }
        i+=1;
    }
    elems.push(current_result);
    (elems.iter().product::<usize>(), operation.len())
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let operations : Vec<(usize,usize)> = r.lines().map(|l| {
        (solve_operation_1(0,l).0, solve_operation_2(0, l).0)
    }).collect();
    println!("{:?}", operations.iter().map(|x| x.0).sum::<usize>());
    println!("{:?}", operations.iter().map(|x| x.1).sum::<usize>());
}
