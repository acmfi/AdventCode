use std::fs;

fn trees_found(forest : &Vec<&str>, down : usize, right : usize) -> usize {
    forest.iter().enumerate().filter(
        |(num,line)| { num % down == 0 && {
            match line.chars().nth((num/down)*right % (line.len())) {
                Some('#') => true,
                _ => false
            }
        }}).count()
}

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let forest : Vec<&str> = r.lines().collect();
    println!("{}", trees_found(&forest, 1, 3));
    println!("{}", trees_found(&forest, 1, 1) * trees_found(&forest, 1, 3) * trees_found(&forest, 1, 5) * trees_found(&forest, 1, 7) * trees_found(&forest, 2, 1));
}
