use std::fs;

// Data structure: position i indicates node with label i+1, data indicates the next
fn move_cups(cups: &mut [usize], current : usize, dimension : usize) -> (&mut [usize], usize) {
    let e1 = cups[current];
    let e2 = cups[e1];
    let e3 = cups[e2];
    let inbounds = |x: usize, y : usize| {let a = x - y; if a > dimension {a+dimension-1} else {a}};
    let not_seq = |x : usize| x != e1 && x != e2 && x != e3;
    let next = cups[e3];

    let to_place : usize = {
        let prev : [usize;4] = [inbounds(current,1),inbounds(current,2),inbounds(current,3),inbounds(current,4)];
        if not_seq(prev[0]) {prev[0]} else
            if not_seq(prev[1]) {prev[1]} else
            if not_seq(prev[2]) {prev[2]} else {prev[3]}};

    let jump = cups[to_place];
    cups[to_place] = e1;
    cups[e3] = jump;
    cups[current] = next;
    
    (cups, next)
}

fn main () {
    let cups : Vec<usize> = fs::read_to_string("input").unwrap().chars().map(|e| e.to_digit(10).unwrap() as usize).collect::<Vec<usize>>();
    let start = cups[0]-1;
    // Construction of (src,dst)
    let mut cupstructure : Vec<(&usize,&usize)> = cups.iter().zip(&cups[1..]).collect();
    cupstructure.push((&cups[8], &cups[0]));
    // We map down the values to a vector where each location points to the correspondent node
    let mut n : Vec<usize> = (0..cups.len()).map(|i| *cupstructure.iter().find(|(src,_dst)| *src-1 == i).unwrap().1-1).collect();
    let mut n2 = n.clone();
    let mut star1 : &mut [usize] = &mut n;
    let c1 = (0..100).fold((star1, start), |(v,current),_| move_cups(v,current,10));
    star1 = c1.0; 
    let mut current_index = star1[0];
    while current_index != 0 {
        print!("{}", current_index+1);
        current_index = star1[current_index];
    }
    println!();

    let dimension = 1_000_000;
    for x in cups.len()..dimension {
        n2.push(x+1);
    }
    n2[cups[8]-1] = cups.len();
    n2[dimension-1] = *cups.first().unwrap()-1;
    let star2 : &mut [usize] = &mut n2;
    let c2 = (0..10_000_000).fold((star2, start), |(v,current),_| move_cups(v,current,dimension+1));
    let n1 = c2.0[0];
    let n2 = c2.0[n1];
    println!("{}", (n1+1)*(n2+1));
}
