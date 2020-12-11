use std::fs;

fn ways_to_connect(current_power : usize, connectors : &[usize]) -> usize {
    if connectors.len() <= 1 { return 1 }
    let mut r = ways_to_connect(connectors[0],&connectors[1..]);
    if connectors.len() >= 2 && connectors[1] <= current_power+3 {
        r += ways_to_connect(connectors[1],&connectors[2..]) }
    if connectors.len() >= 3 && connectors[2] <= current_power+3 {
        r += ways_to_connect(connectors[2],&connectors[3..]) }
    r
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut adapters : Vec<usize> = r.lines().map(|l| l.parse::<usize>().unwrap()).collect();
    adapters.push(0); // Initial
    adapters.sort();
    let diffs : Vec<usize> = adapters.iter().zip(adapters[1..].iter()).map(|(a,b)| *b-a).collect();
    let diffs1 = diffs.iter().filter(|x| *x==&1).count();
    let diffs3 = diffs.iter().filter(|x| *x==&3).count() + 1; // Device differs of 3
    println!("{:?}", diffs1*diffs3);
    // when there are differences of 3, the list can be splitted and we can multiply the ways to sort the 2 parts
    let sl = adapters.iter().zip(diffs).collect::<Vec<(&usize,usize)>>().split(|(_,d)| d == &3).map(|x| match x.last() {
        Some(last) => {
            let mut v = x.to_vec().iter().map(|y| *y.0).collect::<Vec<usize>>();
            v.push(last.0+last.1);
            ways_to_connect(v[0],&v[1..])
        }
        None => 1
    }).collect::<Vec<usize>>();
    println!("{:?}", sl.iter().product::<usize>());
    // This method is too slow: println!("{:?}", ways_to_connect(adapters[0], &adapters[1..]));
}
