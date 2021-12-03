#![feature(drain_filter)]

fn bti(b:bool) -> i16 {if b {1} else {-1}}

fn star1(l : &Vec<Vec<bool>>) {
    let gamma : Vec<char> = l.iter().map(|e| {
        if e.iter().map(|b| bti(*b)).sum::<i16>() >= 0 {'1'} else {'0'}
    }).collect();
    let epsilon : Vec<char> = gamma.iter().map(|b| if *b == '1' {'0'} else {'1'}).collect();
    let g = usize::from_str_radix(&gamma.into_iter().collect::<String>(), 2).unwrap();
    let e = usize::from_str_radix(&epsilon.into_iter().collect::<String>(), 2).unwrap();
    println!("{}", g*e);
}

fn get_common(l : &Vec<Vec<bool>>, sign: fn(i16) -> bool) -> usize {
    let mut p : Vec<Vec<bool>> = l.clone();
    for i in 0..l.len() {
        if p.len() == 1 {break}
        let common_bit = sign(p.iter().map(|e| bti(e[i])).sum::<i16>());
        p = p.drain_filter(|e| e[i] == common_bit).collect();
    }
    usize::from_str_radix(&p[0].iter().map(|b| if *b {'1'} else {'0'}).collect::<String>(), 2).unwrap()
}

fn star2(l : &Vec<Vec<bool>>) {
    let oxygen = get_common(l, |x:i16| x>=0);
    let co2 = get_common(l, |x:i16| x<0);
    println!("{}", oxygen*co2);
}

fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Vec<Vec<bool>> = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if l.len() > 0 {Some(l.chars().map(|x| x == '1').collect())} else {None}
    }).collect();
    star1(&transpose(l.clone()));
    star2(&l);
}
