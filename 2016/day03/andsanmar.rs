use std::fs;

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let triangles_1 = r.lines().map(|l| {
        let mut t : Vec<u16> = l.split_whitespace().map(|n| n.parse::<u16>().unwrap()).collect();
        t.sort();
        (t[0], t[1], t[2])
    });
    println!("{}", triangles_1.filter(|(a,b,c)| c < &(a+b)).count());
    let t = r.lines().collect::<Vec<&str>>();
    let triangles_2 = t.chunks(3).map(|l| {
        let t : Vec<Vec<u16>> = l.iter().map(|x| x.split_whitespace().map(|n| n.parse::<u16>().unwrap()).collect()).collect();
        let mut t1 = vec![t[0][0], t[1][0], t[2][0]]; t1.sort();
        let mut t2 = vec![t[0][1], t[1][1], t[2][1]]; t2.sort();
        let mut t3 = vec![t[0][2], t[1][2], t[2][2]]; t3.sort();
        vec![(t1[0],t1[1],t1[2]),(t2[0],t2[1],t2[2]),(t3[0],t3[1],t3[2])]
    }).flatten(); //.collect::<Vec<(u16,u16,u16)>>();
    println!("{:?}", triangles_2.filter(|(a,b,c)| c < &(a+b)).count());
}
