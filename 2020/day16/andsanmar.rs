use std::fs;

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut it = r.split("\n\n");
    let constraints : Vec<(&str,Vec<(usize,usize)>)> = it.next().unwrap().lines().map(|l| {
        let mut v = l.split(':');
        ( v.next().unwrap(), // Field name
          v.next().unwrap().split("or").map(|nums| {
            let ns : Vec<usize> = nums.split('-').map(|x| x.trim().parse().unwrap()).collect();
            (ns[0],ns[1])
        }).collect())
    }).collect();
    let own_ticket : Vec<usize> = it.next().unwrap().lines().collect::<Vec<&str>>()[1].split(',').map(|x| x.parse::<usize>().unwrap()).collect();
    let nearby_tickets : Vec<Vec<usize>> = it.next().unwrap().lines().collect::<Vec<&str>>()[1..].iter().map(|l| l.split(',').map(|x| x.parse::<usize>().unwrap()).collect()).collect();
    let invalid_fields = nearby_tickets.iter().map(|t| t.iter().filter(|n| !constraints.iter().map(|x| &x.1).flatten().any(|(x,y)| *n >= &x && *n <= &y)));
    println!("{:?}", invalid_fields.flatten().sum::<usize>());
    let valid_tickets : Vec<&Vec<usize>> = nearby_tickets.iter().filter(|t| t.iter().all(|n| constraints.iter().map(|x| &x.1).flatten().any(|(x,y)| n >= x && n <= y))).collect();
    let mut constrained : Vec<(usize,Vec<&&str>)> = (0..valid_tickets.first().unwrap().len()).map(|index| {
        let fields = valid_tickets.iter().map(|t| t[index]).collect::<Vec<usize>>();
        (index, constraints.iter().filter(
            |(_name,req)| fields.iter().all(|f| req.iter().any(|(x,y)| f >= x && f <= y)))
            .map(|(name,_)| name).collect())
    }).collect();
    constrained.sort_by(|(_i1,c1),(_i2,c2)| c1.len().partial_cmp(&c2.len()).unwrap());
    let constrained_unique = constrained.iter().enumerate().map(|(i,x)| (x.0, if i == 0 {x.1[0]} else {x.1.iter().filter(|f| !constrained[i-1].1.contains(f)).next().unwrap()})); // Obtain unique elements
    println!("{:?}", constrained_unique.filter(|(_,v)| v.contains("departure")).map(|(i,_)| own_ticket[i]).product::<usize>());
}
