use std::fs;

fn game1(mut player1 : Vec<usize>, mut player2 : Vec<usize>) -> (bool,Vec<usize>) {
    while !player1.is_empty() && !player2.is_empty() {
        let a = player1[0]; player1.remove(0);
        let b = player2[0]; player2.remove(0);
        if a > b { player1.push(a); player1.push(b); }
        else { player2.push(b); player2.push(a); }
    }
    let mut winner = if player1.is_empty() {(false,player2)} else {(true,player1)};
    winner.1.reverse();
    winner
}

fn game2(mut player1 : Vec<usize>, mut player2 : Vec<usize>) -> (bool,Vec<usize>) {
    let mut played : Vec<(Vec<usize>,Vec<usize>)> = Vec::new();
    while !player1.is_empty() && !player2.is_empty() {
        if played.contains(&(player1.clone(),player2.clone())) { return (true,Vec::new()) }
        else {played.push((player1.clone(),player2.clone()))} // TODO avoid clones
        let a = player1[0]; player1.remove(0);
        let b = player2[0]; player2.remove(0);
        let who_wins = if (player1.len() >= a) && (player2.len() >= b) {
            game2(player1[0..a].to_vec(), player2[0..b].to_vec()).0
        } else { a > b };
        if who_wins { player1.push(a); player1.push(b); }
        else { player2.push(b); player2.push(a); }
    }
    let mut winner = if player1.is_empty() {(false,player2)} else {(true,player1)};
    winner.1.reverse();
    winner
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut players = r.split("\n\n");
    let player1 : Vec<usize> = players.next().unwrap().lines().skip(1).map(|x| x.parse::<usize>().unwrap()).collect();
    let player2 : Vec<usize> = players.next().unwrap().lines().skip(1).map(|x| x.parse::<usize>().unwrap()).collect();
    println!("{:?}", game1(player1.clone(), player2.clone()).1.iter().enumerate().map(|(i,x)| x*(i+1)).sum::<usize>());
    println!("{:?}", game2(player1, player2).1.iter().enumerate().map(|(i,x)| x*(i+1)).sum::<usize>());
}
