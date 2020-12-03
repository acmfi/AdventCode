use std::collections::HashMap;

fn star1(mod_cs: HashMap, ) {
    
}

fn main() {
    let mut mod_classes = HashMap::new();
    
    for i in 0..10 {
        let v: Vec<i32> = vec![];
        mod_classes.insert(
            i, v
        );
    }

    for (key, val) in mod_classes.iter() {
        println!("{}:{}", key, val.len());
    }
}
