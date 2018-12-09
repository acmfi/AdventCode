let fs = require('fs');

// JS sucks, no filter for dicts
let filter_d = (dict, f) => {
    return Object.keys(dict).reduce((filtered, key) => {
        if (f([key, dict[key]])) filtered[key] = dict[key];
        return filtered;
    }, {});
};

// Steps logic
let available = steps => filter_d(steps, ([k, v]) => v.needs == 0);

// Steps constructors
let new_step = () => ({needs: 0, childs: []});
let add_child = (step, name) => ({...step, childs: [...step.childs, name]} );
let add_parent = (step) => ({...step, needs: step.needs + 1});
let remove_parent = (step) => ({...step, needs: step.needs - 1});
let add = (steps, parent, child) => {
    let oparent = steps[parent] || new_step();
    let ochild = steps[child] || new_step();
    let parent_s = add_child(oparent, child);
    let child_s = add_parent(ochild);
    return {...steps, [parent]: parent_s, [child]: child_s};
};
let kill_parent = (steps, name) => {
    let childs = steps[name].childs;
    return childs.reduce((acc, child) => ({...acc, [child]: remove_parent(acc[child])}), steps);
};

function find_path(steps, consumed = []) {
    if (Object.keys(steps).length == 0) {
        return consumed;
    } else {
        let next = Object.keys(available(steps)).sort()[0];
        let next_steps = kill_parent(steps, next);
        delete next_steps[next];
        return find_path(next_steps, [...consumed, next]);
    }
};

function star1(steps) {
    return find_path(steps).join("");
};

let fname = 'input';
let contents = fs.readFileSync(fname, 'utf8') .split('\n').filter(Boolean).map(x => [x.split(" ")[1], x.split(" ")[7]]);
let steps = contents.reduce((acc, v) => add(acc, v[0], v[1]), {});

console.log(star1(steps));
