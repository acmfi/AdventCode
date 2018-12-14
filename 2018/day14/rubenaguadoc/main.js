const input = 760221;
// const input = 59414;

let recipes = [3, 7];
let elf1 = 0;
let elf2 = 1;
let i = 0;
let response = '';

while (true) {
  let newRecipes = (recipes[elf1] + recipes[elf2]).toString().split('');
  newRecipes.forEach(recipe => recipes.push(parseInt(recipe)));
  elf1 = (elf1 + recipes[elf1] + 1) % recipes.length;
  elf2 = (elf2 + recipes[elf2] + 1) % recipes.length;

  if (i >= input && i <= input + 10) {
    response += recipes[i];
    if (response.length == 10) console.log(`1: ${response}`);
  }

  if (i % 10000000 == 0) {
    let match = new RegExp(input.toString()).exec(recipes.join(''));
    if (match) {
      console.log(`2: ${match.index}`);
      break;
    }
  }
  i++;
}
