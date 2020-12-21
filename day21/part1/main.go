package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/pdbogen/aoc20/set"
)

type Recipe struct {
	Ingredients set.Set
	Allergens   set.Set
}

func ParseRecipe(line string) Recipe {
	var il = set.Set{}
	var al = set.Set{}
	if paren := strings.Index(line, "("); paren != -1 {
		for _, allergen := range strings.Split(line[paren+10:len(line)-1], ",") {
			al[strings.TrimSpace(allergen)] = true
		}
		line = strings.TrimSpace(line[:paren])
	}

	for _, i := range strings.Split(line, " ") {
		il[strings.TrimSpace(i)] = true
	}

	return Recipe{Ingredients: il, Allergens: al}
}

func Allergens(recipes []Recipe) set.Set {
	a := set.Set{}
	for _, recipe := range recipes {
		a = a.Union(recipe.Allergens)
	}
	return a
}

func Ingredients(recipes []Recipe, allergen string) set.Set {
	var ingredients set.Set
	for _, recipe := range recipes {
		if !recipe.Allergens[allergen] {
			continue
		}
		if ingredients == nil {
			ingredients = recipe.Ingredients
		} else {
			ingredients = ingredients.Intersect(recipe.Ingredients)
		}
	}
	return ingredients
}

func main() {
	input, err := ioutil.ReadFile("day21.input")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	var recipes []Recipe
	for _, line := range lines {
		recipes = append(recipes, ParseRecipe(line))
	}

	triggers := map[string]string{}
	allergens := Allergens(recipes)
	known := set.Set{}

	for len(allergens) > 0 {
		for allergen := range allergens {
			ingredients := Ingredients(recipes, allergen)
			ingredients = ingredients.Difference(known)
			if len(ingredients) == 0 {
				panic("overconstrained on " + allergen)
			}
			if len(ingredients) != 1 {
				continue
			}
			known = known.Union(ingredients)
			triggers[allergen] = ingredients.Item()
			delete(allergens, allergen)
			break
		}
	}

	var safe = 0
	for _, recipe := range recipes {
		for ingredient := range recipe.Ingredients {
			if known[ingredient] {
				continue
			}
			safe++
		}
	}
	fmt.Println("Part 1:", safe)

	fmt.Println(Ingredients(recipes, "soy"))
	fmt.Println(triggers)
	var unsafe []string
	for _, allergen := range Allergens(recipes).Items() {
		unsafe = append(unsafe, triggers[allergen])
	}
	fmt.Println("Part 2:", strings.Join(unsafe, ","))
}
