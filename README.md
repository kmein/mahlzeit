# Mahlzeit!
Mahlzeit [ˈmaːlˌt͡saɪ̯t] helps you manage your favourite recipes.

> [!TIP]
> This project is unmaintained. I think you should give [Cooklang](https://cooklang.org/) a try.

## Usage

To use Mahlzeit, you have to set the `RECIPE_HOME` variable in the environment where you run it.

```bash
$ export RECIPE_HOME=path/to/your/recipe/folder
```

### Searching your Recipe Database
Mahlzeit allows you to search through your recipes by tags, ingredients and titles. The following, for example, looks for all vegan Asian meals that mention peanut butter in their ingredients.

```bash
$ mahlzeit list --ingredient "peanut butter" --tag asian --tag vegan
[satay-noodles] Vegan Satay Noodles *vegan, Asian*
[fried-rice] Easy Vegan Fried Rice *vegan, Asian*
```

### Calculating the Right Amounts
Once you have found the perfect recipe for you, just provide Mahlzeit with number of servings you want. It will take care of the calculations.

```bash
$ mahlzeit show fried-rice 4
... RECIPE ...
```

*Note*: The output is valid [Pandoc markdown](https://pandoc.org/MANUAL.html#pandocs-markdown), so you can easily pipe it into `pandoc -o recipe.pdf` to generate a good-looking PDF of your recipe.

### Editing a Recipe
In case you want to edit a recipe, just run:

```
$ mahlzeit edit fried-rice
```

This should open your favourite editor (the `EDITOR` environment variable) with the recipe file you specified.

### Importing from Meal-Master
Many [recipes](http://www.garvick.com/recipes/meal-master.htm) [online](http://www.ffts.com/recipes.htm) still use a format
that was first used by a DOS program called Meal-Master, or MM.EXE. Mahlzeit supports importing from this format. To import, run

```bash
$ mahlzeit import <path/to/your/recipe.mm>
```

If you find the parser having problems with a valid Meal-Master file, open an [issue](https://github.com/kmein/mahlzeit/issues/new).

## Mahlzeit's Recipe Format
For examples, see the [examples](./examples) directory.
