# maze-gen-text

A maze generator that supports text snakes and outputs tikz code.

I used this for creating birthday invitations for my child.

## Features

 - generates rectangular mazes or mazes with arbitrary shape from a b/w png file
 - can embed text strings into the mazes. The embedded text strings have no forks and are surrounded by unreachable mazes
 - output is LaTeX TiKZ code, if `standalone` is selected the output file is compilable and will be compiled to a pdf using `latexmk`

## Example

If you ahve the following image as `giraffe.png`:

![blocky black-and-white giraffe](src/test/resources/giraffe.png)

