# To render tasks

# turn solutions off



quarto render tasks_part1.qmd -o tasks_part1.html
quarto render tasks_part2.qmd -o tasks_part2.html

# turn solutions on
quarto render tasks_part1.qmd -o tasks_part1_solutions.html
quarto render tasks_part2.qmd -o tasks_part2_solutions.html

# turn on and of rendering of solutions link in style.scss

.solution-link {
  display: none;  // Hide the solution link
}
