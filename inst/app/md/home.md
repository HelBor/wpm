# Welcome to Well Plate Maker!

WPM is a shiny application (web-based UI) allowing you to create a plan of
well-plate for your experiments by controlling __batch effects__.

Indeed, the placement of samples on a plate can raise questions when we want to
take into account batch effects (because a placement on a plate is a technical
source of variation), which may confound the discovery of real biological
variation. The question therefore arises mainly when the samples are divided
into different groups (here, we speak of a group to define a category of the
factor to be studied. Eg: different treatments to compare, different stages
of development, etc.)

The plate plan is built using a backtracking-inspired algorithm with some
specific spatial constraints which can be chosen by the user. The plate is
filled randomly (and not linearly), i.e. the plate is not filled from left to
right (or from top to bottom, etc.). This avoids having to end up with a
checkerboard plate plan when the numbers in the groups are unbalanced
(which would correspond to a form of batch effect).

There are 2 others panel:

* The __Parameters__ panel where to prepare your data for wpm.
* The __Results__ panel where you wil see your results.

A __vignette__ is also available in the package explaining how to use WPM in detail.
Simply enter `browseVignettes("wpm")` in the R console.
