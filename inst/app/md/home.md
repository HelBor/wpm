# Welcome to Well Plate Maker!

WPM is a shiny application (web-based UI) allowing you to create a map of
well-plate for your experiments by controlling __batch effects__.

Indeed, the placement of samples on a plate can raise questions when we want to
take into account batch effects (because a placement on a plate is a technical
source of variation), which may confound the discovery of real biological
variation. The question therefore arises mainly when the samples are divided
into different groups (here, we speak of a group to define a category of the
factor to be studied. Eg: different treatments to compare, different stages
of development, etc.)

The plate map is built using a __backtracking-inspired algorithm__ with some
specific __spatial constraints__ which can be chosen by the user. The plate is
filled randomly (and not linearly), i.e. the plate is not filled from left to
right (or from top to bottom, etc.). This avoids having to end up with a
checkerboard plate map when the numbers in the groups are unbalanced
(which would correspond to a form of batch effect).

There are 2 other tabs:

* The __Parameters__ tab where to prepare your data for wpm.
* The __Results__ tab where you will see your results.

A __tutorial__ is also available, explaining how to use WPM in detail.
Simply enter `browseVignettes("wpm")` in the R console or see below in the 
current tab.
